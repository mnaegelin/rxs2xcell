#' Get list of input files
#'
#' Given a path_in, get the paths and filenames of the (ROXAS) input files to be
#'  processed.
#'
#' @param path_in path of the input directory.
#'
#' @returns A list of lists containing filepaths of images and ROXAS output
#'          files (cells, rings, settings).
#' @export
get_input_files <- function(path_in) {

  # Regex patterns to be matched by the different ROXAS files
  # NOTE: in addition to the original images (IMGNAME.jpg), the ROXAS output
  # might include annotated images, etc. These are filtered out with keywords.
  pattern_cell_files = "_Output_Cells\\.txt$"
  pattern_ring_files = "_Output_Rings\\.txt$"
  pattern_settings_files = "_ROXAS_Settings\\.txt$"
  pattern_orgimg_files = "\\.(jpg|jpeg)$" # "\\.(jpg|jpeg|png|gif|bmp|tiff)$"
  imgfiles_exclude_keywords = c("annotated",
                                "ReferenceSeries",
                                "Preview")

  # assert that path_in is valid
  beepr::beep_on_error(
    checkmate::assert_directory_exists(path_in),
    sound=2
  )

  # list ROXAS output files: cells, rings, and settings
  files_cells <- list.files(path_in,
                            pattern = pattern_cell_files,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = TRUE, include.dirs = TRUE
                            )
  files_rings <- list.files(path_in,
                            pattern = pattern_ring_files,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = TRUE, include.dirs = TRUE
                            )
  files_settings <- list.files(path_in,
                               pattern = pattern_settings_files,
                               full.names = TRUE, recursive = TRUE,
                               ignore.case = TRUE, include.dirs = TRUE
                               )

  # list original images used for ROXAS analysis
  pattern_excl_keywords = paste(imgfiles_exclude_keywords, collapse="|")
  files_images <- list.files(path_in,
                             pattern = pattern_orgimg_files,
                             full.names = TRUE, recursive = TRUE,
                             ignore.case = TRUE, include.dirs = TRUE
                             ) %>%
    .[!stringr::str_detect(., pattern_excl_keywords)]

  # check that we did find some valid files
  l_files <- list(
    sub(pattern_cell_files, "", files_cells, ignore.case = TRUE),
    sub(pattern_ring_files, "", files_rings, ignore.case = TRUE),
    sub(pattern_settings_files, "", files_settings, ignore.case = TRUE),
    sub(pattern_orgimg_files, "", files_images, ignore.case = TRUE)
  )
  all_fnames <- Reduce(union, l_files)

  # stop if there are no ROXAS files at all
  if (length(all_fnames) == 0) {
    beepr::beep(sound = 2, expr = NULL)
    stop(
      "NO ROXAS output files found under path", "\n",
      paste0("  ", path_in)
    )
  }

  # identify any mismatches between ring, cell, settings, and image files
  dontmatch <- setdiff(
    all_fnames,
    Reduce(intersect, l_files)
  )

  # stop if there are any mismatches
  if (length(dontmatch) > 0) {
    beepr::beep(sound = 2, expr = NULL)
    stop(
      "ROXAS output files DON'T MATCH for:", "\n",
      paste(paste0("  ", dontmatch), collapse = "\n")
    )
  }

  # report the number of images and beep successful ending of the function
  message(paste("Found", length(files_images),
                "images and associated ROXAS files under the given path."))
  beepr::beep(sound = 1, expr = NULL)

  # return all the file paths we need as lists
  return(list(
    fname_image = files_images,
    fname_cells = files_cells,
    fname_rings = files_rings,
    fname_settings = files_settings
    )
  )
}


#' Extract the data structure from image filenames
#'
#' Extract the structure of the data (i.e., which images belong to which slide,
#' woodpiece, tree, site) from the filenames of the input data into a dataframe.
#' This requires that all files follow the same labeling pattern.
#'
#' NOTE: At the moment, we assume that all images are named according to the
#' following pattern:
#' `{site}_{species}_{tree}{woodpiece}_{sample}_{image}`,
#' where tree is assumed to be a two-character code, and woodpiece is optional.
#'
#' NOTE: we already checked that all output filenames match in `get_input_files`,
#' so it is ok to do the pattern extraction on the image filenames only.
#'
#' NOTE: no matter the original labeling pattern, we then use site, species,
#' tree, woodpiece, sample, image joined by underscores as identifiers for each
#' level of the data structure for the subsequent calculations
#' (e.g. `tree_code = {site}_{species}_{tree}`).
#'
#' @param files The list of lists with all input filenames.
#'
#' @returns A dataframe containing the filenames and data structure.
#' @export
extract_data_structure <- function(files) {
  # TODO: allow for other labeling structures
  # e.g., site/species/tree/sample/imgname.jpg, or other variants
  # NOTE: we expect the basenames of the files to have the following structure:
  lbl_structure <- '{site}_{species}_{tree}{woodpiece}_{sample}_{image}'
  # in regex, this corresponds to the following pattern (NOTE the named groups)
  pattern <- "^(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:][:alnum:])(?<woodpiece>[:alnum:]*)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)$"

  # remove paths and extensions from the image filenames (e.g. ".jpg")
  fnames <- basename(files$fname_image) %>%
    stringr::str_split_i("\\.",1)

  # check that we have no duplicate image labels (e.g. from different subfolders)
  duplicates <- fnames[duplicated(fnames)]
  if (length(duplicates > 0)) {
    beepr::beep(sound = 2, expr = NULL)
    stop(
      "There are duplicate image files, please remove or rename:", "\n",
      paste(paste0("  ", grep(paste(duplicates, collapse ='|'),
                              files$fname_image, value = TRUE)),
            collapse = "\n")
    )
  }

  # check that all fnames match the pattern
  if (!all(stringr::str_detect(fnames, pattern))) {
    beepr::beep(sound = 2, expr = NULL)
    stop("The label structure of the files is not consistent.\n",
         "  All labels must be composed following the structure:\n",
         paste0("    ",lbl_structure)
    )
  }

  # extract the matched pattern groups and collect info into df
  df_structure <- as.data.frame(stringr::str_match(fnames,pattern))
  df_structure <- df_structure %>%
    dplyr::rename(org_img_name = V1) %>% # original pattern is in column 1
    dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::na_if(.x,""))) %>%
    tidyr::unite('tree_code', site:tree,
                 sep = '_', na.rm = TRUE, remove = FALSE) %>%
    tidyr::unite('woodpiece_code', site:woodpiece,
                 sep = '_', na.rm = TRUE, remove = FALSE) %>%
    tidyr::unite('slide_code', site:slide,
                 sep = '_', na.rm = TRUE, remove = FALSE) %>%
    tidyr::unite('image_code', site:image,
                 sep = '_', na.rm = TRUE, remove = FALSE) %>%
    dplyr::select(org_img_name, site:image,
                  tree_code, woodpiece_code, slide_code, image_code) # reorder columns

  df_structure <- cbind(as.data.frame(files),df_structure) # add filenames

  # report the identified woodpieces and beep successful ending of the function
  # TODO: woodpiece_code versus org label?
  message("Data structure successfully extracted from ROXAS filenames\n",
          "for the following ",
          length(unique(df_structure$woodpiece_code)),
          " woodpiece codes:\n",
          paste(paste0(' ', unique(df_structure$woodpiece_code)), collapse = "\n"))
  beepr::beep(sound = 1, expr = NULL)

  return(df_structure)
}


#' Recover original labels from data structure
#'
#' Inverse of extract_data_structure, i.e. given a dataframe with the
#' data structure (site, species, tree, woodpiece, sample, image), and the
#' labeling pattern followed by the original image filenames, we reconstruct
#' the filenames.
#' This requires that all files follow the same labeling pattern.
#'
#' NOTE: At the moment, only tested with following pattern:
#' `{site}_{species}_{tree}{woodpiece}_{sample}_{image}`
#' where tree is assumed to be a two-character code, and woodpiece is optional.

#'
#' @param df_structure The df with the data structure.
#' @param pattern The labeling pattern followed by the original image filenames,
#' as a regex expression with named groups.
#'
#' @returns A list of reconstructed file names (without filetype extension)
# TODO: do we really need this? export? (now we have image_name in df_structure?)
reconstruct_img_names <- function(df_structure, pattern) {
  # which groups are named in the pattern?
  named_groups <- stringr::str_match_all(pattern, "\\?<(\\w+)>")[[1]][,2]

  all_groups <- c('site','species','tree','woodpiece','slide','image','some')

  if (length(setdiff(named_groups, all_groups)) > 0) {
    beepr::beep(sound = 2, expr = NULL)
    stop("The pattern contains invalid named groups.")
  }

  # construct a template image name out of the pattern
  template <- stringr::str_replace_all(pattern, "\\^|\\$", "")  # remove start and end anchors
  template <- stringr::str_replace_all(template, "\\(\\?<(\\w+)>[^)]+\\)", "{\\1}") # replace group regex with {group}

  # get the correct columns from df_structure
  df_names <- df_structure[named_groups]
  df_names[is.na(df_names)] <- ""

  # replace groups in template with corresponding values from df_names
  file_names <- template
  for (col in named_groups) {
    file_names <- stringr::str_replace_all(file_names, paste0("\\{", col, "\\}"), df_names[[col]])
  }

  if (nunique(file_names) != nrow(df_structure)) {
    beepr::beep(sound = 2, expr = NULL)
    stop("The reconstructed filenames are not unique, check the provided pattern.")
  }

  return(file_names)
}


#' Include or exclude woodpiece codes from data collection
#'
#' This function filters the data structure dataframe based on the woodpiece codes provided
#'
#' @param df_structure The dataframe containing all input filenames and data structure.
#' @param include_codes EITHER provide a vector of the woodpiece codes to be included
#' @param exclude_codes OR provide a vector of the woodpiece codes to be excluded
#'
#' @returns filtered df_structure.
#' @export
subset_woodpiece_codes <- function(df_structure,
                             include_codes=NULL,
                             exclude_codes=NULL) {
  # check that only one of the two options is used
  if (!xor(is.null(include_codes), is.null(exclude_codes))){
    beepr::beep(sound = 2, expr = NULL)
    stop("Please provide either `include_codes` or `exclude_codes`, but not both.")
  }

  # if include_codes is not null, filter for these
  if (!is.null(include_codes)){
    # check input for validity
    beepr::beep_on_error(
      checkmate::assert_subset(include_codes, df_structure$woodpiece_code),
      sound=2
    )
    df_struct_filt <- df_structure %>% dplyr::filter(woodpiece_code %in% include_codes)
  }

  # if exclude_codes is not null, filter these out
  if (!is.null(exclude_codes)){
    # check input for validity
    beepr::beep_on_error(
      checkmate::assert_subset(exclude_codes, df_structure$woodpiece_code),
      sound=2
    )
    df_struct_filt <- df_structure %>% dplyr::filter(!(woodpiece_code %in% exclude_codes))
  }

  return(df_struct_filt)
}


#' Read and combine image exif data
#'
#' Collect the exif data from all image files
#'
#' @param files_images Vector/list of image filenames
#'
#' @returns A dataframe containing the extracted data.
#'
# TODO: check this works on Windows? (exifr requires PERL)
# TODO: is it robust for different image types?
# TODO: can get date as well if we have original images? error handling for missing tags?
collect_image_info <- function(files_images) {
  df_image_meta <- exifr::read_exif(files_images,
                                    tags = c(
                                      "FileType", "FileSize",
                                      "ImageWidth", "ImageHeight")) %>%
    dplyr::rename(fname_image = SourceFile,
                  img_filetype = FileType,
                  img_size = FileSize,
                  img_width = ImageWidth,
                  img_height = ImageHeight
    )
  return(df_image_meta)
}


#' Extract data from a ROXAS settings file
#'
#' Helper function to read and extract the relevant metadata from a single ROXAS
#' settings file
#'
#' @param file_settings The file to be read.
#' @param roxas_version The version of ROXAS used to create the file (classic, AI)
#'
#' @returns A dataframe containing the extracted data.
#'
# TODO: check that this works for all old versions of ROXAS
#       it looks like it works for ROXAS versions
#       3.0.285, 3.0.575, 3.0.590, 3.0.608, 3.0.620, 3.0.634, 3.0.655 (different date formats)
# TODO: add support for ROXAS AI
extract_roxas_settings <- function(file_settings,
                                   roxas_version = 'classic') {
  # check input
  beepr::beep_on_error(
    checkmate::assert_subset(roxas_version, c('classic')), # TODO: add others
    sound=2
  )

  # read from a single settings file
  df_settings <- readr::read_delim(file_settings,
                        delim = "\t",
                        col_types = readr::cols(.default = "c", RNUM = "d"))

  # extract and reformat the relevant info
  if (roxas_version == 'classic'){
    # NOTE: this relies heavily on the consistent layout of the settings file
    # in particular, we need tab delimiters, columns RNUM, SETTING, DESCRIPTION
    # and the right values in the rows 8,9,10,12,13,17,20,31,33,166,203:208!
    # TODO: check for different / older versions of ROXAS?
    df_settings <- df_settings %>%
      dplyr::filter(RNUM %in% c(8,9,10,
                                12,13,
                                17,18,19,20,
                                31,33,166,
                                203,204,205,206,207,208
                                )) %>%
      dplyr::mutate(new_names = c(
        "configuration_file", "created_at", "sw_version",
        "spatial_resolution", "origin_calibrated",
        "meas_geometry", "circ_lower_limit", "circ_upper_limit", "outmost_year",
        "min_cell_area", "max_cell_area", "dbl_cwt_threshold",
        "max_cwtrad_s", "max_cwtrad_l", "relwidth_cwt_window", "maxrel_opp_cwt",
        "max_cwttan_s", "max_cwttan_l"
      )) %>%
      dplyr::select(SETTING, new_names) %>%
      tidyr::pivot_wider(names_from = new_names, values_from = SETTING) %>%
      dplyr::mutate(
        meas_geometry = dplyr::if_else(meas_geometry==1, "linear", "circular"),
        fname_settings = file_settings,
        software = "ROXAS") %>%
      tidyr::separate(origin_calibrated,
                      into = c("origin_calibrated_x", "origin_calibrated_y"),
                      sep = "[ ]*/[ ]*", remove = TRUE, convert = TRUE) %>%
      dplyr::relocate(fname_settings, software, sw_version)
  }

  # TODO: roxasAI
  # if (roxas_version == 'ROXAS AI'){
  # ...
  # }

  return(df_settings)
}


#' Read and combine ROXAS settings data
#'
#' Collect the settings data from all ROXAS settings files
#'
#' @param files_settings Vector/list of ROXAS settings filenames
#' @param roxas_version The version of ROXAS used to create the files (classic, AI)
#'
#' @returns A dataframe containing the extracted data.
#'
collect_settings_data <- function(files_settings,
                                  roxas_version = 'classic',
                                  tz = NULL) {
  df_settings_all <- files_settings %>%
    purrr::map(\(x) extract_roxas_settings(x, roxas_version = roxas_version)) %>%
    purrr::list_rbind()

  # convert string columns to numeric and integer
  df_settings_all <- df_settings_all %>%
    dplyr::mutate(dplyr::across(c(spatial_resolution,
                                  dbl_cwt_threshold:max_cwttan_l), as.numeric),
                  dplyr::across(circ_lower_limit:max_cell_area, as.integer))

  # convert created_at string to datetime
  # TODO: check robustness for different date formats
  if (is.null(tz)) {
    tz <- Sys.timezone()
  }
  conv_dates <- df_settings_all$created_at %>% lubridate::mdy_hm(., tz=tz)
  # plausibility checks: no NA and not before outmost year
  check_dates <- any(is.na(conv_dates)) |
    any(df_settings_all$outmost_year > lubridate::year(conv_dates))
  if (check_dates){
    beepr::beep(sound = 2, expr = NULL)
    stop("Error converting dates in the settings files.")
  }
  df_settings_all$created_at <- conv_dates

  return(df_settings_all)
}


#' Read and combine raw metadata
#'
#' Collect the ROXAS settings and image exif data from all raw files and
#' combine into one dataframe with the data structure.
#'
#' @param df_structure Dataframe with all input filenames
#' @param roxas_version ROXAS version (required to read the settings files)
#' @param tz Timezone for date conversion of settings created at date (default: system tz)
#'
#' @returns A dataframe containing the extracted data.
#' @export
collect_metadata <- function(df_structure, roxas_version, tz = NULL) {
  df_images <- collect_image_info(df_structure$fname_image)
  df_settings <- collect_settings_data(df_structure$fname_settings,
                                       roxas_version, tz=tz)

  df_meta <- df_structure %>%
    dplyr::left_join(df_images, by='fname_image') %>%
    dplyr::left_join(df_settings, by='fname_settings')

  # beep successful ending of the function
  message("Available metadata successfully extracted from the raw files.")
  beepr::beep(sound = 1, expr = NULL)

  return(df_meta)
}


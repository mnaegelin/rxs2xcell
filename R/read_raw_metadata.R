#' Get list of input files
#'
#' Given a path_in, get the paths and filenames of the ROXAS files to be imported
#'
#' @param path_in path of the input directory.
#'
#' @returns A list of lists of the images and ROXAS files.
#' @export
get_input_files <- function(path_in) {

  # Regex pattern to be matched by the different ROXAS files
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
    fname_cells = files_cells,
    fname_rings = files_rings,
    fname_settings = files_settings,
    fname_image = files_images
    )
  )
}


#' Extract the data structure from image filenames
#'
#' Extract the structure of the data (i.e., which images belong to which slide,
#' woodpiece, tree, site) from the filenames of the input data into a dataframe.
#' This requires that all files follow the same labeling pattern of
#' `site_species_treeWoodpiece_sample_image`
#' NOTE: we already checked that all output filenames match, so it is ok
#' to do the pattern extraction on the image filenames only.
#'
#' @param files The list of lists with all input filenames.
#'
#' @returns A dataframe containing the filenames and data structure.
#' @export
extract_data_structure <- function(files) {
  # TODO: allow for different labeling structures
  # e.g., site/species/tree/sample/imgname.jpg, or other variants
  # NOTE: we expect the basenames of the files to have the following structure:
  lbl_structure <- 'site_species_treeWoodpiece_sample_image'
  # in regex, this corresponds to the following pattern (NOTE the named groups)
  # pattern <- "^(?<site>[:alnum:]+)(?<plot>)_(?<species>[:alnum:]+)_(?<tree>[:alnum:][:alnum:])(?<woodpiece>[:alpha:]*)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)$"
  pattern <- "^(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:][:alnum:])(?<woodpiece>[:alpha:]*)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)$"

  # remove paths and extensions from the image filenames (e.g. ".jpg")
  fnames <- basename(files$fname_image) %>%
    stringr::str_split_i("\\.",1)

  # check that we have no duplicate image labels (e.g. from different subfolders)
  duplicates <- fnames[duplicated(fnames)]
  if (length(duplicates > 0)) {
    beepr::beep(sound = 2, expr = NULL)
    stop(
      "There are duplicate image files:", "\n",
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
    dplyr::rename(complete_match = V1) %>% # original pattern is in column 1
    dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::na_if(.x,""))) %>%
    tidyr::unite('tree_code', site:tree,
                 sep = '_', na.rm = TRUE, remove = FALSE) %>%
    tidyr::unite('woodpiece_code', site:woodpiece,
                 sep = '_', na.rm = TRUE, remove = FALSE) %>%
    tidyr::unite('slide_code', site:slide,
                 sep = '_', na.rm = TRUE, remove = FALSE) %>%
    tidyr::unite('image_code', site:image,
                 sep = '_', na.rm = TRUE, remove = FALSE) %>%
    dplyr::select(site:image,
                  tree_code, woodpiece_code, slide_code, image_code) # reorder columns

  df_structure <- cbind(as.data.frame(files),df_structure) # add filenames

  # warn if some labels are too long to comply with .rwl format
  # TODO: is this really relevant? do we create an rwl? correct to use site + sample?
  # toolong_for_rwl <- df_structure[
  #   nchar(df_structure$site) + nchar(df_structure$sample) > 8,'image_code']
  # if (length(toolong_for_rwl)>0) {
  #   beepr::beep(sound = 10, expr = NULL)
  #   warning("The following image codes exceed the maximum number of characters.\n",
  #           "For compliance with .rwl fomat, `site` and `sample` combined\n",
  #           "should not exceed 8 characters (or 7 if rings extend into BCE).\n",
  #           paste(paste0(' ', toolong_for_rwl), collapse='\n'))
  # }

  # report the identified treecodes and beep successful ending of the function
  message("Data structure successfully extracted from ROXAS filenames\n",
          "for the following ",
          length(unique(df_structure$tree_code)),
          " treecode(s):\n",
          paste(paste0(' ', unique(df_structure$tree_code)), collapse = "\n"))
  beepr::beep(sound = 1, expr = NULL)

  return(df_structure)
}

# TODO: exclude woodpieces rather than trees?
#' Include or exclude tree codes from data collection
#'
#' This function filters the data structure dataframe based on the tree codes provided
#'
#' @param df_structure The dataframe containing all input filenames and data structure.
#' @param include_codes EITHER provide a vector of the tree codes to be included
#' @param exclude_codes OR provide a vector of the tree codes to be excluded
#'
#' @returns df_structure with the filtered tree codes.
#' @export
subset_treecodes <- function(df_structure,
                             include_codes=NULL,
                             exclude_codes=NULL) {
  # check that only one of the two options is used
  if (xor(is.null(include_codes), is.null(exclude_codes))){
    beepr::beep(sound = 2, expr = NULL)
    stop("Please provide either `include_codes` or `exclude_codes`, but not both.")
  }

  # if include_codes is not null, filter for these
  if (!is.null(include_codes)){
    # check input for validity
    beepr::beep_on_error(
      checkmate::assert_subset(include_codes, df_structure$tree_code),
      sound=2
    )
    df_struct_filt <- df_structure %>% dplyr::filter(tree_code %in% include_codes)
  }

  # if exclude_codes is not null, filter these out
  if (!is.null(exclude_codes)){
    # check input for validity
    beepr::beep_on_error(
      checkmate::assert_subset(exclude_codes, df_structure$tree_code),
      sound=2
    )
    df_struct_filt <- df_structure %>% dplyr::filter(!(tree_code %in% exclude_codes))
  }

  return(df_struct_filt)
}



#' Extract data from ROXAS settings file
#'
#' Helper function to read and extract the metadata from a single ROXAS settings file
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
# TODO: date formatting
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
    # and the right values in the rows 8,9,10,12,13,17,20!
    # TODO: check for different / older versions of ROXAS?
    df_settings <- df_settings %>%
      dplyr::filter(RNUM %in% c(8,9,10,12,13,17,20)) %>%
      dplyr::mutate(new_names = c(
        "configuration", "date_created", "software_version",
        "spatial_resolution", "origin_calibrated",
        "sample_geometry", "outmost_year"
      )) %>%
      dplyr::select(SETTING, new_names) %>%
      tidyr::pivot_wider(names_from = new_names, values_from = SETTING) %>%
      dplyr::mutate(
        sample_geometry = dplyr::if_else(sample_geometry==1, "linear", "circular"),
        fname_settings = file_settings,
        software = "ROXAS") %>%
      dplyr::relocate(fname_settings, software, software_version)
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
                                  roxas_version = 'classic') {
  df_settings_all <- files_settings %>%
    purrr::map(\(x) extract_roxas_settings(x, roxas_version = roxas_version)) %>%
    purrr::list_rbind()

  return(df_settings_all)
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
# TODO: which of these do we really need? is it robust for different image types?
# TODO: can get date as well if we have original images? error handling for missing tags?
collect_image_info <- function(files_images) {
  df_image_meta <- exifr::read_exif(files_images,
                                    tags = c(
                                      "FileSize", "FileType",
                                      "XResolution", "YResolution",
                                      "ImageWidth", "ImageHeight")) %>%
    dplyr::rename(fname_image = SourceFile,
                  img_size = FileSize,
                  img_filetype = FileType,
                  img_width = ImageWidth,
                  img_height = ImageHeight,
                  img_resolution_x = XResolution,
                  img_resolution_y = YResolution)
  return(df_image_meta)
}


#' Read and combine raw metadata
#'
#' Collect the ROXAS settings and image exif data from all raw files and
#' combine into one dataframe with the data structure.
#'
#' @param df_structure Dataframe with all input filenames
#' @param roxas_version ROXAS version (used to read the settings files)
#'
#' @returns A dataframe containing the extracted data.
#' @export
collect_metadata <- function(df_structure, roxas_version) {
  df_settings <- collect_settings_data(df_structure$fname_settings, roxas_version)
  df_images <- collect_image_info(df_structure$fname_image)

  df_meta <- df_structure %>%
    dplyr::left_join(df_settings, by='fname_settings') %>%
    dplyr::left_join(df_images, by='fname_image')

  # beep successful ending of the function
  message("Available metadata successfully extracted from the raw files.")
  beepr::beep(sound = 1, expr = NULL)

  return(df_meta)
}









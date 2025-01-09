
pattern_cell_files = "_Output_Cells\\.txt$"
pattern_ring_files = "_Output_Rings\\.txt$"
pattern_settings_files = "_ROXAS_Settings\\.txt$"
pattern_orgimg_files = "\\.(jpg|jpeg)$" # "\\.(jpg|jpeg|png|gif|bmp|tiff)$"
imgfiles_exclude_keywords = c("annotated",
                              "ReferenceSeries",
                              "Preview")


#' Get input files
#'
#' Given a path_in, get the paths of relevant ROXAS files to be imported
#'
#' @param path_in path of the input directory.
#' @returns A list of lists of the images and ROXAS files.
#'
get_input_files <- function(path_in,
                            pattern_cell_files,
                            pattern_ring_files,
                            pattern_settings_files,
                            pattern_orgimg_files,
                            imgfiles_exclude_keywords) {

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
  # TODO: maybe match on "FILENAME\\.(jpg|jpeg)$" from the settings files
  #       instead of manually excluding keywords?
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



# check that the filenames follow the right labeling structure of
# `site_species_tree_sample_subsample`
# and extract this information into a dataframe
# TODO: allow for different labeling structures
# e.g., site/species/tree/sample/imgname.jpg, or other variants
# NOTE: we already checked that all output filenames match, so it is ok
# to work with image filenames only
extract_data_structure <- function(files) {
  # we expect the basenames of the files to have the following structure:
  lbl_structure <- 'site_species_tree_sample_subsample'
  # in regex, this corresponds to the following pattern (NOTE the named groups)
  pattern <- "^(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:]+)_(?<sample>[:alnum:]+)_(?<subsample>[:alnum:]+)$"

  # remove paths and filename extensions (e.g. ".jpg")
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
    dplyr::rename(image_code = V1) %>% # full pattern is in column 1
    tidyr::unite('tree_code', site, species, tree, sep = '_', remove = FALSE) %>%
    dplyr::select(image_code, tree_code,
                  site, species, tree, sample, subsample)

  df_structure <- cbind(as.data.frame(files),df_structure)


  # warn if some labels are too long to comply with .rwl format
  # TODO: is this really relevant? do we create an rwl? correct to use site + sample
  toolong_for_rwl <- df_structure[
    nchar(df_structure$site) + nchar(df_structure$sample) > 8,'image_code']
  if (length(toolong_for_rwl)>0) {
    beepr::beep(sound = 10, expr = NULL)
    warning("The following image codes exceed the maximum number of characters.\n",
            "For compliance with .rwl fomat, `site` and `sample` combined\n",
            "should not exceed 8 characters (or 7 if rings extend into BCE).\n",
            paste(paste0(' ', toolong_for_rwl), collapse='\n'))
  }

  return(df_structure)
}

#' Extract ROXAS settings
#'
#' Read and extract from a single ROXAS settings file
extract_roxas_settings <- function(file_settings,
                                   roxas_version = 'classic') {
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
    # and the right values in the rows 8,9,10,11,12,13,17,20!
    # TODO: check for older versions of ROXAS?
    df_settings <- df_settings %>%
      dplyr::filter(RNUM %in% c(8,9,10,11,12,13,17,20)) %>%
      dplyr::mutate(new_names = c(
        "configuration", "date_created", "software_version",
        "author", "spatial_resolution", "origin_calibrated",
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

#' Collect ROXAS settings
#'
#' Collect the settings data from all ROXAS settings files
collect_settings_data <- function(files_settings,
                                  roxas_version = 'classic') {
  df_settings_all <- files_settings %>%
    purrr::map(\(x) extract_roxas_settings(x, roxas_version = roxas_version)) %>%
    purrr::list_rbind()

  return(df_settings_all)
}

# collect image exif data from image files
# TODO: check this works on Windows?
# TODO: which of these do we really need?
# TODO: can get date as well if we have original images? error handling for missing?
collect_image_info <- function(files_images) {
  df_image_meta <- exifr::read_exif(files_images,
                                    tags = c(
                                      "FileSize", "FileType",
                                      "XResolution", "YResolution",
                                      "ImageWidth", "ImageHeight")) %>%
    dplyr::rename(fname_image = SourceFile,
                  image_size = FileSize,
                  imageType = FileType,
                  ImageXResolution = XResolution,
                  ImageYResolution = YResolution)
  return(df_image_meta)
}



# collect and combine the settings, image info and label structure metadata
# of all input files
collect_metadata <- function(df_structure, roxas_version) {
  df_settings <- collect_settings_data(df_structure$fname_settings, roxas_version)
  df_images <- collect_image_info(df_structure$fname_image)


  df_meta <- df_structure %>%
    dplyr::left_join(df_settings, by='fname_settings') %>%
    dplyr::left_join(df_images, by='fname_image')

  # report the identified treecodes and beep successful ending of the function
  message("The ROXAS files have been verified and their metadata\n",
          "successfully collected for the following ",
          length(unique(df_meta$tree_code)),
          " treecode(s):\n",
          paste(paste0(' ', unique(df_meta$tree_code)), collapse = "\n"))
  beepr::beep(sound = 1, expr = NULL)
  return(df_meta)
}







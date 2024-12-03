

get_input_files <- function(path_in,
                            pattern_cell_files,
                            pattern_ring_files,
                            pattern_settings_files,
                            pattern_orgimg_files) {
  # TODO: add flag of whether the image filenames should also be checked?
  # since technically, they are not needed/stored in db at the moment

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
  # TODO: in future, there might be other image output files
  #       better to match on "FILENAME\\.(jpg|jpeg)$" from the settings files?
  files_images <- list.files(path_in,
                             pattern = pattern_orgimg_files,
                             full.names = TRUE, recursive = TRUE,
                             ignore.case = TRUE, include.dirs = TRUE
                             ) %>%
    .[!stringr::str_detect(., "annotated")] %>%
    .[!stringr::str_detect(., "ReferenceSeries")] %>%
    .[!stringr::str_detect(., "Preview")]


  l_files <- list(
    sub(pattern_cell_files, "", files_cells, ignore.case = TRUE),
    sub(pattern_ring_files, "", files_rings, ignore.case = TRUE),
    sub(pattern_settings_files, "", files_settings, ignore.case = TRUE),
    sub(pattern_orgimg_files, "", files_images, ignore.case = TRUE)
  )

  # check that we did find some valid files
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
  if (length(dontmatch) != 0) {
    beepr::beep(sound = 2, expr = NULL)
    stop(
      "ROXAS output files DON'T MATCH for:", "\n",
      paste(paste0("  ", dontmatch), collapse = "\n")
    )
  }

  # return all the file paths we need as lists
  return(list(
    files_cells = files_cells,
    files_rings = files_rings,
    files_settings= files_settings,
    files_images = files_images
    )
  )
}


extract_roxas_settings <- function(file_settings) {
  # read from a single settings file
  df_settings <- readr::read_delim(file_settings,
                        delim = "\t",
                        col_types = readr::cols(.default = "c", RNUM = "d"))

  # extract and reformat the relevant info
  # NOTE: this relies heavily on the consistent layout of the settings file
  # in particular, we need tab delimiters, columns RNUM, SETTING, DESCRIPTION
  # and the right values in the rows 1:5, 7, 10 AFTER NA REMOVAL!
  # if the settings file structure is updated, this likely breaks
  # TODO: which of these variables do we really need?
  # TODO: make it so this isnt hardcoded, e.g. have settings file line numbers as input
  # TODO: according to Georg, origin should be added to variables
  df_settings <- df_settings %>%
    dplyr::select(DESCRIPTION, SETTING) %>%
    dplyr::filter(!is.na(DESCRIPTION)) %>%
    dplyr::slice(c(1:5, 7, 10)) %>%
    dplyr::mutate(new_names = c(
      "configuration", "dateCreated", "software_version",
      "author", "calibration_pixels_micron",
      "meas_geometry", "outmostYear"
    )) %>%
    dplyr::select(SETTING, new_names) %>%
    tidyr::pivot_wider(names_from = new_names, values_from = SETTING) %>%
    dplyr::mutate(
      meas_geometry = dplyr::if_else(meas_geometry==1, "linear", "circular"),
      files_settings = file_settings) %>%
    dplyr::relocate(files_settings)

  return(df_settings)
}

# collect the settings data from all settings files
collect_settings_data <- function(files_settings) {
  df_settings_all <- purrr::map(files_settings, extract_roxas_settings) %>%
    purrr::list_rbind()
  return(df_settings_all)
}

# collect image exif data from image files
# TODO: check this works on Windows?
# TODO: which of these do we really need?
collect_image_info <- function(files_images) {
  df_image_meta <- exifr::read_exif(files_images,
                                    tags = c(
                                      "FileSize", "FileType",
                                      "XResolution", "YResolution",
                                      "ImageWidth", "ImageHeight")) %>%
    dplyr::rename(files_images = SourceFile,
                  image_size = FileSize,
                  imageType = FileType,
                  ImageXResolution = XResolution,
                  ImageYResolution = YResolution)
  return(df_image_meta)
}

# check that the filenames follow the right labelling structure of
# `site_species_tree_sample_subsample`
# and extract this information into a dataframe
# TODO: what if site/spec/tree/sample/imgname.jpg? or a variant thereof?
collect_label_structure <- function(files_images, pattern_orgimg_files) {
  # NOTE: we already checked that all output filenames match, so it is ok
  # to work with image filenames only
  fnames <- basename(files_images)

  # check that we have no duplicate image labels (e.g. from different subfolders)
  duplicates <- fnames[duplicated(fnames)]
  if (length(duplicates > 0)) {
    beepr::beep(sound = 2, expr = NULL)
    stop(
      "There are duplicate image files:", "\n",
      paste(paste0("  ", grep(paste(duplicates, collapse ='|'),
                              files_images, value = TRUE)),
            collapse = "\n")
    )
  }

  # check that we have a five-part structure for all names
  label_str <- sub(pattern_orgimg_files, "", fnames) %>%
    stringr::str_split(., "_")

  if (any(lengths(label_str) != 5)) {
    beepr::beep(sound = 2, expr = NULL)
    stop("The label structure of the files is not consistent.\n",
         "  All labels must be composed of 5 elements, following the structure:\n",
         "    `site_species_tree_sample_subsample`\n",
         "  For example, `YAM_LASI_224_01_1`."
         )
  }

  # collect label info into df
  df_structure <- data.frame(
    files_images = files_images,
    image_code = sub(pattern_orgimg_files, "", fnames)) %>%
    tidyr::separate(image_code,
                    c("site","species","tree","sample","subsample"),
                    "_", remove = FALSE) %>%
    tidyr::unite('tree_code', site:tree, sep = '_', remove = FALSE)

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

# collect and combine the settings, image info and label structure metadata
# of all input files
collect_metadata <- function(files, pattern_orgimg_files) {
  df_settings <- collect_settings_data(files$files_setting)
  df_images <- collect_image_info(files$files_images)
  df_structure <- collect_label_structure(files$files_images,
                                          pattern_orgimg_files)

  df_meta <- data.frame(files) %>%
    dplyr::left_join(df_structure, by='files_images') %>%
    dplyr::left_join(df_settings, by='files_settings') %>%
    dplyr::left_join(df_images, by='files_images')

  # report the identified treecodes and beep successful ending of the function
  message("The Roxas files have been verified and their metadata\n",
          "successfully collected for the following ",
          length(unique(df_meta$tree_code)),
          " treecode(s):\n",
          paste(paste0(' ', unique(df_meta$tree_code)), collapse = "\n"))
  beepr::beep(sound = 1, expr = NULL)
  return(df_meta)
}







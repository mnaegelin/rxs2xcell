path_in <- '../test_data'


check_path_in_filenames <- function(path_in) {
  # TODO: add flag of whether the image filenames should also be checked?
  # since technically, they are not needed/stored in db at the moment

  # assert that path_in is valid
  beepr::beep_on_error(
    checkmate::assert_directory_exists(path_in),
    sound=2
  )

  # define the patterns of ROXAS output files we want
  # NOTE: the leading _underscore
  pattern_cell_files = "_Output_Cells\\.txt$"
  pattern_ring_files = "_Output_Rings\\.txt$"
  pattern_settings_files = "_ROXAS_Settings\\.txt$"
  pattern_orgimg_files = "\\.(jpg|jpeg)$" # "\\.(jpg|jpeg|png|gif|bmp|tiff)$"

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

  # identify any mismatches between ring, cell, settings, and image files
  l_files <- list(
    sub(pattern_cell_files, "", files_cells, ignore.case = TRUE),
    sub(pattern_ring_files, "", files_rings, ignore.case = TRUE),
    sub(pattern_settings_files, "", files_settings, ignore.case = TRUE),
    sub(pattern_orgimg_files, "", files_images, ignore.case = TRUE)
  )
  dontmatch <- setdiff(
    Reduce(union, l_files),
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
    files_setting = files_settings,
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
      file_settings = file_settings) %>%
    dplyr::relocate(file_settings)

  return(df_settings)
}

collect_settings_data <- function(files_settings) {
  df_settings_all <- purrr::map(files_settings, extract_roxas_settings) %>%
    purrr::list_rbind()
  return(df_settings_all)
}

# TODO: check this works on Windows?
# TODO: which of these do we really need?
collect_image_data <- function(files_images) {
  df_image_meta <- exifr::read_exif(l_files$files_images,
                                    tags = c(
                                      "FileSize", "FileType",
                                      "XResolution", "YResolution",
                                      "ImageWidth", "ImageHeight")) %>%
    dplyr::rename(file_image = SourceFile,
                  image_size = FileSize,
                  imageType = FileType,
                  ImageXResolution = XResolution,
                  ImageYResolution = YResolution)
  return(df_image_meta)
}


df <- collect_settings_data(l_files$files_setting)
df2 <- collect_image_data(l_files$files_images)



# "magnification"
# "only_ew"
# "editing_level"
# "tangentail_image_width"
# "band_width"


# TODO: check/infer the filename structure site_tree etc

# TODO: create output directory / directories (including templates?)

# TODO: tbl_info with combined paths, roxas settings, image info
# create Diagnostic_tbl and Export it in path_out
# Report the identified sample/treeID



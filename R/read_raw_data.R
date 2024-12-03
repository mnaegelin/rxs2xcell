
# TODO: the txt files have only two digits after decimal point for many variables -> enough precision?
# or should we read form xlsx instead?

read_raw_data <- function(df_meta, file_type, subset_treecodes = NULL){
  # check that the type of files to be read is correctly specified
  beepr::beep_on_error(
    checkmate::assert_choice(file_type, c('cells','rings')),
    sound=2
    )

  files_colname <- paste0('files_',file_type)

  sel_tree_codes <- df_meta$tree_code

  # if a subset of treecodes is supplied, check for validity
  if (!is.null(subset_treecodes)) {
    beepr::beep_on_error(
      checkmate::assert_character(subset_treecodes),
      sound=2
    )
    beepr::beep_on_error(
      checkmate::assert_subset(subset_treecodes, df_meta$tree_code),
      sound=2
    )
    sel_tree_codes <- subset_treecodes
  }

  # read all the output files and store in large df
  # TODO: read only the necessary columns?
  # use col_types = cols_only(ID = 'c', col1 = 'd', col2 = 'd', ...)
  df_raw <- df_meta %>%
    dplyr::select(!!dplyr::sym(files_colname), tree_code, image_code) %>%
    dplyr::filter(tree_code %in% sel_tree_codes) %>%
    dplyr::mutate(
      raw_data = purrr::map(
        !!dplyr::sym(files_colname),
        ~ readr::read_delim(.x, delim = "\t",
                            col_types = readr::cols(.default="d", ID="c")))
    ) %>%
    tidyr::unnest(raw_data) %>%
    dplyr::select(-!!dplyr::sym(files_colname), -dplyr::matches('^AOE_\\d'))

  # TODO: check id against image_code?

  # beep successful ending of the function
  message("The raw Roxas ", file_type, " files have been read successfully!")
  beepr::beep(sound = 1, expr = NULL)

  return(df_raw)
}


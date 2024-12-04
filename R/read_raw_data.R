
# TODO: the txt files have only two digits after decimal point for many variables -> enough precision?
# or should we read form xlsx instead?

# helper function to read all the (ring or cell) output files and store in large df
read_raw_data <- function(df_meta, files_colname = 'files_cells', sel_tree_codes){
  # NOTE: all columns except ID are converted to numeric
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
    dplyr::select(-!!dplyr::sym(files_colname))

  # TODO: check id against image_code?

  # beep successful ending of the function
  message("The raw Roxas ", strsplit(files_colname,'_')[[1]][[2]], " files have been read successfully!")
  beepr::beep(sound = 1, expr = NULL)

  return(df_raw)
}


collect_raw_data <- function(df_meta, subset_treecodes = NULL){

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

  # read in the raw data
  df_rings_raw <- read_raw_data(df_meta, 'files_rings', sel_tree_codes)
  df_cells_raw <- read_raw_data(df_meta, 'files_cells', sel_tree_codes)

  # specify the columns we want, and at same time harmonizing column names of
  # different ROXAS versions
  selcols_rings <- c(
    'YEAR', 'RA', 'MRW', 'CWTTAN'
  )
  selcols_cells <- c(
    'YEAR', 'CID', 'XPIX', 'YPIX', 'RADDISTR', 'RRADDISTR',
    'LA', 'ASP', 'MAJAX', 'KH',
    'CWTPI', 'CWTBA', 'CWTLE', 'CWTRI', 'CWTTAN', 'CWTRAD', 'CWTALL',
    'RTSR', 'CTSR', 'DRAD', 'DTAN', 'TB2', 'CWA', 'RWD'
    # not included cols are:
    # ID, RADDIST, ANGLE, XCAL, YCAL (superfluous)
    # NBRNO, NBRID, NBRDST (relates to groups of cells)
    # AOI (relates to areas of interest)
    # DH ??
  )
  # TODO: why not DH? is manually calculated later

  # define any variant name mappings from old ROXAS versions
  # use format: current_name = 'old_name', current_name = 'older_name', etc.
  colname_variants <- c(
    TB2 = 'BEND',
    TB2 = 'CRI',
    LA = 'CA'
  )

  # rename and subset columns, catching errors
  tryCatch(
    beepr::beep_on_error(
      df_rings_raw <- df_rings_raw %>%
        dplyr::rename(dplyr::any_of(colname_variants)) %>%
        dplyr::select(tree_code, image_code, all_of(selcols_rings)),
      sound=2
      ),
    error = function(e){
      message("An error occurred subsetting the rings columns\n",
               "check the raw files:\n", e)
    }
  )

  tryCatch(
    beepr::beep_on_error(
      df_cells_raw <- df_cells_raw %>%
        dplyr::rename(dplyr::any_of(colname_variants)) %>%
        dplyr::select(tree_code, image_code, all_of(selcols_cells)),
      sound=2
    ),
    error = function(e){
      message("An error occurred subsetting the cells columns\n",
              "check the raw files:\n", e)
    }
  )

  return(setNames(
    list(df_rings_raw, df_cells_raw),
    c('rings', 'cells')))
}

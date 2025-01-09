
# TODO: the txt files have only two digits after decimal point for many variables -> enough precision?
# or should we read form xlsx instead?

#' Read all the (ring or cell) output files and store in large df
#'
#' Helper function
#'
#' @param df_meta Dataframe containing metadata.
#' @param files_colname Name of the column in df_meta with the filenames to read.
#' @param sel_tree_codes List/vector of tree codes to be included.
#' @returns A dataframe with the combined raw data.
#'
read_raw_data <- function(df_meta, files_colname = 'fname_cells'){
  # NOTE: all columns except ID are converted to numeric
  #       all columns from all raw files are included in the output
  df_raw <- df_meta %>%
    dplyr::select(!!dplyr::sym(files_colname), tree_code, image_code) %>%
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

subset_treecodes <- function(df_meta, sel_treecodes) {
  # check for validity
  beepr::beep_on_error(
    checkmate::assert_subset(sel_treecodes, df_meta$tree_code),
    sound=2
  )

  df_meta_filt <- df_meta %>% dplyr::filter(tree_code %in% sel_treecodes)
  return(df_meta_filt)
}


#' Collect the raw ROXAS cell and ring data
#'
#' @param df_meta Dataframe containing metadata.
#' @param subset_treecodes (optional) a list of tree codes to include.
#' @returns A dataframe with the combined raw data.
#'
collect_raw_data <- function(df_meta, subset_treecodes = NULL){

  # read in the raw data
  df_rings_raw <- read_raw_data(df_meta, 'fname_rings')
  df_cells_raw <- read_raw_data(df_meta, 'fname_cells')

  # specify the columns we want, and at same time harmonizing column names of
  # different ROXAS versions
  # TODO: what if some columns are not in data / NA? what if there are others?
  # what about different softwares / image analysis types?
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

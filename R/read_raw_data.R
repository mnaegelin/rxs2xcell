# TODO: the txt files have only two digits after decimal point for many variables -> enough precision?
# or should we read form xlsx instead?
# TODO: make it so we could also import analyses in polar/cal-Cartesian coordinates?


#' Read single cell output file
#'
#' Helper function
#'
#' @param file_cells filename to be read
#' @returns A dataframe with the raw data (relevant columns only).
#'
read_cells_output <- function(file_cells){
  # specify the columns we expect and require in a ROXAS cells output file
  # NOTE: it looks like we should have these columns for ROXAS versions
  # 3.0.285, 3.0.575, 3.0.590, 3.0.608, 3.0.620, 3.0.634, 3.0.655
  selcols_cells <- c(
    'YEAR', 'CID', 'XPIX', 'YPIX', 'RADDISTR', 'RRADDISTR',
    'LA', 'ASP', 'MAJAX', 'KH',
    'CWTPI', 'CWTBA', 'CWTLE', 'CWTRI', 'CWTTAN', 'CWTRAD', 'CWTALL',
    'RTSR', 'CTSR', 'DH', 'DRAD', 'DTAN', 'TB2', 'CWA', 'RWD'
    # not included cols are:
    # ID, RADDIST, ANGLE, XCAL, YCAL (superfluous)
    # TODO
    #'NBRNO', 'NBRID', 'NBRDST' # (relates to groups of cells, relevant for non-conifers)
    # if all read, then  df_cells_all %>% purrr::discard(~all(is.na(.x))) might be interesting
    # AOI (relates to areas of interest)
  )
  # define any variant name mappings from old ROXAS versions
  # use format: current_name = 'old_name', current_name = 'older_name', etc.
  colname_variants <- c(
    TB2 = 'BEND',
    TB2 = 'CRI',
    LA = 'CA'
  )

  # read in the raw data
  # read in the raw data
  # catch errors: print the current filename if there are issues
  tryCatch(
    beepr::beep_on_error(
      df_raw <- readr::read_delim(file_cells, delim = "\t",
                                  col_types = readr::cols(.default="d", ID="c")) %>%
        dplyr::rename(dplyr::any_of(colname_variants)) %>%
        dplyr::select(dplyr::all_of(selcols_cells)),
      sound=2
    ),
    error = function(e){
      message("An error occurred while reading file\n", file_cells, "\n",
              "Check the raw file.\n", e)
    }
  )

  return(df_raw)
}


#' Collect raw cells output data
#'
#' Read and combine data from all cell output files
#'
#' @param df_structure Dataframe containing filenames and data structure.
#' @returns A dataframe with the raw data from all files combined (relevant columns only).
#'
collect_cells_data <- function(df_structure){
  df_cells_all <- df_structure %>%
    dplyr::select(image_code, fname_cells) %>%
    dplyr::mutate(raw_data = purrr::map(fname_cells, read_cells_output)) %>%
    tidyr::unnest(raw_data) %>%
    dplyr::select(-fname_cells)

  # beep successful ending of the function
  message("The raw Roxas cells output files have been read successfully!")
  beepr::beep(sound = 1, expr = NULL)

  return(df_cells_all)
}


#' Read single rings output file
#'
#' Helper function
#'
#' @param file_rings filename to be read
#' @returns A dataframe with the raw data (relevant columns only).
#'
# TODO: should we include the other ring measures?-> PF says no, GvA says yes
read_rings_output <- function(file_rings){
  # specify the columns we expect and require in a ROXAS rings output file
  # NOTE: it looks like we should have these columns for ROXAS versions
  # 3.0.285, 3.0.575, 3.0.590, 3.0.608, 3.0.620, 3.0.634, 3.0.655
  selcols_rings <- c(
    'YEAR', 'MRW', 'CWTTAN'
    # 'CNO' (do not need bc we count cells in anyways, else we would miss the incomplete years)
  )

  # read in the raw data
  # catch errors: print the current filename if there are issues
  tryCatch(
    beepr::beep_on_error(
      df_raw <- readr::read_delim(file_rings, delim = "\t",
                                  col_types = readr::cols(.default="d", ID="c")) %>%
        dplyr::select(dplyr::all_of(selcols_rings)),
      sound=2
    ),
    error = function(e){
      message("An error occurred while reading file\n", file_rings, "\n",
              "Check the raw file.\n", e)
    }
  )

  return(df_raw)
}


#' Collect raw rings output data
#'
#' Read and combine data from all ring output files
#'
#' @param df_structure Dataframe containing filenames and data structure.
#' @returns A dataframe with the raw data from all files combined (relevant columns only).
#'
collect_rings_data <- function(df_structure){
  df_rings_all <- df_structure %>%
    dplyr::select(tree_code, woodpiece_code, slide_code, image_code, fname_rings) %>%
    dplyr::mutate(raw_data = purrr::map(fname_rings, read_rings_output)) %>%
    tidyr::unnest(raw_data) %>%
    dplyr::select(-fname_rings)

  # beep successful ending of the function
  message("The raw Roxas rings output files have been read successfully!")
  beepr::beep(sound = 1, expr = NULL)

  return(df_rings_all)
}


#' Collect raw cells and rings output data
#'
#' Read and combine data from all cells and ring output files
#'
#' @param df_structure Dataframe containing filenames and data structure.
#' @returns QWA_data, i.e. a named list containing the combined raw data
#' for cells and rings in two dataframes under $cells and $rings, respectively.
#' @export
collect_raw_data <- function(df_structure){
  df_cells_all <- collect_cells_data(df_structure)
  df_rings_all <- collect_rings_data(df_structure)

  return(setNames(
    list(df_cells_all, df_rings_all),
    c('cells', 'rings')))
}




################################################################################
## OLD:

#' #' Read all the (ring or cell) output files and store in large df
#' #'
#' #' Helper function
#' #'
#' #' @param df_structure Dataframe containing filenames and data structure.
#' #' @param files_colname Name of the column in df_meta with the filenames to read.
#' #' @param sel_tree_codes List/vector of tree codes to be included.
#' #' @returns A dataframe with the combined raw data.
#' #'
#' read_ROXAS_output <- function(df_structure, files_colname = 'fname_cells'){
#'   # NOTE: all columns except ID are converted to numeric
#'   #       all columns from all raw files are included in the output
#'   df_raw <- df_structure %>%
#'     dplyr::select(!!dplyr::sym(files_colname), tree_code, image_code) %>%
#'     dplyr::mutate(
#'       raw_data = purrr::map(
#'         !!dplyr::sym(files_colname),
#'         ~ readr::read_delim(.x, delim = "\t",
#'                             col_types = readr::cols(.default="d", ID="c")))
#'     ) %>%
#'     tidyr::unnest(raw_data) %>%
#'     dplyr::select(-!!dplyr::sym(files_colname))
#'
#'   # TODO: check id against image_code?
#'
#'   # beep successful ending of the function
#'   message("The raw Roxas ", strsplit(files_colname,'_')[[1]][[2]], " files have been read successfully!")
#'   beepr::beep(sound = 1, expr = NULL)
#'
#'   return(df_raw)
#' }
#'
#'
#' #' Collect the raw ROXAS cell and ring data
#' #'
#' #' @param df_meta Dataframe containing metadata.
#' #' @param subset_treecodes (optional) a list of tree codes to include.
#' #' @returns A dataframe with the combined raw data.
#' #'
#' collect_raw_data <- function(df_meta, subset_treecodes = NULL){
#'
#'   # read in the raw data
#'   df_rings_raw <- read_ROXAS_output(df_meta, 'fname_rings')
#'   df_cells_raw <- read_ROXAS_output(df_meta, 'fname_cells')
#'
#'   # specify the columns we want, and at same time harmonizing column names of
#'   # different ROXAS versions
#'   # TODO: what if some columns are not in data / NA? what if there are others?
#'   # what about different softwares / image analysis types?
#'   # NOTE: it looks like we should have these columns for ROXAS versions
#'   # 3.0.285, 3.0.575, 3.0.590, 3.0.608, 3.0.620, 3.0.634, 3.0.655
#'   selcols_rings <- c(
#'     'YEAR', 'RA', 'MRW', 'CWTTAN'
#'   )
#'   selcols_cells <- c(
#'     'YEAR', 'CID', 'XPIX', 'YPIX', 'RADDISTR', 'RRADDISTR',
#'     'LA', 'ASP', 'MAJAX', 'KH',
#'     'CWTPI', 'CWTBA', 'CWTLE', 'CWTRI', 'CWTTAN', 'CWTRAD', 'CWTALL',
#'     'RTSR', 'CTSR', 'DH', 'DRAD', 'DTAN', 'TB2', 'CWA', 'RWD'
#'     # not included cols are:
#'     # ID, RADDIST, ANGLE, XCAL, YCAL (superfluous)
#'     # NBRNO, NBRID, NBRDST (relates to groups of cells)
#'     # AOI (relates to areas of interest)
#'   )
#'
#'   # define any variant name mappings from old ROXAS versions
#'   # use format: current_name = 'old_name', current_name = 'older_name', etc.
#'   colname_variants <- c(
#'     TB2 = 'BEND',
#'     TB2 = 'CRI',
#'     LA = 'CA'
#'   )
#'
#'   # rename and subset columns, catching errors
#'   tryCatch(
#'     beepr::beep_on_error(
#'       df_rings_raw <- df_rings_raw %>%
#'         dplyr::rename(dplyr::any_of(colname_variants)) %>%
#'         dplyr::select(tree_code, image_code, all_of(selcols_rings)),
#'       sound=2
#'       ),
#'     error = function(e){
#'       message("An error occurred subsetting the rings columns\n",
#'                "check the raw files:\n", e)
#'     }
#'   )
#'
#'   tryCatch(
#'     beepr::beep_on_error(
#'       df_cells_raw <- df_cells_raw %>%
#'         dplyr::rename(dplyr::any_of(colname_variants)) %>%
#'         dplyr::select(tree_code, image_code, all_of(selcols_cells)),
#'       sound=2
#'     ),
#'     error = function(e){
#'       message("An error occurred subsetting the cells columns\n",
#'               "check the raw files:\n", e)
#'     }
#'   )
#'
#'   return(setNames(
#'     list(df_rings_raw, df_cells_raw),
#'     c('rings', 'cells')))
#' }

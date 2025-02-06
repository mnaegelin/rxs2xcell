# setup
# install.packages(c("devtools", "roxygen2", "testthat",
#                    "knitr",'checkmate','beepr','readr',
#                    'dplyr','tidyr','exifr', 'ggplot2','data.table'))

# log
# use_package('checkmate')
# use_package('stringr')
# use_package('magrittr')
# usethis::use_import_from("magrittr", "%>%")
# use_package('beepr')
# use_package('readr')
# use_package('dplyr')
# use_package('tidyr')
# use_package('exifr')
# use_package('ggplot2')
# use_package('ggnewscale')
# use_package('aws.s3') # TODO: really in this package, or seperate?
# usethis::use_data_table()
# (maybe) install.packages("bench")
# (maybe) library(bench)
# (maybe) install.packages("EML")
# (maybe) library(EML)
# (maybe) install.packages("readxl")
# (maybe) library(readxl)

################################################################################
################################################################################

library(devtools)
load_all()

# path to the input data
path_in <- '../QWA_Arzac2024'


################################################################################
# get overview of data to be read
files <- get_input_files(path_in)
df_structure <- extract_data_structure(files)


################################################################################
# optional: subset treecodes
# df_structure <- subset_treecodes(df_structure,
#                                  # EITHER: include_codes=c('treecode1','treecode2')
#                                  # OR: exclude_codes=c('treecode3','treecode4')
#                                  )


################################################################################
# read available metadata
df_meta <- collect_metadata(df_structure,
                            roxas_version='classic')
# TODO: write to file, create template for user input


################################################################################
# read raw cells/rings data
# TODO: add batch processing for large datasets?
QWA_data <- collect_raw_data(df_structure)


################################################################################
# clean raw data
QWA_data <- validate_QWA_data(QWA_data)
QWA_data <- remove_double_rings(QWA_data)
# TODO: write to file
# TODO: do we want to store the rings log in DB? flag or remove issues?

################################################################################
# plot yearly coverage to give overview of data and detected issues
# for a single tree, we can use the plot_tree_coverage function
tree <- 'POG_PISY_02B'
df_tree <- QWA_data$rings %>% dplyr::filter(tree_code == tree)
plot_tree_coverage(tree, df_tree,
                   show_plot = TRUE, save_plot = FALSE)

# to iterate over all trees, we use create_coverage_plots and save the plots to path_out
create_coverage_plots(QWA_data$rings,
                      show_plot = FALSE, save_plot = TRUE, path_out = '.')


################################################################################
# manual exclusions of years based on user input
# either denote years to be excluded here:
years_to_exclude <- tibble::tribble(
  ~image_code, ~YEAR,
  'POG_PISY_02B_4_1', 1962,
  'POG_PISY_02B_4_1', 1964,
  'POG_PISY_02B_4_2', 1961
)

# or read from file
# years_to_exclude <- readr::read_csv('path/to/file/years_to_exclude.csv',
#                                     # skip = 4, # if header rows need to be skipped
#                                     col_select = c('image_code', 'YEAR'),
#                                     col_types = 'cn')
# NOTE: dplyr::coalesce might be useful if there are multiple columns with YEARS

QWA_data <- remove_problem_rings(QWA_data, years_to_exclude)


################################################################################
# remove outliers



################################################################################
# complete cell measures


























# all other processing steps could be voluntary
# so from here could move to data write to db
# TODO: compare calc DH to file DH
# TODO: what about the additional calculated vars?


# flag then clean the cells/rings data
df_rings_flags <- flag_problem_rings(raw_data$rings, raw_data$cells)
# TODO: make it so flags can be viewed then manually changed, then data is cleaned?
# there are different requirements:
# automatically remove things which would lead to errors
# flag issues which may lead to problems, but leave decision up to user
# allow user to provide manual input
# keep a log of what was excluded and why
# maybe we have a removed column right away where we store if (and why) something was removed?

flags_remove <- c(
  no_rings_data = TRUE,
  inv_year = TRUE,
  small_MRW = TRUE,
  no_CWT = TRUE,
  duplicate_fewer_cells = TRUE,
  innermost_year = TRUE
)

clean_data <- clean_raw_data(df_rings_flags, raw_data$cells, flags_remove)

# TODO: create out dir
trees <- unique(raw_data$cells$tree_code)
for (tree in trees) {
  plot_coverage(tree, clean_data$rings, df_meta, path_plots='./')
}

plot_coverage(trees[1], clean_data$rings, df_meta, path_plots='./')

# TODO: iterative steps for cleaning?

# TODO: store data

# EWLW estimation need some of the additional measures (SECTOR100, RRADDISTR.ST, ..?)

# and EWLW needs CWT estimates

# the EW LW calculations can be done in multiple ways and in saxoR, they each
# result in 3 new columns in df_cells
# EWW early wood width and LWW late wood width are the same for the whole ring!!
# EW_LW is "EW" for cells belonging to the early wood part of the ring and "LW"
# for the others, based on the boundary estimated by the resp. method
# so shouldnt these variables rather be stored in the ring df???

# proflie calculation needs an EWLW estimate

# for the profile calculation, a method is selected (with the resp. 3 EWLW cols)
# and then moving averages are calculated along the "time" direction for a specified bandwidth ???
# this results in a smaller df
# so PRF_allDATA$tbl_prf_calc[[1]]$CWTRAD might hold the median and other quantiles of CWTRAD
# over the individual bands slid across the cells



# dgn functions require EWLW, but not profiles (only a band attribution for each cell but not calculated measures)
# they add flags to the diagnostics table, but do not change the cells data

# dgn_Ncells: adds n cells and n bands per ring to diagnostics, add a variable that has
# the count of bands with less than X cells per ring (separate per EW, LW??)
# dgn_ringborder
# dgn_outofffocus





# YTE





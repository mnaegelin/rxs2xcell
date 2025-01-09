# setup
install.packages(c("devtools", "roxygen2", "testthat",
                   "knitr",'checkmate','beepr','readr',
                   'dplyr','tidyr','exifr', 'ggplot2'))


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

# install.packages("bench")
# library(bench)
library(devtools)
load_all()

# path to the input data
path_in <- '../QWA_Arzac2024'
# path_in <- '../saxoRdata/inst/extdata/YAM_LASI_2756'

# define the patterns of ROXAS output files we want
# NOTE: the leading _underscore
# pattern_cell_files = "_Output_Cells\\.txt$"
# pattern_ring_files = "_Output_Rings\\.txt$"
# pattern_settings_files = "_ROXAS_Settings\\.txt$"
# pattern_orgimg_files = "\\.(jpg|jpeg)$" # "\\.(jpg|jpeg|png|gif|bmp|tiff)$"
# imgfiles_exclude_keywords = c("annotated",
#                               "ReferenceSeries",
#                               "Preview")

# read raw metadata
files <- get_input_files(path_in,
                         pattern_cell_files,
                         pattern_ring_files,
                         pattern_settings_files,
                         pattern_orgimg_files,
                         imgfiles_exclude_keywords)

df_structure <- extract_data_structure(files)

df_meta <- collect_metadata(df_structure,
                            roxas_version='classic')

# Read raw data
raw_data <- collect_raw_data(df_meta)

# flag then clean
df_rings_flags <- flag_problem_rings(raw_data$rings, raw_data$cells,
                                     max_val_year = 2500,
                                     min_val_MRW = 10)
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





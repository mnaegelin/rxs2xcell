# setup
install.packages(c("devtools", "roxygen2", "testthat",
                   "knitr",'checkmate','beepr','readr',
                   'dplyr','tidyr','exifr'))

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


# install.packages("bench")
# library(bench)
library(devtools)
load_all()


path_in <- '../QWA_Arzac2024'
path_in <- '../saxoRdata/inst/extdata/YAM_LASI_2756'

# define the patterns of ROXAS output files we want
# NOTE: the leading _underscore
pattern_cell_files = "_Output_Cells\\.txt$"
pattern_ring_files = "_Output_Rings\\.txt$"
pattern_settings_files = "_ROXAS_Settings\\.txt$"
pattern_orgimg_files = "\\.(jpg|jpeg)$" # "\\.(jpg|jpeg|png|gif|bmp|tiff)$"

# Read raw metadata
files <- get_input_files(path_in,
                         pattern_cell_files,
                         pattern_ring_files,
                         pattern_settings_files,
                         pattern_orgimg_files)
df_meta <- collect_metadata(files, pattern_orgimg_files)

# Read raw data
df_raw_rings <- read_raw_data(df_meta, 'rings')
df_raw_cells <- read_raw_data(df_meta, 'cells')

clean_data <- clean_raw_data(df_raw_cells,
                             df_raw_rings,
                             max_val_year = 2500,
                             min_val_MRW = 10)

# create out dir
# create diagnostics file?
# save df_meta?



path_out <- './out'

# Harmonizer: the collect function
# Saxor: sxr_initialise and rxs_read




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



library(devtools)
load_all()


path_in <- '../QWA_Arzac2024'
path_out <- './out'

# Harmonizer: the collect function
# Saxor: sxr_initialise and rxs_read


l_files <- check_path_in_filenames(path_in)

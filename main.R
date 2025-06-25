################################################################################
################################################################################
# WORKFLOW USED FOR DEVELOPMENT ONLY
################################################################################


library(devtools)
load_all()
#remotes::install_github('mnaegelin/rxs2xcell', auth_token = GITHUB_PAT)
#library(rxs2xcell)

################################################################################
# set path to the input and output data

path_in <- '../QWA_Arzac2024'
path_out <- './out'

dataset_name <- 'QWA_Arzac2024' # used to name the resulting output files


################################################################################
# get overview of data to be read and extract data structure from filenames
files <- get_input_files(path_in)
df_structure <- extract_data_structure(files)

# NOTE: At the moment, we assume that all images are named according to the
# following pattern:
#`{site}_{species}_{tree}{woodpiece}_{sample}_{image}`,
# where tree is assumed to be a two-character code, and woodpiece is optional.


################################################################################
# read available metadata
df_meta <- collect_metadata_from_files(df_structure,
                                       roxas_version='classic')



# save extracted metadata to file
# write.csv(df_meta, file.path(path_out, paste0(dataset_name, '_meta_extracted.csv')),
#           row.names = FALSE)


################################################################################
# complete the required metadata form via the Shiny app
launch_metadata_app()




################################################################################
# read raw cells/rings data
# TODO: add batch processing for large datasets?
QWA_data <- collect_raw_data(df_structure)


################################################################################
# clean raw data
# TODO: finalize ring flags
QWA_data <- validate_QWA_data(QWA_data, df_meta)


################################################################################
# remove outliers
# NOTE: ROXAS does some threshold based outlier checks and assigns these a
# a negative value. Here, we replace these outliers with NAs)
QWA_data <- remove_outliers(QWA_data)


################################################################################
# complete cell measures
QWA_data <- complete_cell_measures(QWA_data)


################################################################################
# provide user input on ring flags
# interactively in shiny app
launch_coverage_app()

# after running the app, reload the rings data
filepath <- file.choose()
QWA_data$rings <- tibble::as_tibble(read.csv(
  filepath,
  stringsAsFactors = FALSE))
# TODO: check formatting on import?



################################################################################
# save preprocessed data to files
write.csv(QWA_data$cells,
          file.path(path_out, paste0(dataset_name, '_cells.csv')),
          row.names = FALSE)

write.csv(QWA_data$rings,
          file.path(path_out, paste0(dataset_name, '_rings.csv')),
          row.names = FALSE)





################################################################################
################################################################################
################################################################################
# TODO: put some of the data into inst/extdata, usable for testing and examples
# TODO: add tests



# TODO:
# - only_ew variable from min_cell_area? plus user input?














# VARIANT B: ring flags in script / manually -----------------------------------
# TODO: update plot function and manual input, workflow with changes in csv
# use create_coverage_plots and to create and save the plots of the yearly
# coverage and issues (generates one plot per woodpiece)
# create_coverage_plots(QWA_data$rings,
#                       save_plot = TRUE, path_out = path_out)

# can also use the plot_tree_coverage function to plot the coverage of a
# specific woodpiece and show here
# woodpiece <- 'POG_PISY_02_B'
# df_wp <- QWA_data$rings %>% dplyr::filter(woodpiece_code == woodpiece)
# covplot <- plot_woodpiece_coverage(woodpiece, df_wp)
# print(covplot$p)

# after inspecting the plots, and given their own knowledge from the data
# generation process, the user can provide input on which rings to flag
# years_to_flag <- tibble::tribble(
#   ~image_code, ~YEAR,
#   # for example:
#   # 'POG_PISY_02B_4_1', 1962,
#   # 'POG_PISY_02B_4_1', 1964,
#   # 'POG_PISY_02B_4_2', 1961
# )
# QWA_data <- add_user_flags(QWA_data, years_to_flag)

# QWA_data$rings <- QWA_data$rings %>%
#   dplyr::mutate(
#     duplicate_sel = dplyr::if_else(duplicate_rank == 1, TRUE, FALSE),
#     other_issues = FALSE,
#     other_reason = NA_character_
#   )


# complete_QWA_data <- list()
# complete_QWA_data$cells <- QWA_data$cells # read.csv ...







# optional: if we want to exclude certain woodpieces or include only a subset
# of woodpieces, we can do so here
# df_structure <- subset_woodpiece_codes(df_structure,
#                                        # EITHER: include_codes = c('POG_PISY_02_B'), OR
#                                        exclude_codes = c('POG_PISY_02_B'))



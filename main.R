################################################################################
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

# TODO:
# - only_ew variable from min_cell_area? plus user input?
# - infer species -> wood_type from filenames? what if code not available?

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






# df <- table_configs$site_tbl |>
#   purrr::map(
#     \(sub) purrr::map(sub, \(i) if (length(i) == 1) list(i) else i)
#   ) |>
#    dplyr::bind_rows() |>
#   tidyr::unnest(cols = c(col_header, col_header_tippy, type, readOnly, required, options, min_val, max_val)) |>
#   dplyr::group_by(col_header) |>
#   dplyr::mutate(options = list(options)) |>
#   dplyr::ungroup() |>
#   dplyr::distinct() |>
#   dplyr::mutate(attr_name = names(table_configs$site_tbl)) |>
#   dplyr::select(-col_header_tippy,-readOnly, -required)













################################################################################
################################################################################
################################################################################
# TODO: put some of the data into inst/extdata, usable for testing and examples
# TODO: remove the explorations/*, out/* and this main.R file from the package
# TODO: add tests
# TODO: add template workflow?
# TODO: writing to DB
complete_metadata <- jsonlite::fromJSON("./inst/shiny_meta/data.json", flatten = T)
# QWA_data <- list()
# QWA_data$cells <- read.csv("./out/QWA_Arzac2024_cells.csv")
# QWA_data$rings <- read.csv("./out/QWA_Arzac2024_rings.csv")


dbcon2 <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                        dbname = "xcell",
                        host = "pgdbxcell.wsl.ch", # old: pgdbtapp.wsl.ch or pgdboapp.wsl.ch
                        port = 5432,
                        user = "xcell_edit", #naegelin
                        password = keyring::key_get("pgdbt_xcell", username = "xcell_edit")) #naegelin
schema <- 'v3'

write_data_to_db(complete_metadata,
                 QWA_data,
                 dbcon, schema)

DBI::dbDisconnect(dbcon)


################################################################################
# TODO: writing to S3
# path <- "QWA_Arzac2024" # or doi?
# for (img in files$images) {
# aws.s3::put_object(file = img, object = paste0(path, img_basename),
#                    bucket = "s3://xcell-public/", region = "")
# }
# put the json with metadata and raw ring/cells csv into bucket as well?
# aws.s3::put_object(file = "test.png", object = "/testA/test.png",
#                    bucket = "s3://xcell-public/", region = "")

















# VARIANT B: in script / manually ----------------------------------------------
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

# profile calculation needs an EWLW estimate

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





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

#path_in <- '../QWA_Arzac2024'
path_in <- '/Users/maranaegelin/Documents/QWAdata/YAM_1880' # for testing
path_out <- './out'

dataset_name <- 'QWA_YAM1880' # used to name the resulting output files


################################################################################
# get overview of data to be read and extract data structure from filenames
files <- get_input_files(path_in)

#pattern <- "^(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:][:alnum:])(?<woodpiece>[:alnum:]*)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)$"
pattern <- "^(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:].+)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)$"

df_structure <- extract_data_structure(files, pattern)

# NOTE: At the moment, we assume that all images are named according to the
# following pattern:
#`{site}_{species}_{tree}{woodpiece}_{sample}_{image}`,r
# where site, species, slide and image are alphanumerical, tree is assumed to be a two-character code, and woodpiece is optional, or
#`{site}_{species}_{tree}}_{sample}_{image}`, where
# site, species, slide and image are alphanumerical, tree may also contain a '.'.



################################################################################
# read available metadata
df_meta <- collect_metadata_from_files(df_structure,
                                       roxas_version='classic')



# save extracted metadata to file
# write.csv(df_meta, file.path(path_out, paste0(dataset_name, '_meta_extracted.csv')),
#           row.names = FALSE)


################################################################################
# complete the required metadata form via the Shiny app
launch_metadata_app(example_run = FALSE)




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
QWA_data <- read_QWAdata(file_path = './out', dataset_name = dataset_name)


# ADDING THE YTE INFO
yte <- readxl::read_excel(
  '/Users/maranaegelin/Documents/QWAdata/YAM_1880/Years_to_exclude.xlsx',
  skip = 5,
  col_names = TRUE,
  .name_repair = janitor::make_clean_names
)
yte <- yte %>%
  dplyr::rename(image_code = id) %>%
  dplyr::mutate(dplyr::across(c(year_to_exclude,x_dating:others), \(x) as.integer(gsub("\\.", "", x))),
                other_issues = dplyr::if_else(is.na(year_to_exclude),T,F)) %>%
  dplyr::filter(image_code != 'YAM_LASI_125_01_3') %>% tidyr::pivot_longer( # this img code has no year for any flags?
    cols = c(x_dating:others),
    names_to = 'other_reason',
    values_to = 'year', values_drop_na = TRUE
  )

yte[yte$image_code == 'YAM_LASI_243_4_3',"other_reason"] <- 'decay, paraffin' # has two flags
yte <- yte[!duplicated(yte[c('image_code','year')]),] # remove this and other duplicates (not in ds)

QWA_data$rings <- QWA_data$rings %>% dplyr::left_join(yte %>% dplyr::select(image_code,year,other_issues, other_reason), #what about tissue, comment?
                                    by = c('image_code','year')) %>% dplyr::mutate(other_issues = dplyr::if_else(is.na(other_issues), FALSE, other_issues))



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


x<-list.files('/Users/maranaegelin/Documents/QWAdata/YAM_1880', full.names = TRUE)
y <- x[sapply(x, file.size) <1]


uncopied_names <- sub("^.*/([^/]+)_ROXAS_Settings\\.txt$", "\\1", y)
settings_only <- grepl("_Output_Cells\\.txt$", y)
uncopied_names <- uncopied_names[settings_only]


gt_names_c <- c('site_id' = 'Site ID', 'site_code' = 'Site code', 'year' = 'Calendar year', 'site_label' = 'Site label', 'manip' = 'Manipulation', 'country_code' = 'Country code', 'longitude' = 'Longitude', 'latitude' = 'Latitude', 'elevation' = 'Elevation', 'temp' = 'Temperature [C]', 'prec' = 'Precipitation [mm]', 'species_code' = 'Species code', 'wood_type' = 'Wood type', 'leaf_habit' = 'Leaf habit', 'wood_plane' = 'Wood plane', 'organ' = 'Measured organ', 'output' = 'Output', 'hardware' = 'Hardware', 'software' = 'Software', 'last_name' = 'Contributor last name', 'first_name' = 'Contributor first name', 'email' = 'Contributor E-mail', 'institution_code' = 'Institution code', 'n_trees' = 'Number of trees', 'n_radii' = 'Number of radii', 'n_rings' = 'Number of rings', 'n_obs' = 'Number of observations', 'from' = 'Minimum calendar year', 'to' = 'Maximum calendar year', 'ring_width' = 'Ring width [micron]', 'la' = 'Lumen area [micron2]', 'ldrad' = 'Radial lumen diameter [micron]', 'ldtan' = 'Tangential lumen diameter [micron]', 'cwtrad' = 'Thickness of radial cell wall [micron]', 'cwttan' = 'Thickness of tangential cell wall [micron]', 'cwa' = 'Cell wall area [micron2]')

gt_named_c <- gt_names_c[c('longitude', 'latitude', 'elevation', 'temp', 'prec', 'ring_width', 'la', 'ldrad', 'ldtan', 'cwtrad', 'cwttan', 'cwa')] %>%
  tibble::enframe() %>% dplyr::select(value, name) %>% tibble::deframe()

df_cells <- data.frame(fullname = files_cells)
df_cells <- df_cells %>% dplyr::mutate(
  imgcode = gsub(pattern_cell_files, "",basename(fullname)))
df_rings <- data.frame(fullname = files_rings)
df_rings <- df_rings %>% dplyr::mutate(imgcode = gsub(pattern_ring_files, "",basename(fullname)))
df_settings <- data.frame(fullname = files_settings)
df_settings <- df_settings %>% dplyr::mutate(imgcode = gsub(pattern_settings_files, "",basename(fullname)))
df_images <- data.frame(fullname = files_images)
df_images <- df_images %>% dplyr::mutate(imgcode = gsub(pattern_orgimg_files, "",basename(fullname)))
df_images <- df_images %>% dplyr::full_join(df_cells %>% dplyr::select(imgcode), by = 'imgcode')
df_settings <- df_settings %>% dplyr::full_join(df_images %>% dplyr::select(imgcode), by = 'imgcode')
df_cells <- df_cells %>% dplyr::mutate(fsize = file.size(fullname))
df_rings <- df_rings %>% dplyr::mutate(fsize = file.size(fullname))
df_settings <- df_settings %>% dplyr::mutate(fsize = file.size(fullname))
df_images <- df_images %>% dplyr::mutate(fsize = file.size(fullname))

all_yamal_files <- list.files("/Volumes/Dendro/Dendrosciences_All/PatrickFonti_CALDERA_2019_PF/YAM_1880")
all_yamal_files <- data.frame(fname = all_yamal_files)
pattern <- "YAM_LASI_[A-Za-z0-9.]+_[0-9]{1,2}_[0-9]{1,2}"
all_yamal_files$imgcode <- stringr::str_extract(all_yamal_files$fname, pattern)
all_yamal_files <- all_yamal_files %>% dplyr::filter(!is.na(imgcode)) # filters out the years to exlude
fcounts <- all_yamal_files %>%
  dplyr::group_by(imgcode) %>% dplyr::count()
all_yamal_files$filetype <- stringr::str_remove(all_yamal_files$fname, pattern)
all_yamal_files$wanted <- all_yamal_files$filetype %in% c("_annotated.jpg", "_Output_Cells.txt", "_Output_Rings.txt", "_ROXAS_Settings.txt", ".jpg", ".jpeg")
fcounts2 <- all_yamal_files %>% dplyr::filter(wanted) %>%
  dplyr::group_by(imgcode) %>% dplyr::count()

trees_box1 <- unique(stringr::str_extract(list.files("/Volumes/Dendro/Dendrosciences_All/PatrickFonti_CALDERA_2019_PF/Box1-30/Box01/ROXAS/4_Roxas_final"), pattern))
unique_trees_box1 <- unique(stringr::str_split_fixed(trees_box1[3:length(trees_box1)],"_",4)[,3])


trees_box2 <- unique(stringr::str_extract(list.files("/Volumes/Dendro/Dendrosciences_All/PatrickFonti_CALDERA_2019_PF/Box1-30/Box02/ROXAS/4_Roxas_final"), pattern))
unique_trees_box2 <- unique(stringr::str_split_fixed(trees_box2[2:length(trees_box2)],"_",4)[,3])

trees_box3 <- unique(stringr::str_extract(list.files("/Volumes/Dendro/Dendrosciences_All/PatrickFonti_CALDERA_2019_PF/Box1-30/Box03/ROXAS/4_Roxas_final"), pattern))
unique_trees_box3 <- unique(stringr::str_split_fixed(trees_box3[2:length(trees_box3)],"_",4)[,3])


trees_box4 <- unique(stringr::str_extract(list.files("/Volumes/Dendro/Dendrosciences_All/PatrickFonti_CALDERA_2019_PF/Box1-30/Box04/ROXAS/4_Roxas_final"), pattern))
unique_trees_box4 <- unique(stringr::str_split_fixed(trees_box4[2:length(trees_box4)],"_",4)[,3])

trees_selection <- unique(stringr::str_extract(list.files("/Users/maranaegelin/Documents/QWAdata/YAM_1880"), pattern))
unique_trees_sel <- unique(stringr::str_split_fixed(trees_selection,"_",4)[,3])

# get country ISO codes
#countries_list <- unname(get_country_codes())

# Custom JS code for cell rendering
# renderer_no_dupl <- "
#       function(instance, td, row, col, prop, value, cellProperties) {
#         Handsontable.renderers.TextRenderer.apply(this, arguments);
#         var data = instance.getDataAtCol(col);
#         var duplicates = data.filter(function(val, index, arr) {
#           return arr.indexOf(val) !== index && val === value;
#         });
#         if (duplicates.length > 0) {
#           td.style.background = 'lightcoral';
#         }
#       }"

site_tbl_str <- data.frame(
  sitecode = character(0),
  n_tree = integer(0),
  sitename = character(0),
  sitedesc = character(0),
  latitude = numeric(0),
  longitude = numeric(0),
  country = character(0),
  elevation = numeric(0),
  aspect = numeric(0),
  slope = numeric(0),
  topography = character(0),
  soildepth = character(0),
  soilwater_capacity = character(0),
  stand_type = character(0),
  stand_structure = character(0),
  stand_age = character(0),
  stand_main_species = character(0),
  stand_management = character(0),
  stringsAsFactors = FALSE
)

site_tbl_config <- list(
# NOTE: order matters for colHeaders (needs to be same as in df)
  colHeaders = c(
    sitecode = "Site code", n_tree = "Nr of trees", sitename = "Site name",
    sitedesc = "Site description", latitude = "Latitude", longitude = "Longitude",
    country = "Country", elevation = "Elevation", aspect = "Aspect", slope = "Slope",
    topography = "Topography", soildepth = "Soil depth",
    soilwater_capacity = "Soil water holding capacity",
    stand_type = "Forest stand type", stand_structure = "Stand structure",
    stand_age = "Stand age", stand_main_species = "Stand main species",
    stand_management = "Stand management intensity"
  ),
  sitecode = list(type = 'character', readOnly = TRUE),
  n_tree = list(type = 'numeric', readOnly = TRUE),
  sitename = list(type = 'character', required = TRUE, unique = TRUE),
  sitedesc = list(type = 'character', required = FALSE),
  latitude = list(type = 'numeric', required = TRUE, min_val = -90, max_val = 90),
  longitude = list(type = 'numeric', required = TRUE, min_val = -180, max_val = 180),
  country = list(type = 'autocomplete', required = FALSE, options = names(countries_list)), # calculated automatically
  elevation = list(type = 'numeric', required = TRUE, min_val = -420, max_val = 8848),
  aspect = list(type = 'numeric', required = FALSE, min_val = 0, max_val = 360),
  slope = list(type = 'numeric', required = FALSE, min_val = 0, max_val = 90),
  topography = list(type = 'dropdown', required = FALSE,
                    options = c('summit','plateau', 'upper slope', 'middle slope',
                                'lower slope', 'valley bottom', 'flat')),
  soildepth = list(type = 'dropdown', required = FALSE,
                   options = c("0-100 cm", "100-200 cm", ">200 cm")),
  soilwater_capacity = list(type = 'dropdown', required = FALSE,
                             options = c("low", "medium", "high")),
  stand_type = list(type = 'dropdown', required = FALSE,
                    options = c('mono gymnosperm', 'mixed gymnosperm',
                                'mixed forest', 'mixed angiosperm', 'mono angiosperm')),
  stand_structure = list(type = 'dropdown', required = FALSE,
                    options = c('regular', 'irregular', 'old growth')),
  stand_age = list(type = 'dropdown', required = FALSE, options = c('evenaged', 'unevenaged')),
  stand_main_species = list(type = 'autocomplete', required = FALSE,
                            options = species_info$itrdb_species_code),
  stand_management = list(type = 'dropdown', required = FALSE,
                          options = c('unmanaged', 'extensive', 'moderate', 'instensive'))
)

site_tbl <- list(
  tbl_str = site_tbl_str,
  tbl_config = site_tbl_config
)

tree_tbl_str <- data.frame(
  treecode = character(0),
  sitecode = character(0),
  n_wp = integer(0),
  speciescode = character(0),
  speciesname = character(0),
  wood_type = character(0),
  leaf_habit = character(0),
  tree_ring_structure = character(0),
  tree_treatment = character(0),
  tree_dbh = numeric(0),
  tree_height = numeric(0),
  tree_age = numeric(0),
  tree_sex = character(0),
  tree_social_status = character(0),
  tree_health_status = character(0),
  tree_origin = character(0),
  tree_comment = character(0),
  stringsAsFactors = FALSE
)

tree_tbl_config <- list(
  # NOTE: order matters for colHeaders (needs to be same as in df)
  colHeaders = c(treecode = "Tree code", sitecode = "Site code",
                 n_wp = "Nr of pieces", speciescode = "Species code", speciesname = 'Species',
                 wood_type = "Wood type", leaf_habit = "Leaf habit",
                 tree_ring_structure = "Treering structure", tree_treatment = "Treatment",
                 tree_dbh = "Diameter at breast height", tree_height = "Total height",
                 tree_age = "Age", tree_sex = "Sex", tree_social_status = "Social status",
                 tree_health_status = "Health condition", tree_origin = "Origin",
                 tree_comment = "Comments"
                 ),
  treecode = list(type = 'character', readOnly = TRUE),
  sitecode = list(type = 'character', readOnly = TRUE),
  n_wp = list(type = 'numeric', readOnly = TRUE),
  speciescode = list(type = 'autocomplete', required = TRUE, options = species_info$itrdb_species_code),
  speciesname = list(type = 'autocomplete', required = TRUE, options = species_info$tree_species),
  wood_type = list(type = 'dropdown', required = FALSE, options = c('gymnosperm','angiosperm')),
  leaf_habit = list(type = 'dropdown', required = FALSE, options = c('evergreen','deciduous')),
  tree_ring_structure = list(type = 'dropdown', required = FALSE, options = c('conifer','diffuse-porous','ring-porous')),
  tree_treatment = list(type = 'dropdown', required = FALSE,
                        options = c('none', 'pruning', 'thinning', 'water exclusion', 'watering', 'heating', 'cooling')),
  tree_dbh = list(type = 'numeric', required = FALSE, min_val = 0, max_val = 500),
  tree_height = list(type = 'numeric', required = FALSE, min_val = 0, max_val = 120),
  tree_age = list(type = 'numeric', required = FALSE, min_val = 0, max_val = 5000),
  tree_sex = list(type = 'dropdown', required = FALSE,
                  options = c('male','female','both','unknown')),
  tree_social_status = list(type = 'dropdown', required = FALSE,
                             options = c('dominant','co-dominant','suppressed','dying')),
  tree_health_status = list(type = 'dropdown', required = FALSE,
                            options = c('healthy','declining','dying','dead')),
  tree_origin = list(type = 'dropdown', required = FALSE,
                     options = c('planted', 'natural regeneration', 'potted',
                                 'coppiced', 'grafted')),
  tree_comment = list(type = 'character', required = FALSE)
)

tree_tbl <- list(
  tbl_str = tree_tbl_str,
  tbl_config = tree_tbl_config
)

wp_tbl_str <- data.frame(
  wpcode = character(0),
  treecode = character(0),
  n_slides = integer(0),
  sample_date = character(0),
  organ = character(0),
  sampling_height = numeric(0),
  sample_apex_distance = numeric(0),
  sample_prep = character(0),
  sample_type = character(0),
  pith_year = numeric(0),
  stringsAsFactors = FALSE
)

wp_tbl_config <- list(
  colHeaders = c(
    wpcode = "Woodpiece code", treecode = "Tree code", n_slides = "Nr of slides",
    sample_date = "Sample date", organ = "Organ", sampling_height = "Sampling height",
    sample_apex_distance = "Distance from apex", sample_prep = "Sample preparation",
    sample_type = "Sample type", pith_year = "Pith year"
  ),
  wpcode = list(type = 'character', readOnly = TRUE),
  treecode = list(type = 'character', readOnly = TRUE),
  n_slides = list(type = 'numeric', readOnly = TRUE),
  sample_date = list(type = 'date', required = TRUE),
  organ = list(type = 'dropdown', required = TRUE,
               options = c('branch','root','stem')),
  sampling_height = list(type = 'numeric', required = FALSE, min_val = 0, max_val = 150),
  sample_apex_distance = list(type = 'numeric', required = FALSE, min_val = 0, max_val = 150),
  sample_prep = list(type = 'dropdown', required = FALSE,
                      options = c('micro-section', 'cut surface', 'polished surface', 'double saw cut')),
  sample_type = list(type = 'dropdown', required = FALSE,
                      options = c('core 5 mm', 'core 10 mm', 'wood section', 'wood disk')),
  pith_year = list(type = 'numeric', required = FALSE, min_val = -15000, max_val = 2500)
)

woodpiece_tbl <- list(
  tbl_str = wp_tbl_str,
  tbl_config = wp_tbl_config
)

slide_tbl_str <- data.frame(
  slidecode = character(0),
  wpcode = character(0),
  n_imgs = integer(0),
  sample_date = character(0),
  sect_thickness = character(0),
  cuttingplane = character(0),
  slide_prep = character(0),
  slide_staining = character(0),
  img_cap_system = character(0),
  data_structure = character(0),
  stringsAsFactors = FALSE
)

# TODO: FIX
slide_tbl_config <- list(
  colHeaders = c(
    slidecode = "Slide code", wpcode = "Woodpiece code", n_imgs = "Nr of images",
    sample_date = "Sample date", sect_thickness = "Section thickness",
    cuttingplane = "Cutting plane", slide_prep = "Slide preparation",
    slide_staining = "Staining method", img_cap_system = "Image capturing system",
    data_structure = "Data structure"
  ),
  slidecode = list(type = 'character', readOnly = TRUE),
  wpcode = list(type = 'character', readOnly = TRUE),
  n_imgs = list(type = 'numeric', readOnly = TRUE),
  sample_date = list(type = 'date', required = TRUE),
  sect_thickness = list(type = 'dropdown', required = FALSE,
                        options = c('0-10 µm','10-20 µm','20-30 µm','30-40 µm','40-50 µm')),
  cuttingplane = list(type = 'dropdown', required = FALSE,
                      options = c('transversal','longitudinal')),
  slide_prep = list(type = 'dropdown', required = FALSE,
                    options = c('none','double saw cut','micro-section')),
  slide_staining = list(type = 'dropdown', required = FALSE,
                        options = c('none','stain','fluorescent stain')),
  img_cap_system = list(type = 'dropdown', required = FALSE,
                        options = c('none','camera','microscope')),
  data_structure= list(type='dropdown', required=FALSE,
                       options=c('raw data','processed data'))
)

slide_tbl <- list(
  tbl_str = slide_tbl_str,
  tbl_config = NULL
)


# SERVER -----------------------------------------------------------------------
site_server <- function(id, main_session, start_info, countries_list, site_tbl, tree_tbl, woodpiece_tbl, slide_tbl) {
  moduleServer(id, function(input, output, session) {

    # mock event to close map tab on start of app
    observeEvent(1,{
      accordion_panel_close(id = 'map_acc', values = TRUE)
    }, ignoreNULL = FALSE) # to fire the event at startup


    # initialize reactiveVals (responding to changes in df_meta, file upload)
    site_data_in <- reactiveVal(NULL)
    tree_data_in <- reactiveVal(NULL)
    wp_data_in <- reactiveVal(NULL)
    slide_data_in <- reactiveVal(NULL)

    # observe changes in df_meta to (re-)initialize site_data_in
    # NOTE: this purposefully overwrites/resets any updates via hot edit or
    # file upload if the underlying df_meta is changed
    observeEvent(start_info$input_meta$df, {
      df_meta <- start_info$input_meta$df

      # site
      df_site <- site_tbl$tbl_str
      df_site <- df_meta %>%
        dplyr::group_by(site) %>%
        dplyr::summarise(n = dplyr::n_distinct(tree_code)) %>%
        dplyr::rename(sitecode = site, n_tree = n) %>%
        dplyr::left_join(df_site, by = c('sitecode', 'n_tree'))

      site_data_in(df_site)

      # tree
      df_tree <- tree_tbl$tbl_str
      df_tree <- df_meta %>%
        dplyr::group_by(site, species, tree_code) %>%
        dplyr::summarise(n = dplyr::n_distinct(woodpiece_code), .groups = 'keep') %>%
        dplyr::rename(treecode = tree_code, sitecode = site, speciescode = species, n_wp = n) %>%
        dplyr::left_join(df_tree, by = c('treecode', 'sitecode', 'speciescode', 'n_wp')) %>%
        dplyr::select(colnames(df_tree))
      # add species information
      df_tree <- df_tree %>%
        dplyr::left_join(species_info, by = c('speciescode' = 'itrdb_species_code'), suffix = c("",".lookup")) %>%
        dplyr::mutate(speciesname = tree_species,
                      wood_type = wood_type.lookup,
                      leaf_habit = leaf_habit.lookup,
                      tree_ring_structure = tree_ring_structure.lookup) %>%
        dplyr::select(-tree_species, -dplyr::ends_with(".lookup"))

      tree_data_in(df_tree)

      # woodpiece
      df_wp <- woodpiece_tbl$tbl_str
      df_wp <- df_meta %>%
        dplyr::group_by(tree_code, woodpiece_code) %>%
        dplyr::summarise(n = dplyr::n_distinct(slide_code), .groups = 'keep') %>%
        dplyr::rename(wpcode = woodpiece_code, treecode = tree_code, n_slides = n) %>%
        dplyr::left_join(df_wp, by = c('wpcode', 'treecode', 'n_slides')) %>%
        dplyr::select(colnames(df_wp))

      wp_data_in(df_wp)

    })

    # TODO: observe file upload button: merge with site_data_out and update site_data_in
    # observeEvent(input$file_sites, {
    #   # try to load the file
    #   imported_data <- tryCatch({
    #     read.csv(input$file_sites$datapath, stringsAsFactors = FALSE, encoding = 'UTF-8')
    #   }, error = function(e) {
    #     showModal(modalDialog(
    #       title = "Error importing file",
    #       paste("An error occurred while reading the file:", e$message),
    #       easyClose = TRUE,
    #       footer = NULL
    #     ))
    #     return(NULL)
    #   })
    #   # try to convert data to right structure
    #   converted_data <- tryCatch({
    #     align_to_structure(site_tbl_str, imported_data)
    #   }, error = function(e) {
    #     showModal(modalDialog(
    #       title = "Error loading data",
    #       paste("Data could not be aligned with required structure:", e$message),
    #       easyClose = TRUE,
    #       footer = NULL
    #     ))
    #     return(NULL)
    #   })
    #   # updated input data, report any missing columns
    #   site_data$df_in <- converted_data$data
    #   if (length(converted_data$missing_cols) > 0) {
    #     showNotification(
    #       paste("Missing columns filled with NA:",
    #             paste(converted_data$missing_cols, collapse = ", ")),
    #       type = "message")
    #   }
    # })


    slide_data <- reactiveValues(
      df_in = slide_tbl_str,
      df_out = NULL
    )

    # get initial site info from df_meta
    # TODO: warn that manual input here will be lost if df_meta is changed!
    observe({
      df_meta <- start_info$input_meta$df
      if (!is.null(df_meta)) {

        # SLIDE
        df_in_sl <- slide_data$df_in
        slide_imgs <- df_meta %>%
          dplyr::group_by(woodpiece_code, slide_code) %>%
          dplyr::summarise(n = dplyr::n_distinct(image_code), .groups = 'keep') %>%
          dplyr::rename(slidecode = slide_code, wpcode = woodpiece_code, n_imgs = n)
        slide_data$df_in <- df_in_sl %>%
          dplyr::right_join(slide_imgs, by = c('slidecode', 'wpcode', 'n_imgs'))
      }
    })


    # SITE MAP -----------------------------------------------------------------
    # render Leaflet map
    output$site_map <- leaflet::renderLeaflet({
      # start map on WSL coordinates
      sitemap <- leaflet::leaflet() %>%
        leaflet::setView(lng = 8.44256, lat = 47.35515, zoom = 8) %>%
        leaflet::addTiles()

      # add markers for all sites in the table

      site_coords <- site_coordinates()
      # if we have both lat and longitude, add markers and zoom to new bounds
      if (nrow(site_coords)>0) {
        sitemap <- sitemap %>% leaflet::addMarkers(lng = site_coords$longitude, lat = site_coords$latitude) %>%
          leaflet::fitBounds(lng1 = min(site_coords$longitude)-5, lng2 = max(site_coords$longitude)+5,
                             lat1 = min(site_coords$latitude)-5, lat2 = max(site_coords$latitude)+5)
      }

      sitemap

    })

    # site coordinates reactiveVal, update ONLY if coordinates in site_data_out changes
    site_coordinates <- reactiveVal(NULL)

    observeEvent(site_data_out(),{
      # valid range from site tbl config
      min_lng <- site_tbl$tbl_config[['longitude']]$min_val
      max_lng <- site_tbl$tbl_config[['longitude']]$max_val
      min_lat <- site_tbl$tbl_config[['latitude']]$min_val
      max_lat <- site_tbl$tbl_config[['latitude']]$max_val

      # get valid pairs of coordinates from table
      site_coords <- site_data_out()
      site_coords <- site_coords %>%
        dplyr::select(longitude, latitude) %>%
        dplyr::mutate(
          longitude = suppressWarnings(as.numeric(longitude)),
          latitude = suppressWarnings(as.numeric(latitude))
        ) %>%
        dplyr::mutate(
          longitude = ifelse(longitude < min_lng | longitude > max_lng, NA, longitude),
          latitude = ifelse(latitude < min_lat | latitude > max_lat, NA, latitude)
        ) %>%
        dplyr::filter(!is.na(longitude) & !is.na(latitude))

      # update reactive
      site_coordinates(site_coords)
    }, ignoreInit = TRUE)


    #tree_species <- reactiveVal(NULL)

    # update species info in tree table IFF speciesname or code are changed
    observeEvent(input$tree_table,{

      # TODO: loop through multiple changes at once
      table_changes <- input$tree_table$changes$changes
      current_df <- tree_data_out()
      update_tree_data <- FALSE

      for (k in length(table_changes)){
        row <- table_changes[[k]][[1]] # first index can be more than 1
        col <- table_changes[[k]][[2]]
        new_val <- table_changes[[k]][[4]]

        # col 3 is speciescode (0-index)
        if (!is.null(col) && col == 3) {
          # Look species name, woodtype and leaf habit of new species code
          new_species_info <- species_info[species_info$itrdb_species_code == new_val,]
          if (nrow(new_species_info) > 0) {
            current_df[row + 1, col + 2] <- new_species_info$tree_species
            current_df[row + 1, col + 3] <- new_species_info$wood_type
            current_df[row + 1, col + 4] <- new_species_info$leaf_habit
            current_df[row + 1, col + 5] <- new_species_info$tree_ring_structure
            update_tree_data <- TRUE
          }
        }
        # col 4 is speciesname (0-index)
        if (!is.null(col) && col == 4) {
          # Look species name, woodtype and leaf habit of new species code
          new_species_info <- species_info[species_info$tree_species == new_val,]
          if (nrow(new_species_info) > 0) {
            current_df[row + 1, col] <- new_species_info$itrdb_species_code
            current_df[row + 1, col + 2] <- new_species_info$wood_type
            current_df[row + 1, col + 3] <- new_species_info$leaf_habit
            current_df[row + 1, col + 4] <- new_species_info$tree_ring_structure
            update_tree_data <- TRUE
          }
        }
      }

      if (update_tree_data) {
        tree_data_in(current_df)
      }

    }, ignoreInit = TRUE)

    # TODO: maybe update countries automatically? but is expensive -> only at end?
    # use either tidygeocoder (api request, slow) or sf package (downlaod data first via rnaturalearth)
    # site_countries <- reactiveVal(NULL)
    #
    # observeEvent(site_data_out(),{
    #   # # get unique country codes from site data
    #   # site_countries <- site_data_out() %>%
    #   #   dplyr::select(country) %>%
    #   #   dplyr::distinct() %>%
    #   #   dplyr::filter(!is.na(country)) %>%
    #   #   dplyr::pull(country)
    #   #
    #   # # update reactive
    #   # site_countries(site_countries)
    # }, ignoreInit = TRUE)



    output$testing <- renderPrint({
      #input$site_table$changes$changes
      input$tree_table$changes$changes
    })

    # render editable table
    output$site_table <- rhandsontable::renderRHandsontable({
      validate(need(!is.null(site_data_in()), "No data to show"))

      rhandsontable::rhandsontable(
        site_data_in(), #site_data$df_in,
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        #overflow = "visible",
        height = 180,
        colHeaders = unname(site_tbl$tbl_config$colHeaders)) %>%
        #rhandsontable::hot_cols(fixedColumnsLeft = 1) %>%
        # custom validation checks renderers for all cols based on tbl_config
        purrr::reduce(
          names(site_tbl$tbl_config$colHeaders), # names in df
          function(ht, col) {
            config <- site_tbl$tbl_config[[col]]
            colName <- site_tbl$tbl_config$colHeaders[col] # name in ht
            hot_col_wrapper(ht, colName, config)
          },
          .init = .
        )
    })

    # create dataframe reactive to hot updates
    site_data_out <- reactive({
      rhandsontable::hot_to_r(input$site_table)
    })





    output$tree_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        tree_data_in(),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        height = 180,
        colHeaders = unname(tree_tbl$tbl_config$colHeaders)) %>%
        #rhandsontable::hot_cols(fixedColumnsLeft = 1) %>%
        # custom validation checks renderers for all cols based on tbl_config
        purrr::reduce(
          names(tree_tbl_config$colHeaders), # names in df
          function(ht, col) {
            config <- tree_tbl$tbl_config[[col]]
            colName <- tree_tbl$tbl_config$colHeaders[col] # name in ht
            hot_col_wrapper(ht, colName, config)
          },
          .init = .
        )
    })

    # create dataframe reactive to hot updates
    tree_data_out <- reactive({
      rhandsontable::hot_to_r(input$tree_table)
    })

    output$wp_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        wp_data_in(),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        colHeaders = unname(woodpiece_tbl$tbl_config$colHeaders)) %>%
        purrr::reduce(
          names(woodpiece_tbl$tbl_config$colHeaders), # names in df
          function(ht, col) {
            config <- woodpiece_tbl$tbl_config[[col]]
            colName <- woodpiece_tbl$tbl_config$colHeaders[col] # name in ht
            hot_col_wrapper(ht, colName, config)
          },
          .init = .
        )
        # rhandsontable::hot_col(
        #   'sampledate',
        #   renderer = renderer_date(required = FALSE),
        #   type = 'date',
        #   dateFormat = "YYYY-MM-DD"
        # )

    })

    # create dataframe reactive to hot updates
    wp_data_out <- reactive({
      rhandsontable::hot_to_r(input$twp_table)
    })

    output$slide_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        slide_data$df_in,
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all")
    })





    # observeEvent(input$wp_table, {
    #   wp_data$df_out <- rhandsontable::hot_to_r(input$wp_table)
    # })
    observeEvent(input$slide_table, {
      slide_data$df_out <- rhandsontable::hot_to_r(input$slide_table)
    })


    validation_results <- reactiveValues()

    # Validation checks --------------------------------------------------------
    # TODO:
    output$validation_check <- renderUI({
      #req...

      #validation_results <- list()

      # use the iv_gen object to validate the dataset inputs

      # column-wise validation checks on the author table
      # df_site <- site_data$df_in
      # validation_results$site_table <- lapply(colnames(df_site), function(col_name) {
      #   validate_column(df_site[[col_name]], site_tbl_config[[col_name]])
      # })
      # names(validation_results$site_table) <- colnames(df_site)
      #
      # df_tree <- tree_data$df_in
      # validation_results$tree_table <- lapply(colnames(df_tree), function(col_name) {
      #   validate_column(df_tree[[col_name]], tree_tbl_config[[col_name]])
      # })
      # names(validation_results$tree_table) <- colnames(df_tree)
      #
      #
      # # Use a for loop to iterate through validation_results
      # issues_output <- list()
      # for (input_type in names(validation_results)){
      #   issue_list <- list()
      #   for (input_name in names(validation_results[[input_type]])) {
      #     issues <- validation_results[[input_type]][[input_name]]
      #     # If there are issues for the current column, add them to the issue list
      #     if (length(issues) > 0) {
      #       issue_list <- tagList(issue_list, tags$li(paste0(input_name, ": ", paste(issues, collapse = ", "))))
      #     }
      #   }
      #   if (length(issue_list) > 0) {
      #     issues_output <- tagList(issues_output,
      #                              tagList(paste0("Issues with ", input_type, " input:"), tags$ul(issue_list))
      #     )
      #   }
      # }
      #
      # # set the color of the header based on the validation results
      # shinyjs::toggleClass(id = "val_check_header", class = 'bg-secondary',
      #                      condition = length(issues_output) > 0)
      #
      # # TODO:
      # # add warning messages before switching tab or saving data
      #
      # # show output message
      # if (length(issues_output) > 0) {
      #   tagList(issues_output)
      # } else {
      #   tagList(strong("ALL GOOD :)", style = paste0('color: ', prim_col, ';')))
      # }

    })


    # Next button
    observeEvent(input$btn_next, {
      #iv_gen$enable()
      nav_select(id = 'tabs', selected = tab_tree, session = main_session)
    })

    # Previous button
    observeEvent(input$btn_prev, {
      #iv_gen$enable()
      nav_select(id = 'tabs', selected = tab_general, session = main_session)
    })



    return(
        list(
          input_meta = list(
            site_data = site_data_out,
            tree_data = tree_data_out,
            wp_data = wp_data_out
            #slide_data = slide_data$df_out
            )
          # ,
          # val_check = validation_results
      )
    )

  })
}



# observe({
#   df_meta <- start_info$input_meta$df
#   if (!is.null(df_meta)) {
#     # # SITE
    # df_in_si <- site_data$df_in
    # # get unique site codes and nr of trees per site
    # sites_trees <- df_meta %>%
    #   dplyr::group_by(site) %>%
    #   dplyr::summarise(n = dplyr::n_distinct(tree_code)) %>%
    #   dplyr::rename(sitecode = site, n_tree = n)
    # # add to site df and update reactive
    # site_data$df_in <- df_in_si%>%
    #   dplyr::right_join(sites_trees, by = c('sitecode', 'n_tree'))

#     # TREE
#     df_in_tr <- tree_data$df_in
#     # get unique tree codes and nr of wps per site
#     trees_wps <- df_meta %>%
#       dplyr::group_by(site, species, tree_code) %>%
#       dplyr::summarise(n = dplyr::n_distinct(woodpiece_code), .groups = 'keep') %>%
#       dplyr::rename(treecode = tree_code, sitecode = site, speciescode = species, n_wp = n)
#     # add to tree df and update reactive
#     tree_data$df_in <- df_in_tr %>%
#       dplyr::right_join(trees_wps, by = c('treecode', 'sitecode', 'speciescode', 'n_wp'))
#
#     # WOODPIECE
#     df_in_wp <- wp_data$df_in
#     wp_slides <- df_meta %>%
#       dplyr::group_by(tree_code, woodpiece_code) %>%
#       dplyr::summarise(n = dplyr::n_distinct(slide_code), .groups = 'keep') %>%
#       dplyr::rename(wpcode = woodpiece_code, treecode = tree_code, n_slides = n)
#     wp_data$df_in <- df_in_wp %>%
#       dplyr::right_join(wp_slides, by = c('wpcode', 'treecode', 'n_slides'))
#
#     # SLIDE
#     df_in_sl <- slide_data$df_in
#     slide_imgs <- df_meta %>%
#       dplyr::group_by(woodpiece_code, slide_code) %>%
#       dplyr::summarise(n = dplyr::n_distinct(image_code), .groups = 'keep') %>%
#       dplyr::rename(slidecode = slide_code, wpcode = woodpiece_code, n_imgs = n)
#     slide_data$df_in <- df_in_sl %>%
#       dplyr::right_join(slide_imgs, by = c('slidecode', 'wpcode', 'n_imgs'))
#   }
# })


# site_data <- reactiveVal(value = NULL)
# observeEvent({
#   input$site_table
#   prefilled_meta()},{
#     if (is.null(input$site_table))  {
#       df_site <- data.frame(
#         sitecode = character(0),
#         sitename = character(0),
#         country = character(0),
#         latitude = numeric(0),
#         longitude = numeric(0),
#         n_tree = integer(0),
#         stringsAsFactors = FALSE
#       )
#       if (!is.null(prefilled_meta()$df)){
#         df_meta <- prefilled_meta()$df
#         # get unique site codes and nr of trees per site
#         sites_trees <- df_meta %>%
#           dplyr::group_by(site) %>%
#           dplyr::summarise(n = dplyr::n_distinct(tree_code)) %>%
#           dplyr::rename(sitecode = site, n_tree = n)
#         # add to site df and update reactive
#         df_site <- df_site %>%
#           dplyr::right_join(sites_trees, by = c('sitecode', 'n_tree'))
#       }
#       df_site$country <- factor(rep(NA_character_,nrow(df_site)),
#                                 levels = countries_list, ordered = TRUE)
#       isolate(site_data(df_site))
#     }
#     # else {
#     #   df_site <- rhandsontable::hot_to_r(input$site_table)
#     #   output$testing <- renderPrint({
#     #     df_site
#     #   })
#     #   if (!is.numeric(df_site$latitude)) {
#     #     isolate(dfOld <- site_data())
#     #     isolate(site_data(dfOld))
#     #   } else {isolate(site_data(df_site)) }
#     # }
#
#   })


#
# observeEvent(prefilled_meta(), {
#   df_meta <- prefilled_meta()$df
#   df_site <- site_data()
#   if (!is.null(df_meta)) {
#     # get unique site codes and nr of trees per site
#     sites_trees <- df_meta %>%
#       dplyr::group_by(site) %>%
#       dplyr::summarise(n = dplyr::n_distinct(tree_code)) %>%
#       dplyr::rename(sitecode = site, n_tree = n)
#     # add to site df and update reactive
#     df_site <- df_site %>%
#       dplyr::right_join(sites_trees, by = c('sitecode', 'n_tree'))
#     # initialize
#     df_site$country <- factor(rep(NA_character_,nrow(df_site)),
#                               levels = countries_list, ordered = TRUE)
#     site_data(df_site)
#   }
# })

# generate_unique_codes <- function(n, length) {
#   unique_codes <- character(0)
#   while(length(unique_codes) < n) {
#     new_codes <- replicate(n, paste0(sample(c(LETTERS, 0:9), length, replace = TRUE), collapse = ""))
#     unique_codes <- unique(c(unique_codes, new_codes))
#     unique_codes <- unique_codes[1:n]  # Ensure we have exactly n unique codes
#   }
#   unique_codes
# }
#
# # Generate 250 unique character codes of length 5
# unique_codes <- generate_unique_codes(250, 5)
#
# countries_list <- unname(get_country_codes())

# DF = data.frame(integer = 1:10,
#                 numeric = rnorm(10),
#                 logical = rep(TRUE, 10),
#                 character = LETTERS[1:10],
#                 factor = factor(letters[1:10], levels = letters[10:1], ordered = TRUE),
#                 factor_allow = factor(rep(NA_character_,10),levels = countries_list, ordered = TRUE),
#                 date = seq(from = Sys.Date(), by = "days", length.out = 10),
#                 stringsAsFactors = FALSE)




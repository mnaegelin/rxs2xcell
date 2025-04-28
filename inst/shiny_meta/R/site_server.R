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
  sitename = character(0),
  country = factor(character(0)),
  latitude = numeric(0),
  longitude = numeric(0),
  sitedesc = character(0),
  soildepth = character(0),
  n_tree = integer(0),
  stringsAsFactors = FALSE
)

site_tbl_config <- list(
# NOTE: order matters for colHeaders (needs to be same as in df)
  colHeaders = c(sitecode = "Site Code", sitename = "Site Name",
    country = "Country", latitude = "Latitude", longitude = "Longitude",
    sitedesc = "Site Description", soildepth = "Soil Depth", n_tree = "Nr of Trees"
  ),
  sitecode = list(type = 'character', readOnly = TRUE),
  sitename = list(type = 'character', required = TRUE, unique = TRUE),
  country = list(type = 'autocomplete', required = TRUE, options = names(countries_list)),
  latitude = list(type = 'numeric', required = TRUE, min_val = -90, max_val = 90),
  longitude = list(type = 'numeric', required = TRUE, min_val = -180, max_val = 180),
  sitedesc = list(type = 'character', required = FALSE),
  soildepth = list(type = 'dropdown', required = FALSE,
                   options = c("0-100 cm", "100-200 cm", ">200 cm")),
  n_tree = list(type = 'numeric', readOnly = TRUE)
)

tree_tbl_str <- data.frame(
  treecode = character(0),
  sitecode = character(0),
  speciescode = character(0),
  speciesname = character(0),
  woodtype = character(0),
  leafhabit = character(0),
  ring_structure = character(0),
  tree_treatment = character(0),
  treedesc = character(0),
  n_wp = integer(0),
  stringsAsFactors = FALSE
)

tree_tbl_config <- list(
  # NOTE: order matters for colHeaders (needs to be same as in df)
  colHeaders = c(treecode = "Tree Code", sitecode = "Site Code",
                 speciescode = "Species Code", speciesname = 'Species',
                 woodtype = "wood type", leafhabit = "leaf habit",
                 ring_structure = "ring structure",
                 tree_treatment = "treatment", treedesc = "description",
                 n_wp = "Nr of Pieces"),
  treecode = list(type = 'character', readOnly = TRUE),
  sitecode = list(type = 'character', readOnly = TRUE),
  speciescode = list(type = 'autocomplete', required = TRUE, options = species_info$itrdb_species_code),
  speciesname = list(type = 'autocomplete', required = TRUE, options = species_info$tree_species),
  woodtype = list(type = 'dropdown', required = FALSE, options = unique(species_info$wood_type)),
  leafhabit = list(type = 'dropdown', required = FALSE, options = c("evergreen", "deciduous")),
  ring_structure = list(type = 'dropdown', required = FALSE, options = c("conifer", "diffuse-porous", 'ring-porous')),
  tree_treatment = list(type = 'dropdown', required = FALSE,
                        options = c("control", "treatment")),
  treedesc = list(type = 'character', required = FALSE),
  n_wp = list(type = 'numeric', readOnly = TRUE)
)

wp_tbl_str <- data.frame(
  wpcode = character(0),
  treecode = character(0),
  sampledate = character(0),
  organ = character(0),
  sampling_height = numeric(0),
  dist_to_tip = numeric(0),
  sample_prep = character(0),
  sample_type = character(0),
  pith_year = numeric(0),
  n_slides = integer(0),
  stringsAsFactors = FALSE
)

slide_tbl_str <- data.frame(
  slidecode = character(0),
  wpcode = character(0),
  sampledate = character(0),
  sect_thickness = character(0),
  cuttingplane = character(0),
  img_cap_system = character(0),
  data_structure = character(0),
  n_imgs = integer(0),
  stringsAsFactors = FALSE
)


# SERVER -----------------------------------------------------------------------
site_server <- function(id, main_session, start_info) {
  moduleServer(id, function(input, output, session) {

    # mock event to close map tab on start of app
    observeEvent(1,{
      accordion_panel_close(id = 'map_acc', values = TRUE)
    }, ignoreNULL = FALSE) # to fire the event at startup

    # initialize reactives
    site_data <- reactiveValues(
      df_in = site_tbl_str,
      df_out = NULL
    )
    tree_data <- reactiveValues(
      df_in = tree_tbl_str,
      df_out = NULL
    )
    wp_data <- reactiveValues(
      df_in = wp_tbl_str,
      df_out = NULL
    )
    slide_data <- reactiveValues(
      df_in = slide_tbl_str,
      df_out = NULL
    )

    # get initial site info from df_meta
    # TODO: warn that manual input here will be lost if df_meta is changed!
    observe({
      df_meta <- start_info$input_meta$df
      if (!is.null(df_meta)) {
        # SITE
        df_in_si <- site_data$df_in
        # get unique site codes and nr of trees per site
        sites_trees <- df_meta %>%
          dplyr::group_by(site) %>%
          dplyr::summarise(n = dplyr::n_distinct(tree_code)) %>%
          dplyr::rename(sitecode = site, n_tree = n)
        # add to site df and update reactive
        site_data$df_in <- df_in_si%>%
          dplyr::right_join(sites_trees, by = c('sitecode', 'n_tree'))

        # TREE
        df_in_tr <- tree_data$df_in
        # get unique tree codes and nr of wps per site
        trees_wps <- df_meta %>%
          dplyr::group_by(site, species, tree_code) %>%
          dplyr::summarise(n = dplyr::n_distinct(woodpiece_code), .groups = 'keep') %>%
          dplyr::rename(treecode = tree_code, sitecode = site, speciescode = species, n_wp = n)
        # add to tree df and update reactive
        tree_data$df_in <- df_in_tr %>%
          dplyr::right_join(trees_wps, by = c('treecode', 'sitecode', 'speciescode', 'n_wp'))

        # WOODPIECE
        df_in_wp <- wp_data$df_in
        wp_slides <- df_meta %>%
          dplyr::group_by(tree_code, woodpiece_code) %>%
          dplyr::summarise(n = dplyr::n_distinct(slide_code), .groups = 'keep') %>%
          dplyr::rename(wpcode = woodpiece_code, treecode = tree_code, n_slides = n)
        wp_data$df_in <- df_in_wp %>%
          dplyr::right_join(wp_slides, by = c('wpcode', 'treecode', 'n_slides'))

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


    # SITE RHANDSONTABLE -------------------------------------------------------
    # render Leaflet map
    output$site_map <- leaflet::renderLeaflet({

      # TODO: check pairs are kept with na omit
      lng <- site_data$df_out$longitude %>% na.omit()
      lat <- site_data$df_out$latitude %>% na.omit()

      # start map on WSL coordinates
      sitemap <- leaflet::leaflet() %>%
        leaflet::setView(lng = 8.44256, lat = 47.35515, zoom = 8) %>%
        leaflet::addTiles()

      # if we have both lat and longitude, add markers and zoom to new bounds
      if (length(lng) > 0 & length(lat) > 0 & length(lng) == length(lat)) {
        sitemap <- sitemap %>% leaflet::addMarkers(lng = lng, lat = lat) %>%
          leaflet::fitBounds(lng1 = min(lng)-5, lng2 = max(lng)+5,
                             lat1 = min(lat)-5, lat2 = max(lat)+5)
      }

      sitemap

    })

    # render editable table
    output$site_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        site_data$df_in,
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        #overflow = "visible",
        height = 150,
        colHeaders = unname(site_tbl_config$colHeaders)) %>%
        #rhandsontable::hot_cols(fixedColumnsLeft = 1) %>%
        # custom validation checks renderers for all cols based on tbl_config
        purrr::reduce(
          names(site_tbl_config$colHeaders), # names in df
          function(ht, col) {
            config <- site_tbl_config[[col]]
            colName <- site_tbl_config$colHeaders[col] # name in ht
            hot_col_wrapper(ht, colName, config)
          },
          .init = .
        )
    })

    # message if no data
    output$noHTmeta <- renderPrint({
      if (nrow(site_data$df_in)<1) {
        "no site data to show"
      }
    })

    # update data frame on table edit
    observeEvent(input$site_table, {
      site_data$df_in <- rhandsontable::hot_to_r(input$site_table)
    })

    # observe import button
    # TODO: finalize
    observeEvent(input$file_sites, {
      # try to load the file
      imported_data <- tryCatch({
        read.csv(input$file_sites$datapath, stringsAsFactors = FALSE, encoding = 'UTF-8')
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error importing file",
          paste("An error occurred while reading the file:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      })
      # try to convert data to right structure
      converted_data <- tryCatch({
        align_to_structure(site_tbl_str, imported_data)
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error loading data",
          paste("Data could not be aligned with required structure:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      })
      # updated input data, report any missing columns
      site_data$df_in <- converted_data$data
      if (length(converted_data$missing_cols) > 0) {
        showNotification(
          paste("Missing columns filled with NA:",
                paste(converted_data$missing_cols, collapse = ", ")),
          type = "message")
      }
    })


    output$tree_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        tree_data$df_in,
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        height = 150,
        colHeaders = unname(tree_tbl_config$colHeaders)) %>%
        #rhandsontable::hot_cols(fixedColumnsLeft = 1) %>%
        # custom validation checks renderers for all cols based on tbl_config
        purrr::reduce(
          names(tree_tbl_config$colHeaders), # names in df
          function(ht, col) {
            config <- tree_tbl_config[[col]]
            colName <- tree_tbl_config$colHeaders[col] # name in ht
            hot_col_wrapper(ht, colName, config)
          },
          .init = .
        )
    })

    output$wp_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        wp_data$df_in,
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all") %>%
        rhandsontable::hot_col(
          'sampledate',
          renderer = renderer_date(required = FALSE),
          type = 'date',
          dateFormat = "YYYY-MM-DD"
        )

    })

    output$slide_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        slide_data$df_in,
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all")
    })

    # Update data frame on table edit
    observeEvent(input$tree_table, {
      tree_data$df_in <- rhandsontable::hot_to_r(input$tree_table)

      table_changes <- input$tree_table$changes$changes
      row <- table_changes[[1]][[1]]
      col <- table_changes[[1]][[2]]
      new_val <- table_changes[[1]][[4]]

      if (!is.null(col) && col == 2) {
        tree_data$df_in[row + 1, col + 2] <- new_val # LOOKUP NEW SPECIES
      }
    # }, ignoreInit = TRUE, ignoreNULL = TRUE

    })

    output$testing <- renderPrint({
      #input$tree_table$changes$changes
      site_data$df_out
    })



    observeEvent(input$wp_table, {
      wp_data$df_out <- rhandsontable::hot_to_r(input$wp_table)
    })
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
      reactive(
        list(
          input_meta = list(
            site_data = site_data$df_out,
            tree_data = tree_data$df_in,
            wp_data = wp_data$df_out,
            slide_data = slide_data$df_out
            )
          # ,
          # val_check = validation_results
        )
      )
    )

  })
}


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




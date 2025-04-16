# get country ISO codes
countries_list <- unname(get_country_codes())

site_tbl_str <- data.frame(
  sitecode = character(0),
  sitename = character(0),
  country = factor(character(0), levels = countries_list),
  latitude = numeric(0),
  longitude = numeric(0),
  n_tree = integer(0),
  sitedesc = character(0),
  soildepth = character(0),
  stringsAsFactors = FALSE
)

site_tbl_config <- list(
# NOTE: order matters for colHeaders (needs to be same as in df)
  colHeaders = c(sitecode = "Site Code", sitename = "Site Name",
    country = "Country", latitude = "Latitude", longitude = "Longitude",
    n_tree = "Nr of Trees", sitedesc = "Site Description", soildepth = "Soil Depth"
  ),
  sitecode = list(type = 'character', readOnly = TRUE),
  sitename = list(type = 'character', required = TRUE, unique = TRUE),
  country = list(type = 'autocomplete', required = TRUE, options = countries_list),
  latitude = list(type = 'numeric', required = TRUE, min = -90, max = 90),
  longitude = list(type = 'numeric', required = TRUE, min = -180, max = 180),
  n_tree = list(type = 'numeric', readOnly = TRUE),
  sitedesc = list(type = 'character', required = FALSE),
  soildepth = list(type = 'dropdown', required = FALSE,
                   options = c("0-100 cm", "100-200 cm", ">200 cm"))
)

# Custom JS code for cell rendering
renderer_no_dupl <- "
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        var data = instance.getDataAtCol(col);
        var duplicates = data.filter(function(val, index, arr) {
          return arr.indexOf(val) !== index && val === value;
        });
        if (duplicates.length > 0) {
          td.style.background = 'lightcoral';
        }
      }"


site_server <- function(id, main_session, prefilled_meta) {
  moduleServer(id, function(input, output, session) {

    # initialize site_data reactives
    site_data <- reactiveValues(
      df_in = site_tbl_str,
      df_out = NULL
    )

    # get initial site info from df_meta
    # TODO: warn that manual input here will be lost if df_meta is changed!
    observeEvent(prefilled_meta(), {
      if (!is.null(prefilled_meta()$df)) {
        df_meta <- prefilled_meta()$df
        df_in <- site_data$df_in
        # get unique site codes and nr of trees per site
        sites_trees <- df_meta %>%
          dplyr::group_by(site) %>%
          dplyr::summarise(n = dplyr::n_distinct(tree_code)) %>%
          dplyr::rename(sitecode = site, n_tree = n)
        # add to site df and update reactive
        site_data$df_in <- df_in %>%
          dplyr::right_join(sites_trees, by = c('sitecode', 'n_tree'))
      }
    })


    # Render editable table
    output$site_table <- rhandsontable::renderRHandsontable({
      # rhandsontable::rhandsontable(
      #   site_data$df_in,
      #   rowHeaders = TRUE,
      #   #stretchH = "all",
      #   height = '150',
      #   overflow = "visible",
      #   colHeaders = c("Site Code", "Site Name", "Country", "Latitude",
      #                  "Longitude", "Nr of Trees", "Site Description")
      #   ) %>%
      # rhandsontable::hot_col("Site Code", readOnly = TRUE) %>%
      # rhandsontable::hot_col("Site Name", renderer = renderer_no_dupl) %>%
      # rhandsontable::hot_col("Country", allowInvalid = TRUE) %>%
      # rhandsontable::hot_validate_numeric('Latitude', min = -90, max = 90) %>%
      # rhandsontable::hot_validate_numeric('Longitude', min = -180, max = 180) %>%
      # rhandsontable::hot_col('Nr of Trees', readOnly = TRUE) %>%
      # rhandsontable::hot_cols(fixedColumnsLeft = 1)

      rhandsontable::rhandsontable(
        site_data$df_in,
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        #overflow = "visible",
        height = 300,
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


    # Update data frame on table edit
    observeEvent(input$site_table, {
      site_data$df_out <- rhandsontable::hot_to_r(input$site_table)
    })

    # Observe import button
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


    # VALIDATION CHECKS
    # TODO:


    # Observe save data button
    # TODO:



    # Render Leaflet map when file is uploaded
    output$site_map <- leaflet::renderLeaflet({

      lng <- site_data$df_out$longitude %>% na.omit()
      lat <- site_data$df_out$latitude %>% na.omit()

      sitemap <- leaflet::leaflet() %>%
        leaflet::setView(lng = 8.44256, lat = 47.35515, zoom = 8) %>%
        leaflet::addTiles()

      if (length(lng) > 0 & length(lat) > 0 & length(lng) == length(lat)) {
        sitemap <- sitemap %>% leaflet::addMarkers(lng = lng, lat = lat) %>%
          leaflet::fitBounds(lng1 = min(lng)-5, lng2 = max(lng)+5,
                             lat1 = min(lat)-5, lat2 = max(lat)+5)
      }

      sitemap

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


    output$testing <- renderPrint({
      site_data$df_out$longitude
    })

    return(
      reactive(
        site_data$df_out
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




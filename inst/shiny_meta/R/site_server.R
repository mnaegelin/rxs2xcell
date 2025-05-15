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



# SERVER -----------------------------------------------------------------------
site_server <- function(id, main_session, start_info, countries_list, site_tbl, tree_tbl, woodpiece_tbl, slide_tbl) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # mock event to close map tab on start of app
    observeEvent(1,{
      accordion_panel_close(id = 'map_acc', values = TRUE)
    }, ignoreNULL = FALSE) # to fire the event at startup


    # initialize reactiveVals (responding to changes in df_meta, file upload)
    site_data_in <- reactiveVal(NULL)
    tree_data_in <- reactiveVal(NULL)
    wp_data_in <- reactiveVal(NULL)
    slide_data_in <- reactiveVal(NULL)

    # observe changes in df_meta to (re-)initialize these dataframes
    # NOTE: this purposefully overwrites/resets any updates via hot edit or
    # file upload if the underlying df_meta is changed
    observeEvent(start_info$input_meta$df, {
      df_meta <- start_info$input_meta$df

      # site
      df_site <- create_empty_tbl(site_tbl, nrows=0)
      df_site <- df_meta %>%
        dplyr::group_by(site) %>%
        dplyr::summarise(n = dplyr::n_distinct(tree_code)) %>%
        dplyr::rename(site_code = site, n_trees = n) %>%
        dplyr::left_join(df_site, by = c('site_code', 'n_trees'))

      site_data_in(df_site)

      # tree
      df_tree <- create_empty_tbl(tree_tbl, nrows=0)
      df_tree <- df_meta %>%
        dplyr::group_by(site, species, tree_code) %>%
        dplyr::summarise(n = dplyr::n_distinct(woodpiece_code), .groups = 'keep') %>%
        dplyr::rename(site_code = site, species_code = species, n_woodpieces = n) %>%
        dplyr::left_join(df_tree, by = c('tree_code', 'site_code', 'species_code', 'n_woodpieces')) %>%
        dplyr::select(colnames(df_tree))
      # add species information
      df_tree <- df_tree %>%
        dplyr::left_join(species_info, by = c('species_code' = 'itrdb_species_code'), suffix = c("",".lookup")) %>%
        dplyr::mutate(species_name = species_name.lookup,
                      wood_type = wood_type.lookup,
                      leaf_habit = leaf_habit.lookup,
                      tree_ring_structure = tree_ring_structure.lookup) %>%
        dplyr::select(-dplyr::ends_with(".lookup"))

      tree_data_in(df_tree)

      # woodpiece
      df_wp <- create_empty_tbl(woodpiece_tbl, nrows=0)
      df_wp <- df_meta %>%
        dplyr::group_by(tree_code, woodpiece_code) %>%
        dplyr::summarise(n = dplyr::n_distinct(slide_code), .groups = 'keep') %>%
        dplyr::rename(n_slides = n) %>%
        dplyr::left_join(df_wp, by = c('woodpiece_code', 'tree_code', 'n_slides')) %>%
        dplyr::select(colnames(df_wp))

      wp_data_in(df_wp)

      # slide
      df_slide <- create_empty_tbl(slide_tbl, nrows=0)
      df_slide <- df_meta %>%
        dplyr::group_by(woodpiece_code, slide_code) %>%
        dplyr::summarise(n = dplyr::n_distinct(image_code), .groups = 'keep') %>%
        dplyr::rename(n_imgs = n) %>%
        dplyr::left_join(df_slide, by = c('slide_code', 'woodpiece_code', 'n_imgs')) %>%
        dplyr::select(colnames(df_slide))

      slide_data_in(df_slide)
    })

    # TODO: observe file upload button: merge file input with site_data_out and update site_data_in
    # add only info for valid sitecodes / colnames?
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


    # SITE MAP -----------------------------------------------------------------
    # site coordinates reactiveVal, updates IFF coord cols in site_data_out change
    site_coordinates <- reactiveVal(NULL)

    observeEvent(site_data_out(),{
      # valid range from site tbl config
      min_lng <- site_tbl$longitude$min_val
      max_lng <- site_tbl$longitude$max_val
      min_lat <- site_tbl$latitude$min_val
      max_lat <- site_tbl$latitude$max_val

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
        dplyr::filter(!is.na(longitude) & !is.na(latitude)) %>%
        dplyr::distinct()

      # update reactive
      site_coordinates(site_coords)
    }, ignoreInit = TRUE)

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




    # SITE TABLE ---------------------------------------------------------------
    # render editable table
    output$site_table <- rhandsontable::renderRHandsontable({
      validate(need(!is.null(site_data_in()), "No data to show"))

      colHeaders <- get_tbl_colHeaders(site_tbl)
      tippies <- get_header_tippies(site_tbl)

      rhandsontable::rhandsontable(
        site_data_in(), #site_data$df_in,
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        #overflow = "visible",
        height = 180,
        colHeaders = unname(colHeaders),
        afterGetColHeader = tippy_renderer(tippies)) %>%
        #rhandsontable::hot_cols(fixedColumnsLeft = 1) %>%
        # custom validation checks renderers for all cols based on tbl_config
        purrr::reduce(
          names(colHeaders), # names in df
          function(ht, col) {
            config <- site_tbl[[col]]
            colName <- colHeaders[col] # name in ht
            hot_col_wrapper(ht, colName, config)
          },
          .init = .
        )
    })

    # create dataframe reactive to hot updates
    site_data_out <- reactive({
      rhandsontable::hot_to_r(input$site_table)
    })

    # update country column in site data based on coordinates
    observeEvent(site_coordinates(),{
      site_coords <- site_coordinates()
      if (nrow(site_coords) > 0) {
        current_df <- site_data_out()
        site_coords$iso_codes <- country_from_coords(lng = site_coords$longitude, lat = site_coords$latitude, countries_sf)
        site_coords$new_country <- ifelse(
          is.na(site_coords$iso_codes) | site_coords$iso_codes == "-99",
          "",  # Assign empty string for NA values
          names(countries_list)[match(site_coords$iso_codes, countries_list)]
        )

        # update country column in site data
        current_df <- current_df %>% dplyr::left_join(site_coords, by = c('longitude', 'latitude')) %>%
          dplyr::mutate(country = new_country) %>%
          dplyr::select(-iso_codes, -new_country)
        site_data_in(current_df)
      }

    }, ignoreInit = TRUE)


    # networks
    observeEvent(input$btn_add_nws, {
      site_codes <- site_data_out() %>% dplyr::pull(site_code)
      showModal(modalDialog(
        title = "Add site network",
        tagList(
          textInput(ns("nw_name"), "Network name", value = NA,
                    placeholder = "Specify a name for the network (max 64 char.)"),
          textAreaInput(ns("nw_desc"), "Network description", rows = 4,
                    placeholder = "Provide a brief description of what characterises the site network."),
          selectizeInput(
            inputId = ns("sel_sites"),
            label = "Choose the sites:",
            choices = site_codes,
            multiple = TRUE)
        ),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("btn_trans_nw"), "Add")
        )
      ))
    })

    nw_data_in <- reactiveVal(data.frame(
      Name = character(0),
      Description = character(0),
      Sites = character(0),
      stringsAsFactors = FALSE
    ))

    observeEvent(input$btn_trans_nw, {
      # check if all fields are filled
      validate(need(input$nw_name != "", "Please provide a name for the network"))
      validate(need(input$nw_desc != "", "Please provide a description for the network"))
      validate(need(length(input$sel_sites) > 0, "Please select at least one site"))

      # create new row
      new_row <- data.frame(
        Name = input$nw_name,
        Description = input$nw_desc,
        Sites = paste(input$sel_sites, collapse = ", "),
        stringsAsFactors = FALSE
      )

      # add to existing data
      current_df <- nw_data_in()
      current_df[nrow(current_df)+1,] <- new_row
      nw_data_in(current_df)

      removeModal()
    })

    # TODO:
    # add validator rules for dataset inputs
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("nw_name", shinyvalidate::sv_required())
    iv$add_rule("nw_name", max_char_limit, limit = 64)
    # iv$add_rule(
    #   "ds_name",
    #   shinyvalidate::sv_regex("^[a-zA-Z0-9]*$", "Only alphanumeric characters allowed")
    # )
    iv$add_rule("nw_desc", shinyvalidate::sv_required())
    iv$add_rule("sel_sites", shinyvalidate::sv_required())

    iv$enable() # TODO: enable from start?

    shiny::observe({
      shinyjs::toggleState(id = "btn_trans_nw",
                           condition = iv$is_valid())
    })

    output$networks <- DT::renderDataTable({
      req(nrow(nw_data_in()) > 0)

      deleteButtonColumn(nw_data_in(), 'delbtn', ns)

    })

    # observe delete row events
    observeEvent(input$deletePressed, {
      rowNum <- parseDeleteEvent(input$deletePressed)
      # Delete the row from the data frame
      current_df <- nw_data_in()
      current_df <- current_df[-rowNum,]
      if (nrow(current_df) > 0) {
        rownames(current_df) <- 1:nrow(current_df) # update rownames
      }
      nw_data_in(current_df)
    })


    # TREE TABLE ---------------------------------------------------------------
    output$tree_table <- rhandsontable::renderRHandsontable({
      validate(need(!is.null(tree_data_in()), "No data to show"))

      colHeaders <- get_tbl_colHeaders(tree_tbl)
      tippies <- get_header_tippies(tree_tbl)

      rhandsontable::rhandsontable(
        tree_data_in(),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        height = 180,
        colHeaders = unname(colHeaders),
        afterGetColHeader = tippy_renderer(tippies)) %>%
        #rhandsontable::hot_cols(fixedColumnsLeft = 1) %>%
        # custom validation checks renderers for all cols based on tbl_config
        purrr::reduce(
          names(colHeaders), # names in df
          function(ht, col) {
            config <- tree_tbl[[col]]
            colName <- colHeaders[col] # name in ht
            hot_col_wrapper(ht, colName, config)
          },
          .init = .
        )
    })

    # create dataframe reactive to hot updates
    tree_data_out <- reactive({
      rhandsontable::hot_to_r(input$tree_table)
    })

    # TODO: for simultaneous updates
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
            current_df[row + 1, col + 2] <- new_species_info$species_name
            current_df[row + 1, col + 3] <- new_species_info$wood_type
            current_df[row + 1, col + 4] <- new_species_info$leaf_habit
            current_df[row + 1, col + 5] <- new_species_info$tree_ring_structure
            update_tree_data <- TRUE
          }
        }
        # col 4 is speciesname (0-index)
        if (!is.null(col) && col == 4) {
          # Look species name, woodtype and leaf habit of new species code
          new_species_info <- species_info[species_info$species_name == new_val,]
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


    # WOODPIECE TABLE ----------------------------------------------------------
    output$wp_table <- rhandsontable::renderRHandsontable({
      validate(need(!is.null(wp_data_in()), "No data to show"))

      colHeaders <- get_tbl_colHeaders(woodpiece_tbl)
      tippies <- get_header_tippies(woodpiece_tbl)

      rhandsontable::rhandsontable(
        wp_data_in(),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        height = 180,
        colHeaders = unname(colHeaders),
        afterGetColHeader = tippy_renderer(tippies)) %>%
        purrr::reduce(
          names(colHeaders), # names in df
          function(ht, col) {
            config <- woodpiece_tbl[[col]]
            colName <- colHeaders[col] # name in ht
            hot_col_wrapper(ht, colName, config)
          },
          .init = .
        )
    })

    # create dataframe reactive to hot updates
    wp_data_out <- reactive({
      rhandsontable::hot_to_r(input$wp_table)
    })

    # SLIDE TABLE --------------------------------------------------------------
    output$slide_table <- rhandsontable::renderRHandsontable({
      validate(need(!is.null(slide_data_in()), "No data to show"))

      colHeaders <- get_tbl_colHeaders(slide_tbl)
      tippies <- get_header_tippies(slide_tbl)

      rhandsontable::rhandsontable(
        slide_data_in(),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        colHeaders = unname(colHeaders),
        afterGetColHeader = tippy_renderer(tippies)) %>%
        purrr::reduce(
          names(colHeaders), # names in df
          function(ht, col) {
            config <- slide_tbl[[col]]
            colName <- colHeaders[col] # name in ht
            hot_col_wrapper(ht, colName, config)
          },
          .init = .
        )
    })

    # create dataframe reactive to hot updates
    slide_data_out <- reactive({
      rhandsontable::hot_to_r(input$slide_table)
    })



    # TODO: check configs, val functions, edge cases
    # VALIDATION CHECKS --------------------------------------------------------
    validation_checks <- reactive({
      results <- list()

      # 1) site table
      df_site <- site_data_out()
      results$site_data <- collect_hot_val_results(df_site, site_tbl)

      # 2) tree table
      df_tree <- tree_data_out()
      results$tree_data <- collect_hot_val_results(df_tree, tree_tbl)

      # 3) woodpiece table
      df_wp <- wp_data_out()
      results$wp_data <- collect_hot_val_results(df_wp, woodpiece_tbl)

      # 4) slide table
      df_slide <- slide_data_out()
      results$slide_data <- collect_hot_val_results(df_slide, slide_tbl)



      # convert collected results to dataframe
      df_results <- results %>%
        purrr::map(~ .x %>%
                     purrr::map(~ tibble::tibble(
                       field = .x$field,
                       type = .x$type,
                       message = .x$message
                     )) %>%
                     purrr::list_rbind(names_to = 'fname')) %>%
        purrr::list_rbind(names_to = 'tname')

      df_results$topic <- input_field_names[df_results$tname]

      dplyr::bind_rows(
        data.frame(topic = character(0), field = character(0),
                   type = character(0), message = character(0)),
        df_results)
    })


    output$validation_check <- renderUI({
        df_validation <- validation_checks()

        if (nrow(df_validation) == 0) {
          return(tagList(strong("ALL GOOD :)", style = paste0('color: ', prim_col, ';'))))
        } else {
          # generate html lists for each topic
          html_output <- df_validation %>%
            dplyr::group_by(topic) %>%
            dplyr::summarise(
              content = paste0("<li>", field, ": ", message, "</li>", collapse = "")
            ) %>%
            dplyr::mutate(
              html = paste0("<b>", topic, ":</b><ul>", content, "</ul>")
            ) %>%
            dplyr::pull(html) %>%
            paste(collapse = "")

          return(HTML(html_output))

          # TODO:
          # add warning messages before switching tab or saving data

        }
    })


    # Next button
    observeEvent(input$btn_next, {
      #iv_gen$enable()
      nav_select(id = 'tabs', selected = tab_summary, session = main_session)
    })

    # Previous button
    observeEvent(input$btn_prev, {
      #iv_gen$enable()
      nav_select(id = 'tabs', selected = tab_general, session = main_session)
    })


    # TODO:
    # SAVE BUTTON



    return(
      list(
        input_meta = list(
          site_data = site_data_out,
          tree_data = tree_data_out,
          wp_data = wp_data_out,
          slide_data = slide_data_out
        ),
        val_check = validation_checks
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




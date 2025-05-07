# columns required for df_meta
cols_structure <- c('site', 'species', 'tree', 'woodpiece', 'slide', 'image')
cols_img <- c('org_img_name', 'img_filetype', 'img_size', 'img_width', 'img_height')
cols_settings <- c('software', 'sw_version', 'configuration_file', 'created_at',
                   'spatial_resolution', 'origin_calibrated_x', 'origin_calibrated_y',
                   'meas_geometry', 'circ_lower_limit', 'circ_upper_limit',
                   'outmost_year', 'min_cell_area', 'max_cell_area', 'dbl_cwt_threshold',
                   'max_cwtrad_s', 'max_cwtrad_l', 'relwidth_cwt_window',
                   'maxrel_opp_cwt', 'max_cwttan_s', 'max_cwttan_l')
# TODO: ROXAS AI
# if XXXX in colnames (->assume ROXAS AI), then cols_settings <- cols_ai
cols_paths <- c('fname_image', 'fname_cells', 'fname_rings', 'fname_settings')
cols_other <- c('image_code', 'tree_code', 'woodpiece_code', 'slide_code')

# a custom table container to get two row headers
create_table_sketch <- function(cols_structure, cols_img, cols_settings, cols_paths, color1, color2){
  sketch <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, style = paste0('background: ', color1), 'Image code'),
        th(colspan = length(cols_structure), style = paste0('background: ', color2), 'Data structure'),
        th(colspan = length(cols_img), 'Image info'),
        th(colspan = length(cols_settings), style = paste0('background: ', color2), 'Roxas settings'),
        th(colspan = length(cols_paths), 'File paths')
      ),
      tr(
        lapply(cols_structure,
               \(x) th(x, style = paste0('background: ', color2))),
        lapply(cols_img, th),
        lapply(cols_settings,
               \(x) th(x, style = paste0('background: ', color2))),
        lapply(cols_paths, th),
      )
    )
  ))

  return(sketch)
}



# SERVER -----------------------------------------------------------------------
start_server <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # BUTTONS AND INPUTS -------------------------------------------------------
    # toggle file input based on input source radiobuttons
    observe({
      shinyjs::toggleState(id = "file_input",
                           condition = input$input_src %in% c("df_meta_csv", "df_meta_json"))
      shinyjs::toggleState(id = "btn_load_input",
                           condition = (!is.null(input$input_src) && input$input_src == 'df_meta_env') || (!is.null(input$file_input)))
    })

    # initialize reactives for input data
    input_meta <- reactiveValues(
      source = "Please provide and load input source",
      df = NULL,
      meta_json = NULL
    )

    # next button
    # toggle: enable only if we have input data and check_raw is checked
    observe({
      shinyjs::toggleState(id = "btn_next", condition = (!is.null(input_meta$df) && input$check_raw))
    })

    # functionality: switch to next tab
    observeEvent(input$btn_next, {
      nav_select(id = 'tabs', selected = tab_general, session = main_session)
    })



    # load input data
    observeEvent(input$btn_load_input, {
      # Check if df is already set
      if (!is.null(input_meta$df)) {
        showModal(
          modalDialog(
            title = "Warning",
            "This action overwrites any existing inputs provided in the app.
            Are you sure you want to proceed?",
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("confirm_submit"), "Proceed")
            )
          )
        )
      } else {
        # Load the data directly if no existing df
        data <- load_input_data(input$input_src, input$file_input)
        input_meta$df <- data$df
        input_meta$source <- data$source
        if (input$input_src == "df_meta_json") {
          input_meta$meta_json <- load_whole_json(input$file_input$datapath)
        }
        shinyjs::reset(id = "check_raw")
      }
    })

    # # load input data
    # observeEvent(input$btn_load_input, {
    #   if (is.null(input_meta$df)){
    #     input_meta <- load_input_data(input$input_src, input$file_input, input_meta)
    #     shinyjs::reset(id = "check_raw")
    #   } else {
    #     # show warning if we already have df_meta
    #     showModal(
    #       modalDialog(
    #         title = "Warning",
    #         "This action overwrites any existing inputs provided in the app. Are you sure you want to proceed?",
    #         footer = tagList(
    #           modalButton("Cancel"),
    #           actionButton(ns("confirm_submit"), "Proceed")
    #         )
    #       )
    #     )
    #   }
    # })

    # load input data in case of confirm overwrite
    observeEvent(input$confirm_submit, {
      removeModal()
      # Load the data
      data <- load_input_data(input$input_src, input$file_input)
      input_meta$df <- data$df
      input_meta$source <- data$source
      if (input$input_src == "df_meta_json") {
        input_meta$meta_json <- load_whole_json(input$file_input$datapath)
      }
      shinyjs::reset(id = "check_raw")
    })

    # print source of input data
    output$file_status <- renderUI({
      if (is.null(input_meta$df)) {
        code(input_meta$source, class = 'code-output', style = 'color: #da292e;') # this is the bs-danger color
      } else {
        code(input_meta$source, class = 'code-output')
      }
    })


    # RENDER SHINYTREE ---------------------------------------------------------
    # create data.tree and shinyTree compatible JSON of data structure
    dtree_json <- reactive({
      req(input_meta$df)

      #df_meta %>% dplyr::select(site, species, tree, tree_code, woodpiece, woodpiece_code, slide, slide_code, image_code, org_img_name)
      df_dtree <- input_meta$df %>% dplyr::select(site, species, tree, tree_code, woodpiece, woodpiece_code, slide, slide_code, image_code, org_img_name)
      df_dtree <- df_dtree %>% #tidyr::replace_na(list(woodpiece = '(n.a.)')) %>% # TODO: if other levels are NA, replace also with '(n.a.)'?
        dplyr::mutate(dplyr::across(c('tree', 'woodpiece', 'slide'), \(x) tidyr::replace_na(x, '(n.a.)'))) %>%
        dplyr::mutate(DS = 'dataset', .before = 1) %>% # need a root name for data.tree
        tidyr::unite('pathString', DS, site, tree_code, woodpiece_code, slide_code, image_code, sep = '/', remove = FALSE)

      dtree <- data.tree::FromDataFrameTable(
        df_dtree,
        colLevels = list(NULL, NULL, c('species','tree'),
                         'woodpiece', 'slide', 'org_img_name'))

      for (site in dtree$children) {
        site$state <- c(selected = TRUE, loaded = TRUE, opened = TRUE)
        site$icon <- "glyphicon glyphicon-map-marker"
        site$text <- paste0('<strong>',site$name, '</strong>')
        for (tree in site$children) {
          tree$icon <- "glyphicon glyphicon-tree-conifer"
          tree$text <- paste0('<strong>', paste(tree$species, tree$tree), '</strong>', sprintf("  [code: %s]", tree$name))
          for (wp in tree$children) {
            wp$icon <- "fa fa-bore-hole"
            wp$state <- c(opened = TRUE, loaded = TRUE)
            wp$text <- paste0('<strong>', wp$woodpiece, '</strong>', sprintf("  [code: %s]", wp$name))
            for (slide in wp$children) {
              slide$icon <- "fa fa-vial"
              slide$state <- c(opened = TRUE, loaded = TRUE)
              slide$text <- paste0('<strong>', slide$slide, '</strong>', sprintf("  [code: %s]", slide$name))
              for (img in slide$children) {
                img$icon <- "glyphicon glyphicon-picture"
                img$text <- paste0('<strong>', img$org_img_name, '</strong>', sprintf(" [code: %s]", img$name))
              }
            }
          }
        }
      }

      shinyTree::treeToJSON(dtree, pretty = TRUE)
    })

    output$tree <- shinyTree::renderTree({
      validate(need(!is.null(input_meta$df), "No data to show"))

      dtree_json()
      })

    # output$notree <- renderPrint({
    #   if (is.null(input_meta$df)) {
    #     "no data to show"
    #   }
    # })


    # RENDER DT ----------------------------------------------------------------
    # Preprocess input metadata
    filt_meta <- reactive({
      req(input$tree)

      # format raw df: cols in right order
      df <- input_meta$df %>%
        dplyr::select(image_code, dplyr::all_of(cols_structure),
                      dplyr::all_of(cols_img), dplyr::all_of(cols_settings),
                      dplyr::all_of(cols_paths)) %>%
        dplyr::mutate(dplyr::across(site:slide, as.factor))
      # filter for selected images in tree
      selected_imgs <- get_selected_imgs(input$tree)
      df <- df %>% dplyr::filter(image_code %in% selected_imgs)
      df
    })


    # render the datatable
    output$DTmeta <- DT::renderDT({
      validate(need(!is.null(input_meta$df), "No data to show"))

      DT::datatable(filt_meta(),
                    style = 'default',
                    rownames = FALSE,
                    container = create_table_sketch(
                      cols_structure, cols_img, cols_settings, cols_paths,
                      prim_col_grad[4], prim_col_grad[6]),
                    selection = 'none',
                    extensions = "FixedColumns",
                    options = list(
                      scrollX = TRUE,
                      #searching = FALSE,
                      fixedColumns = list(leftColumns = 1)
                    )
      ) %>%
        DT::formatStyle(
          columns = c('image_code'), backgroundColor = prim_col_grad[4]) %>%
        DT::formatStyle(
          columns = c(cols_structure,cols_settings), backgroundColor = prim_col_grad[6])
    })

    # output$noDTmeta <- renderPrint({
    #   if (is.null(input_meta$df)) {
    #     "no data to show"
    #   }
    # })

    validation_checks <- reactive({
      df_results <- data.frame(topic = character(0), field = character(0),
                               type = character(0), message = character(0))
      if (!input$check_raw) {
        df_results <- dplyr::bind_rows(
          df_results,
         data.frame(topic = "Raw input data",
                    field = "Inferred structure",
                    type = "error",
                    message = "Not confirmed"))
      }

      df_results

    })

    output$testing <- renderPrint(
      input_meta$meta_json
    )


    # return the input meta and val check for use in other tabs
    return(
      list(
        input_meta = input_meta,
        val_check = validation_checks
      )
    )

  }) # end of moduleServer
}





# prefilled_meta <- reactive({
#   if (is.null(input$file_upload)) {
#     # check if there is df_meta available in the environment
#     if (exists('df_meta')){
#       return(list(df = df_meta, source = 'df_meta from R environment'))
#     } else {
#       return(list(df = NULL, source = 'Please provide a metadata file'))
#     }
#   } else { # load from provided input
#     df <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
#     return(list(df = df, source = paste0('read from file ',input$file_upload$name)))
#   }
# })
#
#
# # Get already available metadata from env or file
# prefilled_meta <- reactive({
#   if (is.null(input$file_upload)) {
#     # check if there is df_meta available in the environment
#     if (exists('df_meta')){
#       return(list(df = df_meta, source = 'df_meta from R environment'))
#     } else {
#       return(list(df = NULL, source = 'Please provide a metadata file'))
#     }
#   } else { # load from provided input
#     df <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
#     return(list(df = df, source = paste0('read from file ',input$file_upload$name)))
#   }
# })


# output$testing1 <- renderPrint({
#   tree <- input$tree
#   req(tree)
#   #print_tree(shinyTree::get_selected(tree))
#   #tree
#   get_selected_imgs(tree)
#   #tree
# })

# Preprocess partial metadata
# partial_meta_out <- reactive({
#   req(prefilled_meta()$df)
#   df <- prefilled_meta()$df %>% dplyr::select(site, woodpiece_code, image_code,
#                                               software:img_height, fname_image)
#   reactable::reactable(df,
#                        groupBy = c('site', 'woodpiece_code'),
#                        theme = reactable_theme)
# })
#
# # Print table of prefilled metadata
# output$partial_meta_out <- reactable::renderReactable({
#   partial_meta_out()
# })


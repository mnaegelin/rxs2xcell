start_server <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {

    # Get already available metadata from env or file
    prefilled_meta <- reactive({
      if (is.null(input$file_upload)) {
        # check if there is df_meta available in the environment
        if (exists('df_meta')){
          return(list(df = df_meta, source = 'df_meta from R environment'))
        } else {
          return(list(df = NULL, source = 'Please provide a metadata file'))
        }
      } else { # load from provided input
        df <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
        return(list(df = df, source = paste0('read from file ',input$file_upload$name)))
      }
    })

    # Print source of prefilled metadata
    output$file_status <- renderUI({
      if (is.null(prefilled_meta()$df)) {
        code(prefilled_meta()$source, class = 'code-output', style = 'color: red;')
      } else {
        code(prefilled_meta()$source, class = 'code-output')
      }
    })

    # RENDER SHINYTREE -----------------------------------------------------------
    # create data.tree and shinyTree compatible JSON of data structure
    dtree_json <- reactive({
      req(prefilled_meta()$df)

      df_dtree <- prefilled_meta()$df %>% dplyr::select(site, tree_code, woodpiece, slide, image_code)
      df_dtree <- df_dtree %>% tidyr::replace_na(list(woodpiece = '(n.a.)')) %>% # TODO: if other levels are NA, replace also with '(n.a.)'?
        dplyr::mutate(DS = 'dataset', .before = 1) %>% # need a root name for data.tree
        tidyr::unite('pathString', sep = '/', remove = FALSE)
      dtree <- data.tree::as.Node(df_dtree)

      # set up the attributes for shinyTree:
      # icons, all selected (-> at highest level), lower levels opened and loaded
      for (site in dtree$children) {
        site$state = c(selected = TRUE, loaded = TRUE, opened = TRUE)
        site$icon = "glyphicon glyphicon-map-marker"
        for (tree in site$children) {
          tree$icon = "glyphicon glyphicon-tree-conifer"
          for (wp in tree$children) {
            wp$icon = "glyphicon glyphicon-triangle-right"
            wp$state = c(opened = TRUE, loaded = TRUE)
            for (slide in wp$children) {
              slide$icon = "glyphicon glyphicon-triangle-right"
              slide$state = c(opened = TRUE, loaded = TRUE)
              for (img in slide$children) {
                img$icon = "glyphicon glyphicon-picture"
              }
            }
          }
        }
      }

      shinyTree::treeToJSON(dtree, pretty = TRUE)
    })

    output$tree <- shinyTree::renderTree(dtree_json())

    # RENDER DT ------------------------------------------------------------------
    # Preprocess partial metadata
    filt_meta <- reactive({
      req(prefilled_meta()$df)
      req(input$tree)

      # format raw df
      df <- prefilled_meta()$df %>%
        dplyr::select(image_code, site, species, tree_code, woodpiece_code, slide_code,
                      org_img_name, img_filetype, img_size, img_width, img_height,
                      software:max_cwttan_l,
                      fname_image, fname_cells, fname_rings, fname_settings
        ) %>%
        dplyr::mutate(dplyr::across(site:slide_code, as.factor))
      # filter for selected images in tree
      selected_imgs <- get_selected_imgs(input$tree)
      df <- df %>% dplyr::filter(image_code %in% selected_imgs)
      df
    })


    # a custom table container to get two row headers
    color1 <- '#CCE0E0'
    color2 <- '#F2F7F7'
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, style = paste0('background: ', color1), 'Image code'),
          th(colspan = 5, style = paste0('background: ', color2), 'Data structure'),
          th(colspan = 5, 'Image info'),
          th(colspan = 20, style = paste0('background: ', color2), 'Roxas settings'),
          th(colspan = 4, 'File paths')
        ),
        tr(
          lapply(c('site', 'species', 'tree_code', 'woodpiece_code', 'slide_code'),
                 \(x) th(x, style = paste0('background: ', color2))),
          lapply(c('org_img_name', 'img_filetype', 'img_size', 'img_width', 'img_height'), th),
          lapply(c('software', 'sw_version', 'configuration_file', 'created_at',
                   'spatial_resolution', 'origin_calibrated_x', 'origin_calibrated_y',
                   'meas_geometry', 'circ_lower_limit', 'circ_upper_limit',
                   'outmost_year', 'min_cell_area', 'max_cell_area', 'dbl_cwt_threshold',
                   'max_cwtrad_s', 'max_cwtrad_l', 'relwidth_cwt_window',
                   'maxrel_opp_cwt', 'max_cwttan_s', 'max_cwttan_l'),
                 \(x) th(x, style = paste0('background: ', color2))),
          lapply(c('fname_image', 'fname_cells', 'fname_rings', 'fname_settings'), th),
        )
      )
    ))

    output$DTmeta <- DT::renderDT({
      req(filt_meta())
      DT::datatable(filt_meta(),
                    style = 'default',
                    rownames = FALSE,
                    container = sketch,
                    selection = 'none',
                    extensions = "FixedColumns",
                    options = list(
                      scrollX = TRUE,
                      #searching = FALSE,
                      fixedColumns = list(leftColumns = 1)
                    )
      ) %>%
        DT::formatStyle(
          columns = c('image_code'), backgroundColor = color1) %>%
        DT::formatStyle(
          columns = c(2:6,12:31) , backgroundColor = color2)
    })

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

    # Next button
    # toggle: only enable in case we have df_meta
    observe({
      shinyjs::toggleState(id = "btn_next_meta", condition = !is.null(prefilled_meta()$df))
    })
    # functionality: switch to next tab
    observeEvent(input$btn_next_meta, {
      nav_select(id = 'tabs', selected = tab_general, session = main_session)
    })


    return(
      prefilled_meta
    )

  }) # end of moduleServer
}

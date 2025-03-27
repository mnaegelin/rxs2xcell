

reactable_theme <- reactable::reactableTheme(
  backgroundColor = "#dfe7e8"
)



# helper to get all selected images in the tree
get_selected_imgs <- function(tree, selected = c()) { #ancestry = "",
  for (leaf in names(tree)){
    if (is.null(names(tree[[leaf]]))) {
      a <- attr(tree[[leaf]], 'stselected', TRUE)
      if (!is.null(a) && a == TRUE) {
        selected <- c(selected, leaf) # paste0(ancestry, "/", leaf) # since image_codes are unique, we don't need the path
      }
    } else {
      selected <- get_selected_imgs(tree[[leaf]], selected) #paste0(ancestry, "/", leaf)
    }
  }
  return(selected)
}

# server -----------------------------------------------------------------------
server <- function(input, output, session) {


  # TAB: prefill metadata
  # ----------------------------------------------------------------------------

  # Get already available metadata from env or file
  prefilled_meta <- reactive({
    if (is.null(input$file_upload)) {
      # check if there is df_meta available in the environment
      if (exists('df_meta')){
        return(list(df = df_meta, source = 'Using df_meta available in R environment.'))
      } else {
        return(list(df = NULL, source = span('Please provide a metadata file.', class = 'text-danger')))
      }
    } else { # load from provided input
      df <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
      return(list(df = df, source = paste('Using provided file',input$file_upload$name)))
    }
  })

  # Print source of prefilled metadata
  output$file_status <- renderUI({
    prefilled_meta()$source
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
  #print(sketch)

  output$DTmeta <- DT::renderDT({
    req(filt_meta())
    DT::datatable(filt_meta(),
                  style = 'default',
                  rownames = FALSE,
                  container = sketch,
                  #filter = 'top',
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
    nav_select(id = 'tabs', selected = "General")
  })



  # TAB: general ---------------------------------------------------------------

  # toggle: only enable in case we have a country and search string
  shiny::observe({
    shinyjs::toggleState(id = "search_ror",
                         condition = !((input$search_string=="") &&
                                         (input$search_country=="")))
  })

  # run the search via ROR API
  shiny::observeEvent(input$search_ror, {
    req(input$country_code)
    req(input$search_string)

    search_url <- sprintf(
      'https://api.ror.org/v2/organizations?query=%s&filter=country.country_code:%s',
      URLencode(input$search_string),
      input$country_code)

    ror_res <- httr::GET(search_url, httr::timeout(5))

    if (httr::status_code(ror_res) == 200) {
      ror_data <- jsonlite::fromJSON(rawToChar(ror_res$content))

      if (ror_data$number_of_results > 0) {

        # output$testing2 <- renderPrint({
        #   ror_data$items$names %>%
        #     dplyr::bind_rows()
        # })
        # get the names (assuming that there is always exactly one ror_display name)
        res_names <- ror_data$items$names %>%
          dplyr::bind_rows() %>%
          dplyr::filter(grepl('ror_display', types)) %>%
          dplyr::pull(value)

        # get the locations
        res_locs <- ror_data$items$locations %>%
          dplyr::bind_rows() %>%
          dplyr::pull(geonames_details) %>%
          tidyr::unite(col = 'address', name, country_name, sep = ', ') %>%
          dplyr::pull(address)

        # a dataframe of res_locs, res_names and res_ror_ids
        res_df <- data.frame(ROR = ror_data$items$id, Name = res_names,
                             Location = res_locs)

        #updateSelectizeInput(session, "result_choice", choices = res_names)
        output$ror_results <- DT::renderDT({
          DT::datatable(res_df, rownames = FALSE)
        })
      } else {
        showNotification("No ROR results found. Try again.", type = "message")
      }
    } else {
      showNotification("ROR API request failed. Try again.", type = "error")
    }

  })
  iv_gen <- shinyvalidate::InputValidator$new()

  iv_gen$add_rule("ds_name", shinyvalidate::sv_required())
  iv_gen$add_rule("ds_name", max_char_limit, limit = 64)

  iv_gen$add_rule("autname_1", shinyvalidate::sv_required())
  iv_gen$add_rule("autmail_1", shinyvalidate::sv_required())

  # Keep track of the number of authors
  author_count <- reactiveVal(1)

  # Enable delete button only if there are more than 1 authors
  observe({
    shinyjs::toggleState(id = "del_author_btn", condition = author_count() > 1)
  })

  # Add author input if button is clicked
  observeEvent(input$add_author_btn, {
    author_count(author_count() + 1)
    author_id <- paste0('aut', author_count())
    insertUI(
      selector = "#author_inputs",
      where = "beforeBegin",
      ui = div(id = author_id,
               author_input(author_count()))
    )
    iv_gen$add_rule(paste0('autname_',author_count()), shinyvalidate::sv_required())
  })

  # Delete author input
  observeEvent(input$del_author_btn, {
    removeUI(selector = paste0("#aut", author_count()))
    iv_gen$remove_rules(paste0('autname_',author_count()))
    author_count(author_count() - 1)

  })

  # Dynamic selection of contact person
  output$contact_person <- renderUI({
    radioButtons("contact_person", NULL,
                 choices = paste("Author", 1:author_count()))
  })

  iv_gen$enable()


  output$check_val <- renderPrint({
    iv_gen$validate()

  })

  # Next button
  observeEvent(input$nex_btn_general, {
    nav_select(id = 'tabs', selected = "Study Details")
  })


  # unique_sites <- unique(df_meta$site)
  #
  # # Dynamic UI generation for each unique site_code
  # output$site_inputs <- renderUI({
  #   lapply(unique_sites, function(site_code) {
  #     fluidRow(
  #       column(12, tags$h3(paste("Site Code:", site_code))),
  #       column(6, textInput(paste0("name_", site_code), "Site Name")),
  #       column(6, textInput(paste0("country_", site_code), "Country")),
  #       column(12, textAreaInput(paste0("description_", site_code), "Description")),
  #       column(4, numericInput(paste0("longitude_", site_code), "Longitude", value = NA)),
  #       column(4, numericInput(paste0("latitude_", site_code), "Latitude", value = NA)),
  #       column(4, numericInput(paste0("elevation_", site_code), "Elevation", value = NA)),
  #       hr()
  #     )
  #   })
  # })
  #
  # # Collect and display the input data
  # output$site_data <- renderPrint({
  #   site_data <- lapply(unique_sites, function(site_code) {
  #     list(
  #       site_code = site_code,
  #       name = input[[paste0("name_", site_code)]],
  #       country = input[[paste0("country_", site_code)]],
  #       description = input[[paste0("description_", site_code)]],
  #       longitude = input[[paste0("longitude_", site_code)]],
  #       latitude = input[[paste0("latitude_", site_code)]],
  #       elevation = input[[paste0("elevation_", site_code)]]
  #     )
  #   })
  #   site_data <- dplyr::bind_rows(site_data)
  #   print(site_data)
  # })
  #
  #
  # # Render editable datatable
  # output$tree_table <- DT::renderDT({
  #   dtable <- DT::datatable(df_edit,
  #                           editable = list(target = "cell"),
  #                           selection = "none",
  #                           extensions = "AutoFill",
  #                           callback = DT::JS(callback),
  #                           options = list(
  #                             autoFill = list(horizontal = FALSE)
  #                           )
  #   )
  # }, server = FALSE)
  #
  # Data <- reactive({
  #   info <- rbind(input[["tree_table_cells_filled"]], input[["tree_table_cell_edit"]])
  #   if(!is.null(info)){
  #     info <- unique(info)
  #     info$value[info$value==""] <- NA
  #     df_edit <<- editData(df_edit, info)
  #   }
  #   df_edit
  # })


  # TAB: study details ---------------------------------------------------------
  # Reactive values to store uploaded data
  study_data <- reactiveVal(NULL)
  # Dynamically generate location inputs based on the number of study sites
  output$dynamic_location_inputs <- renderUI({
    df <- df_meta_pre()

    if (is.null(df)) {
      return("No study site data uploaded yet.")
    }

    num_sites <- nrow(df)

    site_inputs <- lapply(1:num_sites, function(i) {
      wellPanel(
        h4(paste("Study Site", i)),
        textInput(paste0("country_", i), "Country", df$Country[i]),
        textInput(paste0("city_", i), "City", df$City[i]),
        numericInput(paste0("latitude_", i), "Latitude", df$Latitude[i], min = -90, max = 90),
        numericInput(paste0("longitude_", i), "Longitude", df$Longitude[i], min = -180, max = 180)
      )
    })

    do.call(tagList, site_inputs)
  })

  # Collect metadata
  metadata <- reactive({
    df <- study_data()

    if (is.null(df)) {
      locations <- list(
        list(Country = "", City = "", Latitude = NA, Longitude = NA)
      )
    } else {
      locations <- lapply(1:nrow(df), function(i) {
        list(
          Country = input[[paste0("country_", i)]] %||% df$Country[i],
          City = input[[paste0("city_", i)]] %||% df$City[i],
          Latitude = input[[paste0("latitude_", i)]] %||% df$Latitude[i],
          Longitude = input[[paste0("longitude_", i)]] %||% df$Longitude[i]
        )
      })
    }

    list(
      Title = input$title,
      Author = input$author,
      Description = input$description,
      Study_Type = input$study_type,
      Sample_Size = input$sample_size,
      Study_Date = as.character(input$study_date),
      Locations = locations
    )
  })

  # Show metadata preview
  output$preview_table <- renderTable({
    data <- metadata()
    locations_df <- do.call(rbind, lapply(data$Locations, as.data.frame))
    cbind(data.frame(Title = data$Title, Author = data$Author,
                     Description = data$Description, Study_Type = data$Study_Type,
                     Sample_Size = data$Sample_Size, Study_Date = data$Study_Date),
          locations_df)
  })

  # Save metadata to file
  observeEvent(input$save_btn, {
    data <- metadata()
    locations_df <- do.call(rbind, lapply(data$Locations, as.data.frame))

    final_df <- cbind(data.frame(
      Title = data$Title,
      Author = data$Author,
      Description = data$Description,
      Study_Type = data$Study_Type,
      Sample_Size = data$Sample_Size,
      Study_Date = data$Study_Date
    ), locations_df)

    write.csv(final_df, "metadata.csv", row.names = FALSE)

    output$save_status <- renderText("Metadata saved to metadata.csv")
  })


}


# RADIALNETWORK TRY
# Modified JavaScript code to send the selected node to Shiny for the networkD3 plot
# clickJS <- '
# d3.selectAll(".node").on("click", function(d){
#   Shiny.onInputChange("selected_node", d.data.name);
# })
# '

# dtree_list <- data.tree::ToListExplicit(dtree, unname = TRUE)
# dtree_list

# })

# output$radial_network <- networkD3::renderDiagonalNetwork({
#   onRender(networkD3::diagonalNetwork(dtree_list()), clickJS)
# })

# Display the selected node in verbatimTextOutput
# output$selectedNode <- renderPrint({
#   input$selected_node
# })

# SUNBURST TRY
# df_sb <- data.tree::ToDataFrameNetwork(dtree) %>%
#   dplyr::rename(parents = from, ids = to) %>%
#   dplyr::mutate(labels = stringr::str_split_i(ids, "/", -1))
# df_sb[df_sb$parents == 'dataset', 'parents'] = ""

# fig <- plotly::plot_ly(
#   df_sb, ids = ~ids, labels = ~labels, parents = ~parents,
#   type = 'sunburst',
#   insidetextorientation='tangential'
#   ) %>% plotly::layout(showlegend = TRUE)
# fig
#

# DENDROGRAM TRY
# tree_data <- ggdendro::dendro_data(as.dendrogram(dtree))
#
# p <- ggplot2::ggplot() +
#   ggplot2::geom_segment(data = tree_data$segments, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
#                colour = "blue", alpha = 0.5) +
#   ggplot2::geom_text(data = tree_data$labels,
#             ggplot2::aes(x = x, y = y, label = label), vjust = -0.5, size = 3) +
#   # ggplot2::geom_text(data = ggdendro::leaf_label(tree_data),
#   #           ggplot2::aes(x = x, y = y, label = label), vjust = 0.5, size = 2) +
#   ggplot2::coord_flip() +
#   ggdendro::theme_dendro()
# p

# shinyTree
#   # helper to get the path of the highest level selected in the tree
# get_selected_highest <- function(tree, ancestry = "", selected = c()) {
#   for (leaf in names(tree)){
#     a <- attr(tree[[leaf]], 'stselected', TRUE)
#     if (!is.null(a) && a == TRUE) {
#       selected <- c(selected, paste0(ancestry, "/", leaf))
#     } else {
#       selected <- get_selected_highest(tree[[leaf]], paste0(ancestry, "/", leaf), selected)
#     }
#   }
#   return(selected)
# }

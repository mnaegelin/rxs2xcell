# tree_tbl_str <- data.frame(
#   treecode = character(0),
#   sitecode = character(0),
#   speciescode = character(0),
#   speciesname = character(0),
#   woodtype = character(0),
#   leafhabit = character(0),
#   ring_structure = character(0),
#   tree_treatment = character(0),
#   treedesc = character(0),
#   n_wp = integer(0),
#   stringsAsFactors = FALSE
# )
#
# tree_tbl_config <- list(
#   # NOTE: order matters for colHeaders (needs to be same as in df)
#   colHeaders = c(treecode = "Tree Code", sitecode = "Site Code",
#                  speciescode = "Species Code", speciesname = 'Species',
#                  woodtype = "wood type", leafhabit = "leaf habit",
#                  ring_structure = "ring structure",
#                  tree_treatment = "treatment", treedesc = "description",
#                  n_wp = "Nr of Pieces"),
#   treecode = list(type = 'character', readOnly = TRUE),
#   sitecode = list(type = 'character', readOnly = TRUE),
#   speciescode = list(type = 'character', required = TRUE, options = c('ABSP','PISY')),
#   speciesname = list(type = 'autocomplete', required = TRUE, options = c('Abies Mill. fir','Pinus sylvestris L.')),
#   woodtype = list(type = 'dropdown', required = FALSE, options = c("gymnosperm", "angiosperm")),
#   leafhabit = list(type = 'dropdown', required = FALSE, options = c("evergreen", "deciduous")),
#   ring_structure = list(type = 'dropdown', required = FALSE, options = c("conifer", "diffuse-porous", 'ring-porous')),
#   tree_treatment = list(type = 'dropdown', required = FALSE,
#                       options = c("control", "treatment")),
#   treedesc = list(type = 'character', required = FALSE),
#   n_wp = list(type = 'numeric', readOnly = TRUE)
# )
#
#
# wp_tbl_str <- data.frame(
#   wpcode = character(0),
#   treecode = character(0),
#   sampledate = character(0),
#   organ = character(0),
#   sampling_height = numeric(0),
#   dist_to_tip = numeric(0),
#   sample_prep = character(0),
#   sample_type = character(0),
#   pith_year = numeric(0),
#   n_slides = integer(0),
#   stringsAsFactors = FALSE
# )
#
# slide_tbl_str <- data.frame(
#   slidecode = character(0),
#   wpcode = character(0),
#   sampledate = character(0),
#   sect_thickness = character(0),
#   cuttingplane = character(0),
#   img_cap_system = character(0),
#   data_structure = character(0),
#   n_imgs = integer(0),
#   stringsAsFactors = FALSE
# )
#


tree_server <- function(id, main_session, prefilled_meta) {
  moduleServer(id, function(input, output, session) {

    # initialize site_data reactives
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
    observeEvent(prefilled_meta(), {
      if (!is.null(prefilled_meta()$df)) {
        df_meta <- prefilled_meta()$df

        df_in_tr <- tree_data$df_in
        # get unique site codes and nr of trees per site
        trees_wps <- df_meta %>%
          dplyr::group_by(site, species, tree_code) %>%
          dplyr::summarise(n = dplyr::n_distinct(woodpiece_code), .groups = 'keep') %>%
          dplyr::rename(treecode = tree_code, sitecode = site, speciescode = species, n_wp = n)
        # add to site df and update reactive
        tree_data$df_in <- df_in_tr %>%
          dplyr::right_join(trees_wps, by = c('treecode', 'sitecode', 'speciescode', 'n_wp'))

        df_in_wp <- wp_data$df_in
        wp_slides <- df_meta %>%
          dplyr::group_by(tree_code, woodpiece_code) %>%
          dplyr::summarise(n = dplyr::n_distinct(slide_code), .groups = 'keep') %>%
          dplyr::rename(wpcode = woodpiece_code, treecode = tree_code, n_slides = n)
        wp_data$df_in <- df_in_wp %>%
          dplyr::right_join(wp_slides, by = c('wpcode', 'treecode', 'n_slides'))

        df_in_sl <- slide_data$df_in
        slide_imgs <- df_meta %>%
          dplyr::group_by(woodpiece_code, slide_code) %>%
          dplyr::summarise(n = dplyr::n_distinct(image_code), .groups = 'keep') %>%
          dplyr::rename(slidecode = slide_code, wpcode = woodpiece_code, n_imgs = n)
        slide_data$df_in <- df_in_sl %>%
          dplyr::right_join(slide_imgs, by = c('slidecode', 'wpcode', 'n_imgs'))

      }
    })

    output$tree_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        tree_data$df_in,
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
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
      tree_data$df_out <- rhandsontable::hot_to_r(input$tree_table)
    })
    observeEvent(input$wp_table, {
      wp_data$df_out <- rhandsontable::hot_to_r(input$wp_table)
    })
    observeEvent(input$slide_table, {
      slide_data$df_out <- rhandsontable::hot_to_r(input$slide_table)
    })

    # Next button
    observeEvent(input$btn_next, {
      nav_select(id = 'tabs', selected = tab_summary, session = main_session)
    })

    # Previous button
    observeEvent(input$btn_prev, {
      nav_select(id = 'tabs', selected = tab_site, session = main_session)
    })

    output$testing <- renderPrint({
      wp_data$df_out
    })

    return(
      reactive(
        list(
          tree_data = tree_data$df_out,
          wp_data = wp_data$df_out,
          slide_data = slide_data$df_out
        )
      )
    )

  })
}

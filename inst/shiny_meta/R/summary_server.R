summary_server <- function(id, main_session, prefilled_meta, dataset_info, site_info, tree_info) {
  moduleServer(id, function(input, output, session) {
    # Previous button
    observeEvent(input$btn_prev, {
      nav_select(id = 'tabs', selected = tab_tree, session = main_session)
    })

    observeEvent(input$save_btn, {
      # save row id as author nr
      author_table <- dataset_info()$author_data
      author_table <- author_table %>% tibble::rowid_to_column(data, "author_nr")

      json_data <- jsonlite::toJSON(
        list(
          ds_info = dataset_info()$ds_data,
          author_table = author_table,
          funding_table = dataset_info()$funding_data,
          site_table = site_info(),
          tree_table = tree_info()$tree_data,
          wp_table = tree_info()$wp_data,
          slide_table = tree_info()$slide_data,
          image_table = prefilled_meta()$df
          ), pretty = TRUE)
      write(json_data, "data.json")
    })

  })
}

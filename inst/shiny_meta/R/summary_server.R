summary_server <- function(id, main_session, start_info, dataset_info, site_info, tree_info) {
  moduleServer(id, function(input, output, session) {

    # output$testing <- renderPrint({
    #   validation_results <- site_data()$val_check
    #   print(validation_results$site_table)
    # })
    #
    # output$validation_check <- renderUI({
    #
    #   validation_results <- dataset_info()$val_check
    #   validation_results <- c(validation_results, site_info()$val_check)
    #
    #   issues_output <- list()
    #   for (input_type in names(validation_results)){
    #     issue_list <- list()
    #     for (input_name in names(validation_results[[input_type]])) {
    #       issues <- validation_results[[input_type]][[input_name]]
    #       # If there are issues for the current column, add them to the issue list
    #       if (length(issues) > 0) {
    #         issue_list <- tagList(issue_list, tags$li(paste0(input_name, ": ", paste(issues, collapse = ", "))))
    #       }
    #     }
    #     if (length(issue_list) > 0) {
    #       issues_output <- tagList(issues_output,
    #                                tagList(paste0("Issues with ", input_type, " input:"), tags$ul(issue_list))
    #       )
    #     }
    #   }
    #
    #   # set the color of the header based on the validation results
    #   # shinyjs::toggleClass(id = "val_check_header", class = 'bg-secondary',
    #   #                      condition = length(issues_output) > 0)
    #
    #   # TODO:
    #   # add warning messages before switching tab or saving data
    #
    #   # show output message
    #   if (length(issues_output) > 0) {
    #     tagList(issues_output)
    #   } else {
    #     tagList(strong("ALL GOOD :)", style = paste0('color: ', prim_col, ';')))
    #   }
    #
    # })

    # Previous button
    observeEvent(input$btn_prev, {
      nav_select(id = 'tabs', selected = tab_tree, session = main_session)
    })

    # observeEvent(input$save_btn, {
    #   # save row id as author nr
    #   author_table <- dataset_info()$input_meta$author_data
    #   author_table <- author_table %>% tibble::rowid_to_column(data, "author_nr")
    #
    #   json_data <- jsonlite::toJSON(
    #     list(
    #       ds_info = dataset_info()$ds_data,
    #       author_table = author_table,
    #       funding_table = dataset_info()$funding_data,
    #       site_table = site_info(),
    #       tree_table = tree_info()$tree_data,
    #       wp_table = tree_info()$wp_data,
    #       slide_table = tree_info()$slide_data,
    #       image_table = prefilled_meta()$df
    #       ), pretty = TRUE)
    #   write(json_data, "data.json")
    # })

  })
}

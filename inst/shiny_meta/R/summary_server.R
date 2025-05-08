summary_server <- function(id, main_session, start_info, dataset_info, site_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    valchecks_combined <- reactive({
      dplyr::bind_rows(
        start_info$val_check(),
        dataset_info$val_check(),
        site_info$val_check()
      )
    })

    output$DT_valcheck <- DT::renderDataTable({
      DT::datatable(
        valchecks_combined() %>% dplyr::select(-dplyr::any_of(c('fname', 'tname')))
      )

    })

    data_combined <- reactive({
      results <- list()
      results$ds_data <- list(
        ds_name = dataset_info$input_meta$ds_data$ds_name(),
        ds_desc = dataset_info$input_meta$ds_data$ds_desc(),
        ds_access = dataset_info$input_meta$ds_data$ds_access(),
        ds_license = dataset_info$input_meta$ds_data$ds_license(),
        ds_embargoed = dataset_info$input_meta$ds_data$ds_embargoed(),
        ds_ackn = dataset_info$input_meta$ds_data$ds_ackn()
      )
      results$author_data <- dataset_info$input_meta$author_data()
      results$funding_data <- dataset_info$input_meta$funding_data()
      results$doi_data <- dataset_info$input_meta$doi_data()
      results$site_data <- site_info$input_meta$site_data()
      results$tree_data <- site_info$input_meta$tree_data()
      results$wp_data <- site_info$input_meta$wp_data()
      results$slide_data <- site_info$input_meta$slide_data()

      results
    })



    observeEvent(input$btn_save, {
      validation_message <- if (nrow(valchecks_combined()) > 0) {
        tags$p(
          style = "color: red;",
          "Note that the automatic validation checks have raised issues with the collected metadata, see the validation check summary."
        )
      } else {
        NULL
      }

      showModal(modalDialog(
        title = "Confirm Export",
        tagList(
          "Do you want to save your progress for reuse later, or is this the final export?",
          validation_message
        ),
        footer = tagList(
          downloadButton(ns("save_progress"), "Save Progress"),
          downloadButton(ns("final_export"), "Final Export"),
          modalButton("Cancel")
        )
      ))
    })

    output$save_progress <- downloadHandler(
      filename = function() {
        paste0("collected_data_", Sys.Date(), ".json")
      },
      content = function(file) {
        removeModal()
        # Save the data to a file
        data_to_export <- data_combined()
        data_to_export$export_type <- 'in_progress'
        jsonlite::write_json(data_to_export, file)
      }
    )

    output$final_export <- downloadHandler(
      filename = function() {
        paste0("collected_data_", Sys.Date(), ".json")
      },
      content = function(file) {
        removeModal()
        # Save the data to a file
        data_to_export <- data_combined()
        data_to_export$export_type <- 'final'
        jsonlite::write_json(data_to_export, file)
      }
    )

    # # Handle "Save Progress" action
    # observeEvent(input$save_progress, {
    #   removeModal()  # Close the modal
    #   download_data("in_progress")
    # })
    #
    # # Handle "Final Export" action
    # observeEvent(input$final_export, {
    #   removeModal()  # Close the modal
    #   download_data("final")
    # })
    #
    # # Function to trigger download
    # download_data <- function(export_type) {
    #   # Add export_type to collected data
    #   data_to_export <- dataset_info$input_meta()
    #   data_to_export$export_type <- export_type
    #
    #   # Write data to JSON (example: save to a temporary file)
    #   json_file <- tempfile(fileext = ".json")
    #   jsonlite::write_json(data_to_export, json_file)
    #
    #   # Trigger file download
    #   shiny::downloadHandler(
    #     filename = function() {
    #       paste0("collected_data_", export_type, ".json")
    #     },
    #     content = function(file) {
    #       file.copy(json_file, file)
    #     }
    #   )
    #
    # }
    #
    # output$testing <- renderPrint({
    #   valchecks_combined()
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
      nav_select(id = 'tabs', selected = tab_site, session = main_session)
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

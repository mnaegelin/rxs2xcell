
# TODO: renderers for multicolumn checks?
# TODO: multicol validations?
renderer_ror_or_aff_name_addr <- "
  function(instance, td, row, col, prop, value, cellProperties) {
    var data = instance.getDataAtRow(row);
    var aff_name = data[4];
    var aff_address = data[7];
    var aff_rorid  = data[5];
    if (((aff_name === null || aff_name === '') && (aff_address === null || aff_address === '')) && (aff_rorid === null || aff_rorid === '')) {
      td.style.background = 'pink';
    } else if ((aff_name === null || aff_name === '') || (aff_address === null || aff_address === '')) {
      if (aff_rorid === null || aff_rorid === '') {
        td.style.background = 'pink';
      } else {
        td.style.background = '';
      }
    } else {
      td.style.background = '';
    }
    return td.innerHTML = value;
  }"


countries_list <- names(get_country_codes())

author_tbl_str <- data.frame(
    last_name = character(1),
    first_name = character(1),
    email = character(1),
    orcid = character(1),
    org_name = character(1),
    org_rorid = character(1),
    aff_dep = character(1),
    aff_street = character(1),
    aff_plz = character(1),
    aff_city = character(1),
    org_country = character(1),
    contactperson = logical(1),
    stringsAsFactors = FALSE)

author_tbl_config <- list(
  # NOTE: order matters for colHeaders (needs to be same as in df)
  colHeaders = c(last_name = 'Last name', first_name = 'First name',
                 email = 'Email address', orcid = 'ORCID',
                 org_name = 'Research Institution', org_rorid = 'RORID',
                 aff_dep = 'Department', aff_street = 'Street',
                 aff_plz = 'Postal code', aff_city = "City",
                 org_country = "Country", contactperson = 'Contact person'),
  last_name = list(type = 'character', required = TRUE),
  first_name = list(type = 'character', required = TRUE),
  email = list(type = 'character', required = TRUE,
               regex_pattern = '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$',
               unique = TRUE),
  orcid = list(type = 'character', required = FALSE),
  org_name = list(type = 'character', required = TRUE),
  org_rorid = list(type = 'character', required = FALSE),
  aff_dep = list(type = 'character', required = FALSE),
  aff_street = list(type = 'character', required = FALSE),
  aff_plz = list(type = 'character', required = FALSE),
  aff_city = list(type = 'character', required = FALSE),
  org_country = list(type = 'autocomplete', required = TRUE, options = countries_list),
  contactperson = list(type = 'checkbox', required = TRUE, max_checks = 1)
)

funding_tbl_str <- data.frame(
  inst_name = character(1),
  inst_rorid = character(1),
  inst_address = character(1),
  grantnr = character(1),
  stringsAsFactors = FALSE)

funding_tbl_config <- list(
  # NOTE: order matters for colHeaders (needs to be same as in df)
  colHeaders = c(inst_name = 'Funding Institution', inst_rorid = 'Inst. ROR',
                 inst_address = 'Inst. Address', grantnr = 'Grant Nr.'),
  inst_name = list(type = 'character', required = TRUE),
  inst_rorid = list(type = 'character', required = FALSE),
  inst_address = list(type = 'character', required = FALSE),
  grantnr = list(type = 'character', required = TRUE)
)


dataset_server <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # mock event to close ROR tab on start of app
    observeEvent(1,{
      accordion_panel_close(id = 'ror_search_acc', values = TRUE)
    }, ignoreNULL = FALSE) # to fire the event at startup


    # DATASET INPUT ------------------------------------------------------------
    # add validator rules for dataset inputs
    iv_gen <- shinyvalidate::InputValidator$new()
    iv_gen$add_rule("ds_name", shinyvalidate::sv_required())
    iv_gen$add_rule("ds_name", max_char_limit, limit = 64)
    iv_gen$add_rule("ds_desc", shinyvalidate::sv_required())

    iv_gen$enable() # TODO: enable from start?

    # ROR SEARCH ---------------------------------------------------------------
    # toggle search button: only enable if we have a country and search string
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
          ror_links <- paste0("<a href='",ror_data$items$id,"' target='_blank'>",ror_data$items$id,"</a>")
          rorids <- gsub('https://ror.org/', '', ror_data$items$id)
          res_df <- data.frame(Link = ror_links, RORID = rorids, Name = res_names,
                               Location = res_locs)

          #updateSelectizeInput(session, "result_choice", choices = res_names)
          output$ror_results <- DT::renderDT({
            DT::datatable(res_df,
                          style = 'default',
                          rownames = FALSE, selection = "none", escape = FALSE,
                          options = list(
                            pageLength = 5
                          )
            )
          })
        } else {
          showNotification("No ROR results found. Try again.", type = "message")
        }
      } else {
        showNotification("ROR API request failed. Try again.", type = "error")
      }

    })


    # AUTHOR RHANDSONTABLE -----------------------------------------------------
    # Initialize data frame
    author_data <- reactiveValues(
      df_in = author_tbl_str,
      df_out = NULL)

    # Render editable table
    output$author_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        author_data$df_in,
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        height = 150,
        colHeaders = unname(author_tbl_config$colHeaders)) %>%
        # custom validation checks renderers for all cols based on tbl_config
        purrr::reduce(
          names(author_tbl_config$colHeaders), # names in df
          function(ht, col) {
            config <- author_tbl_config[[col]]
            colName <- author_tbl_config$colHeaders[col] # name in ht
            hot_col_wrapper(ht, colName, config)
          },
          .init = .
        )
    })

    funding_data <- reactiveValues(
      df_in = funding_tbl_str,
      df_out = NULL)

    # Render editable table
    output$funding_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        funding_data$df_in,
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        colHeaders = unname(funding_tbl_config$colHeaders)) %>%
        # custom validation checks renderers for all cols based on tbl_config
        purrr::reduce(
          names(funding_tbl_config$colHeaders), # names in df
          function(ht, col) {
            config <- funding_tbl_config[[col]]
            colName <- funding_tbl_config$colHeaders[col] # name in ht
            hot_col_wrapper(ht, colName, config)
          },
          .init = .
        )
    })


    # Update data frame on table edit
    observeEvent(input$author_table, {
      author_data$df_out <- rhandsontable::hot_to_r(input$author_table)
    })

    observeEvent(input$funding_table, {
      funding_data$df_out <- rhandsontable::hot_to_r(input$funding_table)
    })


    # Observe add row button
    observeEvent(input$add_author_btn, {
      new_row <- author_tbl_str
      author_data$df_in <- rbind(author_data$df_out, new_row)
    })

    # Observe delete row button
    observeEvent(input$del_author_btn, {
      req(nrow(author_data$df_out) > 1)
      author_data$df_in <- author_data$df_out[-nrow(author_data$df_out), ]
    })

    # Observe add row button
    observeEvent(input$add_fund_btn, {
      new_row <- funding_tbl_str
      funding_data$df_in <- rbind(funding_data$df_out, new_row)
    })

    # Observe delete row button
    observeEvent(input$del_fund_btn, {
      req(nrow(funding_data$df_out) > 1)
      funding_data$df_in <- funding_data$df_out[-nrow(funding_data$df_out), ]
    })


    # Observe import button
    observeEvent(input$file_authors, {
      # try to load the file
      imported_data <- tryCatch({
        read.csv(input$file_authors$datapath, stringsAsFactors = FALSE, encoding = 'UTF-8')
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
        align_to_structure(author_tbl_str, imported_data)
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
      author_data$df_in <- converted_data$data
      if (length(converted_data$missing_cols) > 0) {
        showNotification(
          paste("Missing columns filled with NA:",
                paste(converted_data$missing_cols, collapse = ", ")),
          type = "message")
      }
    })


    # Validation check
    output$validation_check <- renderUI({
      req(author_data$df_out)

      validation_results <- list()

      # use the iv_gen object to validate the dataset inputs
      validation_results$ds_info <- list(
        ds_name = switch(is.null(iv_gen$validate()[[ns('ds_name')]]) + 1, 'invalid name', NULL),
        ds_desc = switch(is.null(iv_gen$validate()[[ns('ds_desc')]]) + 1, 'invalid description', NULL)
      )

      # column-wise validation checks on the author table
      df_out <- author_data$df_out
      validation_results$author_table <- lapply(names(df_out), function(col_name) {
        validate_column(df_out[[col_name]], author_tbl_config[[col_name]])
      })
      names(validation_results$author_table) <- names(df_out)


      # Use a for loop to iterate through validation_results
      issues_output <- list()
      for (input_type in names(validation_results)){
        issue_list <- list()
        for (input_name in names(validation_results[[input_type]])) {
          issues <- validation_results[[input_type]][[input_name]]
          # If there are issues for the current column, add them to the issue list
          if (length(issues) > 0) {
            issue_list <- tagList(issue_list, tags$li(paste0(input_name, ": ", paste(issues, collapse = ", "))))
          }
        }
        if (length(issue_list) > 0) {
          issues_output <- tagList(issues_output,
            tagList(paste0("Issues with ", input_type, " input:"), tags$ul(issue_list))
          )
        }
      }

      # set the color of the header based on the validation results
      shinyjs::toggleClass(id = "val_check_header", class = 'bg-secondary',
        condition = length(issues_output) > 0)

      # TODO:
      # add warning messages before switching tab or saving data

      # show output message
      if (length(issues_output) > 0) {
        tagList(issues_output)
      } else {
        tagList(strong("ALL GOOD :)", style = paste0('color: ', prim_col, ';')))
      }

    })

    # Observe save button
    observeEvent(input$btn_save_aut, {
      data <- author_data$df_out

      # TODO: file download rather than predefined file?
      # Save to CSV
      write.csv(data, file = "author_data.csv", row.names = FALSE)
      showModal(modalDialog(
        title = "Success",
        "Data saved successfully!",
        easyClose = TRUE,
        footer = NULL
      ))
    })


    # Next button
    observeEvent(input$btn_next, {
      #iv_gen$enable()
      nav_select(id = 'tabs', selected = tab_site, session = main_session)
    })

    # Previous button
    observeEvent(input$btn_prev, {
      #iv_gen$enable()
      nav_select(id = 'tabs', selected = tab_start, session = main_session)
    })


    # TODO:
    # additional validations
    # read data from file: append or warn if there is already data?
    # from / to csv or xslx? sheets?
    # more details on validation errors (nr characters, pattern, etc.)
    # clean up, refactor data import and validation checks into functions

    return(
      reactive(
        list(
          author_data = author_data$df_out,
          funding_data = funding_data$df_out,
          ds_data = list(
            ds_name = input$ds_name,
            ds_desc = input$ds_desc,
            ds_access = input$df_access,
            ds_license = input$ds_license,
            ds_embargoed = input$ds_embargoed
          )
        )
      )
    )

  })
}


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




# iv_gen$add_rule("autname_1", shinyvalidate::sv_required())
# iv_gen$add_rule("autmail_1", shinyvalidate::sv_required())
#
# # Keep track of the number of authors
# author_count <- reactiveVal(1)
#
# # Enable delete button only if there are more than 1 authors
# observe({
#   shinyjs::toggleState(id = "del_author_btn", condition = author_count() > 1)
# })
#
# # Add author input if button is clicked
# observeEvent(input$add_author_btn, {
#   author_count(author_count() + 1)
#   author_id <- paste0('aut', author_count())
#   insertUI(
#     selector = "#author_inputs",
#     where = "beforeBegin",
#     ui = div(id = author_id,
#              author_input(author_count()))
#   )
#   iv_gen$add_rule(paste0('autname_',author_count()), shinyvalidate::sv_required())
# })
#
# # Delete author input
# observeEvent(input$del_author_btn, {
#   removeUI(selector = paste0("#aut", author_count()))
#   iv_gen$remove_rules(paste0('autname_',author_count()))
#   author_count(author_count() - 1)
#
# })

# # Dynamic selection of contact person
# output$contact_person <- renderUI({
#   radioButtons("contact_person", NULL,
#                choices = paste("Author", 1:author_count()))
# })


# renderer_email_valid <- "
#   function(instance, td, row, col, prop, value, cellProperties) {
#
#     // Remove any existing tooltips
#     var existingTooltip = document.getElementById('email-tooltip');
#     if (existingTooltip) {
#       document.body.removeChild(existingTooltip);
#     }
#
#     var tooltip = document.createElement('div');
#     tooltip.id = 'email-tooltip';
#     tooltip.innerHTML = 'test tooltip';
#     tooltip.style.display = 'none';
#     tooltip.style.position = 'absolute';
#     tooltip.style.backgroundColor = 'black';
#     tooltip.style.color = 'white';
#     tooltip.style.padding = '5px';
#     tooltip.style.borderRadius = '3px';
#     tooltip.style.zIndex = '1000';
#
#     document.body.appendChild(tooltip);
#
#     if (!/^\\S+@\\S+\\.\\S+$/.test(value)) {
#       td.style.background = 'pink';
#
#       td.addEventListener('mouseover', function(e) {
#         console.log('hover over');
#         tooltip.style.display = 'block';
#         tooltip.style.left = (e.pageX + 10) + 'px';
#         tooltip.style.top = (e.pageY + 10) + 'px';
#      });
#
#     td.addEventListener('mouseout', function() {
#       tooltip.style.display = 'none';
#     });
#
#     } else {
#       td.style.background = '';
#
#       td.addEventListener('mouseover', function(e) {
#           console.log('hover over');
#           tooltip.style.display = 'none';
#        });
#
#       td.addEventListener('mouseout', function() {
#         tooltip.style.display = 'none';
#       });
#     }
#     return td.innerHTML = value;
#   }"

# renderer_mandatory_char <- "
#   function(instance, td, row, col, prop, value, cellProperties) {
#     if (value === null || value === '') {
#       td.style.background = 'pink';
#     } else {
#       td.style.background = '';
#     }
#     return td.innerHTML = value;
#   }"
#
#
#
# renderer_contact_pers <- "
#   function(instance, td, row, col, prop, value, cellProperties) {
#     var data = instance.getDataAtCol(col);
#     var true_count = data.filter(function(val) { return val === true; }).length;
#     console.log(true_count);
#     if (true_count !== 1) {
#       td.style.background = 'pink';
#     } else {
#       td.style.background = '';
#     }
#     Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
#     return td;
#   }"



# output$author_table <- rhandsontable::renderRHandsontable({
#   rhandsontable::rhandsontable(
#     author_data$df_in,
#     rowHeaders = T,
#     contextMenu = FALSE,
#     stretchH = "all",
#     colHeaders = c('Last name', 'First name', 'Email address', 'ORCID',
#                    'Affiliation Name', 'Aff. ROR', 'Aff. department',
#                    'Aff. Address', 'Contact person')) %>%
#     rhandsontable::hot_col('Last name', renderer = renderer_mandatory_char) %>%
#     rhandsontable::hot_col('First name', renderer = renderer_mandatory_char) %>%
#     rhandsontable::hot_col('Email address', renderer = renderer_email_valid) %>%
#     rhandsontable::hot_col("Affiliation Name", renderer = renderer_ror_or_aff_name_addr) %>%
#     rhandsontable::hot_col("Aff. ROR", renderer = renderer_ror_or_aff_name_addr) %>%
#     rhandsontable::hot_col("Aff. Address", renderer = renderer_ror_or_aff_name_addr) %>%
#     rhandsontable::hot_col("Contact person",type = "checkbox",renderer = renderer_contact_pers)
# })

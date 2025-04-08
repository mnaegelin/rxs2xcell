renderer_email_valid <- "
  function(instance, td, row, col, prop, value, cellProperties) {

    // Remove any existing tooltips
    var existingTooltip = document.getElementById('email-tooltip');
    if (existingTooltip) {
      document.body.removeChild(existingTooltip);
    }

    var tooltip = document.createElement('div');
    tooltip.id = 'email-tooltip';
    tooltip.innerHTML = 'test tooltip';
    tooltip.style.display = 'none';
    tooltip.style.position = 'absolute';
    tooltip.style.backgroundColor = 'black';
    tooltip.style.color = 'white';
    tooltip.style.padding = '5px';
    tooltip.style.borderRadius = '3px';
    tooltip.style.zIndex = '1000';

    document.body.appendChild(tooltip);

    if (!/^\\S+@\\S+\\.\\S+$/.test(value)) {
      td.style.background = 'pink';

      td.addEventListener('mouseover', function(e) {
        console.log('hover over');
        tooltip.style.display = 'block';
        tooltip.style.left = (e.pageX + 10) + 'px';
        tooltip.style.top = (e.pageY + 10) + 'px';
     });

    td.addEventListener('mouseout', function() {
      tooltip.style.display = 'none';
    });

    } else {
      td.style.background = '';

      td.addEventListener('mouseover', function(e) {
          console.log('hover over');
          tooltip.style.display = 'none';
       });

      td.addEventListener('mouseout', function() {
        tooltip.style.display = 'none';
      });
    }
    return td.innerHTML = value;
  }"

validateEmail <- function(email) {
  grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", email)
}

renderer_mandatory_char <- "
  function(instance, td, row, col, prop, value, cellProperties) {
    if (value === null || value === '') {
      td.style.background = 'pink';
    } else {
      td.style.background = '';
    }
    return td.innerHTML = value;
  }"

check_missing <- function(data){
  ((is.na(data) | data == ''))
}

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

renderer_contact_pers <- "
  function(instance, td, row, col, prop, value, cellProperties) {
    var data = instance.getDataAtCol(col);
    var true_count = data.filter(function(val) { return val === true; }).length;
    console.log(true_count);
    if (true_count !== 1) {
      td.style.background = 'pink';
    } else {
      td.style.background = '';
    }
    Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
    return td;
  }"

dataset_server <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # mock event to close ROR tab on start of app
    observeEvent(1,{
      accordion_panel_close(id = 'ror_search_acc', values = TRUE)
    }, ignoreNULL = FALSE) # # to fire the event at startup

    iv_gen <- shinyvalidate::InputValidator$new()

    # DATASET INPUT --------------------------------------------------------------
    # add validator rules for dataset inputs
    iv_gen$add_rule("ds_name", shinyvalidate::sv_required())
    iv_gen$add_rule("ds_name", max_char_limit, limit = 64)
    iv_gen$add_rule("ds_desc", shinyvalidate::sv_required())


    # ROR SEARCH -----------------------------------------------------------------
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
          res_df <- data.frame(ROR = ror_data$items$id, Name = res_names,
                               Location = res_locs)

          #updateSelectizeInput(session, "result_choice", choices = res_names)
          output$ror_results <- DT::renderDT({
            DT::datatable(res_df,
                          style = 'default',
                          rownames = FALSE, selection = "none",
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


    # AUTHOR RHANDSONTABLE -------------------------------------------------------
    # Initialize data frame
    author_data <- reactiveValues(
      df_in = data.frame(
        lastname = character(1),
        firstname = character(1),
        email = character(1),
        orcid = character(1),
        aff_name = character(1),
        aff_rorid = character(1),
        aff_dep = character(1),
        aff_address = character(1),
        contactperson = logical(1),
        stringsAsFactors = FALSE),
      df_out = NULL)

    # Render editable table
    output$author_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        author_data$df_in,
        rowHeaders = T,
        stretchH = "all",
        colHeaders = c('Last name', 'First name', 'Email address', 'ORCID',
                       'Affiliation Name', 'Aff. ROR', 'Aff. department',
                       'Aff. Address', 'Contact person')) %>%
        rhandsontable::hot_col('Last name', renderer = renderer_mandatory_char) %>%
        rhandsontable::hot_col('First name', renderer = renderer_mandatory_char) %>%
        rhandsontable::hot_col('Email address', renderer = renderer_email_valid) %>%
        rhandsontable::hot_col("Affiliation Name", renderer = renderer_ror_or_aff_name_addr) %>%
        rhandsontable::hot_col("Aff. ROR", renderer = renderer_ror_or_aff_name_addr) %>%
        rhandsontable::hot_col("Aff. Address", renderer = renderer_ror_or_aff_name_addr) %>%
        rhandsontable::hot_col("Contact person",type = "checkbox",renderer = renderer_contact_pers) %>%
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })

    # Update data frame on table edit
    observeEvent(input$author_table, {
      author_data$df_out <- rhandsontable::hot_to_r(input$author_table)
    })

    # Observe add row button
    observeEvent(input$add_author_btn, {
      new_row <- data.frame(
        lastname = "", firstname = "", email = "", orcid = "", aff_name = "",
        aff_rorid = "", aff_dep = "", aff_address = "", contactperson = FALSE,
        stringsAsFactors = FALSE
      )
      author_data$df_in <- rbind(author_data$df_out, new_row)
    })

    # Observe delete row button
    observeEvent(input$del_author_btn, {
      req(nrow(author_data$df_out) > 1)
      author_data$df_in <- author_data$df_out[-nrow(author_data$df_out), ]
    })


    output$testing2 <- renderPrint({

      iv_gen$validate()[[ns('ds_name')]]

    })

    output$validation_check <- renderUI({
      req(author_data$df_out)

      df_out <- author_data$df_out
      val_issues <- c()

      # check dataset inputs
      ds_issues <- c()
      if (!is.null(iv_gen$validate()[[ns('ds_name')]])) {
        ds_issues <- c(ds_issues, 'name')
      }
      if (!is.null(iv_gen$validate()[[ns('ds_desc')]])) {
        ds_issues <- c(ds_issues, 'description')
      }
      if (length(ds_issues) > 0) {
        msg <- paste("missing dataset", paste(ds_issues, collapse = ", "))
        val_issues <- c(val_issues, msg)
      }

      # check email
      if (any(!validateEmail(df_out$email))) {
        val_issues <- c(val_issues, 'invalid email')
      }

      # check mandatory
      mandatory_author_cols <- c('lastname', 'firstname')
      has_missing <- apply(df_out[,mandatory_author_cols], 2, check_missing)
      columns_with_missing <- names(has_missing[has_missing])
      if (length(columns_with_missing) > 0) {
        msg <- paste("missing values in columns:", paste(columns_with_missing, collapse = ", "))
        val_issues <- c(val_issues, msg)
      }

      # output
      if (length(val_issues) > 0) {
        tagList(
          "The following issues were detected in the provided metadata:",
          tags$ul(
            lapply(val_issues, tags$li)
          ))
      } else {
        strong("ALL GOOD :)", style=paste0('color: ',prim_col,';'))
      }

    })


    # Observe import button
    # Observe file input for importing CSV
    observeEvent(input$file_authors, {
      req(input$file_authors)
      imported_data <- read.csv(input$file_authors$datapath, stringsAsFactors = FALSE)
      req_cols <- c('lastname', 'firstname', 'email', 'orcid', 'aff_name',
                    'aff_rorid', 'aff_dep', 'aff_address', 'contactperson')
      if (all(req_cols %in% colnames(imported_data))) {
        imported_data <- imported_data %>% # ensure correct column order
          dplyr::select(dplyr::all_of(req_cols))
        author_data$df_in <- imported_data
      } else {
        showModal(modalDialog(
          title = "Validation Error",
          "The CSV file does not have the required columns.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })

    # Observe save button
    observeEvent(input$save_authors_btn, {
      data <- author_data$df_out

      # Validation checks
      if (any(!sapply(data$firstname, is.character)) || any(!sapply(data$lastname, is.character))) {
        showModal(modalDialog(
          title = "Validation Error",
          "First name and last name must be characters.",
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      }

      if (any(!validateEmail(data$email))) {
        showModal(modalDialog(
          title = "Validation Error",
          "Invalid email addresses.",
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      }

      if (sum(data$contactperson) != 1) {
        showModal(modalDialog(
          title = "Validation Error",
          "Exactly one contact person must be selected.",
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      }

      # Save to CSV
      write.csv(data, file = "author_data.csv", row.names = FALSE)
      showModal(modalDialog(
        title = "Success",
        "Data saved successfully!",
        easyClose = TRUE,
        footer = NULL
      ))
    })

    # TODO:
    # validations
    # read data from file: append?
    # next submits data? needed? or just validation checks as well?
    # from / to csv or xslx? sheets? explanations? vs universal


    # Next button
    observeEvent(input$nex_btn_general, {
      iv_gen$enable()
      nav_select(id = 'tabs', selected = "Study Details", session = main_session)
    })

    # Previous button
    observeEvent(input$btn_prev_general, {
      iv_gen$enable()
      nav_select(id = 'tabs', selected = tab_start, session = main_session)
    })


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

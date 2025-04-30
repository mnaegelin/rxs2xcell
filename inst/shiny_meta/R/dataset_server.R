
# TODO: renderers for multicolumn checks?
# TODO: multicol validations?
# renderer_ror_or_aff_name_addr <- "
#   function(instance, td, row, col, prop, value, cellProperties) {
#     var data = instance.getDataAtRow(row);
#     var aff_name = data[4];
#     var aff_address = data[7];
#     var aff_rorid  = data[5];
#     if (((aff_name === null || aff_name === '') && (aff_address === null || aff_address === '')) && (aff_rorid === null || aff_rorid === '')) {
#       td.style.background = 'pink';
#     } else if ((aff_name === null || aff_name === '') || (aff_address === null || aff_address === '')) {
#       if (aff_rorid === null || aff_rorid === '') {
#         td.style.background = 'pink';
#       } else {
#         td.style.background = '';
#       }
#     } else {
#       td.style.background = '';
#     }
#     return td.innerHTML = value;
#   }"


# helper to create a numbered list of authors for modal dialog with checkboxes
get_author_choices <- function(author_df){
  cb_names <- paste0("Author Nr. ", rownames(author_df), ": ",
                     author_df$last_name, ", ",
                     author_df$first_name)
  cb_names <- gsub(": ,", ": [last name],", cb_names)
  cb_names <- gsub(", $", ", [first name]", cb_names)
  cb_vals <- rownames(author_df)

  return(stats::setNames(cb_vals, cb_names))
}

get_funding_choices <- function(funding_df){
  cb_names <- paste0("Funding Inst. Nr. ", rownames(funding_df))
  cb_vals <- rownames(funding_df)

  return(stats::setNames(cb_vals, cb_names))
}

ror_api_request <- function(search_string, country_code){
  search_url <- sprintf(
    'https://api.ror.org/v2/organizations?query=%s&filter=country.country_code:%s',
    URLencode(search_string), country_code)
  ror_res <- httr::GET(search_url, httr::timeout(5))

  if (httr::status_code(ror_res) == 200) {
    ror_data <- jsonlite::fromJSON(rawToChar(ror_res$content))

    if (ror_data$number_of_results > 0) {
      # get the names (assuming that there is always exactly one ror_display name)
      res_names <- ror_data$items$names %>%
        dplyr::bind_rows() %>%
        dplyr::filter(grepl('ror_display', types)) %>%
        dplyr::rename(Name = value) %>%
        dplyr::select(Name)

      # get the locations
      res_locs <- ror_data$items$locations %>%
        dplyr::bind_rows() %>%
        dplyr::pull(geonames_details) %>%
        tidyr::unite(col = 'Location', name, country_name, sep = ', ', remove = FALSE) %>%
        dplyr::rename(city = name) %>%
        dplyr::select(Location, country_code, city)

      res_df <- cbind(res_names, res_locs)

      # get the ror ids and corresponding hyperlinks
      res_df <- res_df %>%
        dplyr::mutate(
          RORID = gsub('https://ror.org/', '', ror_data$items$id),
          Link = paste0("<a href='",ror_data$items$id,"' target='_blank'>",ror_data$items$id,"</a>")
        )
      return(res_df)

    } else {
      showNotification("No ROR results found. Try again.", type = "message")
      return(NULL)
    }

  } else {
    showNotification("ROR API request failed. Try again.", type = "error")
    return(NULL)
  }
}



orcid_api_request <- function(search_string = NULL, last_name = NULL, first_name = NULL){
  # query by search string
  if (!is.null(search_string)) {
    # query either orcid or by names, depending on the format of the search_string
    if (grepl("^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$", search_string)) {
      # query by orcid
      query <- sprintf('?q=orcid:%s',search_string)
    } else {
      # query by names
      search_terms <- URLencode(gsub(" ", "+AND+", search_string))
      query <- sprintf('?q=given-and-family-names:(%s)+OR+other-names:(%s)',
                       search_terms, search_terms)
    }
    print(query)

    search_url <- paste0(
      'https://pub.orcid.org/v3.0/csv-search/', query,
      '&fl=family-name,given-names,email,orcid,current-institution-affiliation-name,other-names',
      '&rows=50')

  # OR query by first and last names explicitly
  } else if (!(is.null(last_name) || last_name == "" || is.na(last_name)) ||
            !(is.null(first_name) || first_name == "" || is.na(first_name))) {

    query_ln <- ifelse(!(is.null(last_name) || last_name == "" || is.na(last_name)),
                       sprintf('(family-name:(%s))', URLencode(gsub(" ", "+AND+", last_name))),
                       "")
    query_fn <- ifelse(!(is.null(first_name) || first_name == "" || is.na(first_name)),
                       sprintf('(given-names:(%s))', URLencode(gsub(" ", "+AND+", first_name))),
                       "")
    query <- paste0('?q=', query_ln, ifelse(nchar(query_ln)>0&&nchar(query_fn)>0, '+AND+', ""), query_fn)
    print(query)

    search_url <- paste0(
      'https://pub.orcid.org/v3.0/csv-search/', query,
      '&fl=family-name,given-names,email,orcid,current-institution-affiliation-name,other-names',
      '&rows=50')

  } else {
    # no valid search terms provided, abort
    # showNotification("Could not generate query. Provide either a search_string or last_name, first_name arguments.", type = "error")
    return(NULL)
  }

  orcid_res <- httr::GET(search_url, httr::timeout(5))

  if (httr::status_code(orcid_res) == 200) {
    orcid_data <- read.table(text = rawToChar(orcid_res$content),
                             sep =",", header = TRUE,
                             stringsAsFactors = FALSE, allowEscapes = TRUE)

    if (nrow(orcid_data) > 0) {
      orcid_data <- orcid_data %>%
        dplyr::rename(
          last_name = 'family.name',
          first_name = 'given.names',
          orcid_id = 'orcid',
          org_name = 'current.institution.affiliation.name',
          other_names = 'other.names') %>%
        # only use the first entry for email and affiliation
        tidyr::separate(email, into = c("email"), sep = ",(?!\\s)", extra = "drop") %>%
        tidyr::separate(org_name, into = c("org_name"), sep = ",(?!\\s)", extra = "drop") %>%
        dplyr::mutate(
          # create orcid hyperlinks
          orcid = paste0("<a href='https://orcid.org/", orcid_id, "' target='_blank'>",orcid_id,"</a>"))

      return(orcid_data)

    } else {
      showNotification("No ROR results found. Try again.", type = "message")
      return(NULL)
    }

  } else {
    showNotification("ROR API request failed. Try again.", type = "error")
    return(NULL)
  }

}





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
  org_name = list(type = 'text', required = TRUE),
  org_rorid = list(type = 'character', required = FALSE),
  aff_dep = list(type = 'character', required = FALSE),
  aff_street = list(type = 'character', required = FALSE),
  aff_plz = list(type = 'character', required = FALSE),
  aff_city = list(type = 'character', required = FALSE),
  org_country = list(type = 'autocomplete', required = TRUE, options = names(countries_list)),
  contactperson = list(type = 'checkbox', required = TRUE, max_checks = 1)
)
author_tbl <- list(
  tbl_str = author_tbl_str,
  tbl_config = author_tbl_config
)



funding_tbl_str <- data.frame(
  inst_name = character(1),
  inst_rorid = character(1),
  inst_city = character(1),
  inst_country = character(1),
  grantnr = character(1),
  stringsAsFactors = FALSE)

funding_tbl_config <- list(
  # NOTE: order matters for colHeaders (needs to be same as in df)
  colHeaders = c(inst_name = 'Funding Institution', inst_rorid = 'Inst. RORID',
                 inst_city = 'City', inst_country = 'Country', grantnr = 'Grant Nr.'),
  inst_name = list(type = 'text', required = TRUE),
  inst_rorid = list(type = 'character', required = FALSE),
  inst_city = list(type = 'character', required = FALSE),
  inst_country = list(type = 'autocomplete', required = FALSE, options = names(countries_list)),
  grantnr = list(type = 'character', required = TRUE)
)

funding_tbl <- list(
  tbl_str = funding_tbl_str,
  tbl_config = funding_tbl_config
)


# SERVER -----------------------------------------------------------------------
dataset_server <- function(id, main_session,
                           countries_list, author_tbl, funding_tbl) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # mock event to close ROR tab on start of app
    observeEvent(1,{
      accordion_panel_close(id = 'search_tools', values = TRUE)
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
      shinyjs::toggleState(id = "btn_ror_search",
                           condition = (!(input$ror_search_string=="") &&
                                        !(input$ror_search_country=="")))
    })

    # ror_df: a reactive updated only in the event of the search button being clicked
    ror_df <- eventReactive(input$btn_ror_search, {
      req(input$ror_search_country, input$ror_search_string)

      # run the ROR API request with the input search string
      ror_api_request(search_string = input$ror_search_string,
                      country_code = input$ror_search_country)
    })

    # render instructions
    output$ror_instr <- renderUI({
      if (is.null(ror_df())) {
        tags$i("Run ROR search first...")
      } else {
        tags$i("Click on a row to select and transfer the ROR data to the tables below.")
      }
    })

    # render ROR DT
    output$ror_results <- DT::renderDT({
      validate(need(!is.null(ror_df()), "No data to show"))
      DT::datatable(ror_df() %>% dplyr::select(Link, RORID, Name, Location),
                    style = 'default',
                    rownames = FALSE,
                    selection = "single",
                    escape = FALSE,
                    options = list(pageLength = 5))
    })

    # observe ROR row selection: open modal
    observeEvent(input$ror_results_rows_selected, {
      showModal(modalDialog(
        title = "Transfer ROR data",
        tagList(
          p("Do you want to transfer the selected ROR data to the author and/or funding table?"),
          checkboxGroupInput(
            ns("selected_authors"),
            label = "Select authors to update:",
            choices = get_author_choices(author_data_out())#author_data$df_in
          ),
          checkboxGroupInput(
            ns("selected_funders"),
            label = "Select funding institutions to update:",
            choices = get_funding_choices(funding_data_out())
          )
        ),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("btn_trans_ror"), "Transfer")
        )
      ))
    })

    # transfer ROR data to authors on confirm transfer
    observeEvent(input$btn_trans_ror, {
      # update the authors table
      if (!is.null(input$selected_authors) && !is.null(input$ror_results_rows_selected)) {
        # get the ROR data of the selected row (NOTE that only 1 row can be selected)
        selected_ror_data <- ror_df()[input$ror_results_rows_selected, c("RORID", "Name", "city", 'country_code')]
        selected_ror_data$Name <- gsub('\n','<br>',stringr::str_wrap(selected_ror_data$Name, width = 50))
        selected_ror_data$country_code <- names(countries_list[countries_list == selected_ror_data$country_code])
        # update the author data table for the selected authors
        current_df <- author_data_out()
        current_df[input$selected_authors, c("org_rorid", "org_name", "aff_city", "org_country")] <- selected_ror_data
        author_data_in(current_df)
      }
      # update the funding table
      if (!is.null(input$selected_funders) && !is.null(input$ror_results_rows_selected)) {
        # get the ROR data of the selected row (NOTE that only 1 row can be selected)
        selected_ror_data <- ror_df()[input$ror_results_rows_selected, c("RORID", "Name", "city", 'country_code')]
        # selected_ror_data$Name <- gsub('\n','<br>',stringr::str_wrap(selected_ror_data$Name, width = 50))
        selected_ror_data$country_code <- names(countries_list[countries_list == selected_ror_data$country_code])
        # update in the funding data table for the selected funders
        current_df <- funding_data_out()
        current_df[input$selected_funders, c("inst_rorid", "inst_name", "inst_city", "inst_country")] <- selected_ror_data
        funding_data_in(current_df)
      }
      # close the modal
      removeModal()
    })



    # ORCID SEARCH ---------------------------------------------------------------
    # toggle search button: only enable if we have a country and search string
    observe({
      shinyjs::toggleState(id = "btn_orcid_search",
                           condition = !(input$orcid_search_string==""))
    })

    # orcid_df: a reactive updated only in the event of the search button being clicked
    orcid_df <- reactiveVal()

    observeEvent(input$btn_orcid_search, {
      req(input$orcid_search_string)

      # run the ORCID API request with the input search string
      res_df <- orcid_api_request(search_string = input$orcid_search_string)
      res_df$search_terms <- input$orcid_search_string

      # update the reactiveVal
      orcid_df(res_df)
    })

    output$testing <- renderPrint({
      print(input$orcid_search_string)
      print(orcid_df())
    })

    observeEvent(input$btn_orcid_tbl, {
      current_df <- author_data_out()
      results_combined <- list()
      for (row in 1:nrow(current_df)){
        # short break every 10th run to avoid crashing the API
        if (row > 10 && row %% 10 == 0) {Sys.sleep(0.6)}

        # run a name based api request for each row
        last_name <- current_df$last_name[row]
        first_name <- current_df$first_name[row]
        results <- orcid_api_request(last_name = last_name, first_name = first_name)
        results_combined[[paste(last_name, first_name)]] <- results
      }
      # combine the results and update the reactive
      res_df <- dplyr::bind_rows(results_combined, .id = 'search_terms')
      orcid_df(res_df)
    })

    # render instructions
    output$orcid_instr <- renderUI({
      if (is.null(orcid_df())) {
        tags$i("Run ORCID search first...")
      } else {
        tags$i("Click on a row to select and transfer the ORCID data to the table below.
                Note that existing email and affiliation data will not be overwritten.")
      }
    })

    # render ORCID DT
    output$orcid_results <- DT::renderDT({
      validate(need(!is.null(orcid_df()), "No data to show"))
      DT::datatable(orcid_df() %>% dplyr::select(search_terms, last_name, first_name, email, orcid, org_name, other_names),
                    extensions = 'RowGroup',
                    style = 'default',
                    rownames = FALSE,
                    selection = "single",
                    escape = FALSE,
                    options = list(pageLength = 5,
                                   rowGroup = list(dataSrc = 0)))
    })

    # observe ORCID row selection: open modal
    observeEvent(input$orcid_results_rows_selected, {
      showModal(modalDialog(
        title = "Transfer ORCID data",
        tagList(
          p("Do you want to transfer the selected ORCID data to the author table?"),
          radioButtons(
            ns("sel_author_orc"),
            label = "Select author to update:",
            choices = c(get_author_choices(author_data_out()), "Add new author" = "new"),
            selected = "new"
          )
        ),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("btn_trans_orcid"), "Transfer")
        )
      ))
    })

    # transfer ORCID data to author table on confirm transfer
    observeEvent(input$btn_trans_orcid, {
      if (!is.null(input$sel_author_orc) && !is.null(input$orcid_results_rows_selected)) {
        # get the ORCID data of the selected row (NOTE: only 1 row can be selected)
        sel_orcid_data <- orcid_df()[input$orcid_results_rows_selected,]
        sel_orcid_data$org_name <- gsub('\n','<br>',stringr::str_wrap(sel_orcid_data$org_name, width = 50))
        # update in the author data table for the selected author (NOTE: only 1 can be selected)
        current_df <- author_data_out()
        row <- ifelse(input$sel_author_orc == "new", nrow(current_df) + 1, input$sel_author_orc)
        current_df[row, c("last_name", "first_name", "orcid")] <- sel_orcid_data[, c("last_name", "first_name", "orcid_id")]
        # only update email if the field is NA or empty
        if (is.na(current_df[row, "email"]) || current_df[row, "email"] == "") {
          current_df[row, "email"] <- sel_orcid_data$email
        }
        # only update org_name if the field is NA or empty
        if (is.na(current_df[row, "org_name"]) || current_df[row, "org_name"] == "") {
          current_df[row, "org_name"] <- sel_orcid_data$org_name
        }
        author_data_in(current_df)
      }

      # close the modal
      removeModal()
    })

    # TODO: logic if update from author table

    # output$testing <- renderPrint({
    #   funding_data$df_in
    # })



    # AUTHOR RHANDSONTABLE -----------------------------------------------------
    # initialize reactiveVal (responding to add/delete row, ror/orcid transfer, file upload)
    author_data_in <- reactiveVal(author_tbl$tbl_str)

    # render editable table
    output$author_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        author_data_in(),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        height = 150,
        colHeaders = unname(author_tbl$tbl_config$colHeaders)) %>%
      # custom validation check renderers for all cols based on tbl_config
      purrr::reduce(
        names(author_tbl$tbl_config$colHeaders), # names in df
        function(ht, col) {
          config <- author_tbl$tbl_config[[col]]
          colName <- author_tbl$tbl_config$colHeaders[col] # name in ht
          hot_col_wrapper(ht, colName, config)
        },
        .init = .
      )
    })

    # create dataframe reactive to hot updates
    author_data_out <- reactive({
      rhandsontable::hot_to_r(input$author_table)
    })

    # observe add row button
    observeEvent(input$btn_add_author, {
      new_row <- author_tbl$tbl_str
      current_df <- author_data_out()
      current_df[nrow(current_df)+1,] <- new_row
      author_data_in(current_df)
    })

    # observe delete row button
    observeEvent(input$btn_del_author, {
      req(nrow(author_data_out()) > 1)
      current_df <- author_data_out()
      current_df <- current_df[-nrow(current_df),]
      author_data_in(current_df)
    })

    # observe import data button
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
        align_to_structure(author_tbl$tbl_str, imported_data)
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
      #author_data$df_in <- converted_data$data
      author_data_in(converted_data$data)
      if (length(converted_data$missing_cols) > 0) {
        showNotification(
          paste("Missing columns filled with NA:",
                paste(converted_data$missing_cols, collapse = ", ")),
          type = "message")
      }
    })


    # FUNDING RHANDSONTABLE ----------------------------------------------------
    # funding_data <- reactiveValues(
    #   df_in = funding_tbl_str,
    #   df_out = NULL)
    # initialize reactiveVal (responding to add/delete row, ror transfer, file upload)
    funding_data_in <- reactiveVal(funding_tbl$tbl_str)

    # Render editable table
    output$funding_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        funding_data_in(),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        height = 150,
        colHeaders = unname(funding_tbl$tbl_config$colHeaders)) %>%
      # custom validation checks renderers for all cols based on tbl_config
      purrr::reduce(
        names(funding_tbl$tbl_config$colHeaders), # names in df
        function(ht, col) {
          config <- funding_tbl$tbl_config[[col]]
          colName <- funding_tbl$tbl_config$colHeaders[col] # name in ht
          hot_col_wrapper(ht, colName, config)
        },
        .init = .
      )
    })

    # create dataframe reactive to hot updates
    funding_data_out <- reactive({
      rhandsontable::hot_to_r(input$funding_table)
    })

    # observe add row button
    observeEvent(input$btn_add_fund, {
      new_row <- funding_tbl$tbl_str
      current_df <- funding_data_out()
      current_df[nrow(current_df)+1,] <- new_row
      funding_data_in(current_df)
    })

    # observe delete row button
    observeEvent(input$btn_del_fund, {
      req(nrow(funding_data_out()) > 1)
      current_df <- funding_data_out()
      current_df <- current_df[-nrow(current_df),]
      funding_data_in(current_df)
    })


    # Observe import data button
    # TODO:



    # RELATED RESOURCES --------------------------------------------------------
    doi_data <- reactiveValues(
      df_in = data.frame(
        DOI = character(0),
        Citation = character(0),
        XCELLID = character(0),
        stringsAsFactors = FALSE
      ),
      df_out = NULL)



    output$rel_resources <- DT::renderDT({
      # a DT with a column of delete buttons
      deleteButtonColumn(doi_data$df_in, 'delbtn', ns)
    })

    # observe delete row events
    observeEvent(input$deletePressed, {
      rowNum <- parseDeleteEvent(input$deletePressed)
      # Delete the row from the data frame
      doi_data$df_in <- doi_data$df_in[-rowNum,]
    })

    observeEvent(input$btn_add_pub,{
      # TODO: logic if both are provided? (disable submit button?)
      # TODO: what about XCELL datasets?
      # DOI API REQUEST
      if (!is.null(input$doi) && input$doi != "") {
        search_url <- sprintf(
          'https://citation.doi.org/format?doi=%s&style=apa&lang=en-US',
          URLencode(input$doi)
        )
        doi_res <- httr::GET(search_url, httr::timeout(5))
        if (httr::status_code(doi_res) == 200) {
          doi_content <- httr::content(doi_res, as = "text", encoding = "UTF-8")
          df_doi <- doi_data$df_in
          df_doi <- rbind(df_doi, data.frame(
            DOI = input$doi,
            Citation = doi_content,
            XCELLID = NA,
            stringsAsFactors = FALSE
          ))
          doi_data$df_in <- df_doi
          # clear input field
          updateTextInput(session, "doi", value = "")
        } else {
          showNotification("Failed to fetch citation from DOI. Please check the DOI.", type = "error")
        }
      # COPY CITATION TEXT TO TABLE
      } else  if (!is.null(input$citation) && input$citation != "") {
        df_doi <- doi_data$df_in
        df_doi <- rbind(df_doi, data.frame(
          DOI = NA,
          Citation = input$citation,
          XCELLID = NA,
          stringsAsFactors = FALSE
        ))
        doi_data$df_in <- df_doi
        # clear input field
        updateTextInput(session, "citation", value = "")
      }
    })


#
#     #Form for data entry
#     entry_form <- function(button_id){
#       showModal(
#         modalDialog(
#           div(id=ns("entry_form"),
#               fluidPage(
#                 textInput(ns("enter_doi"), "Enter DOI:", placeholder = "e.g., 10.3389/fpls.2016.00781"),
#                 textAreaInput(ns("enter_citation"), "Or enter full citation", "",
#                               placeholder = "e.g., von Arx, G. et al., Quantitative Wood Anatomy—Practical Guidelines. Front. Plant Sci. 7, 781 (2016)."),
#                 textInput(ns("enter_xcellid"), "OR XCELL ID", placeholder = "TO BE IMPLEMENTED"),
#                 actionButton(ns("btn_sub_rel"), "Submit", class = "btn btn-info")
#               )
#           )
#         )
#       )
#     }


    validation_results <- reactiveValues()

    # Validation checks --------------------------------------------------------
    output$validation_check <- renderUI({
      # req(author_data$df_in)
      #
      #validation_results <- list()

      # use the iv_gen object to validate the dataset inputs
      validation_results$ds_info <- list(
        ds_name = switch(is.null(iv_gen$validate()[[ns('ds_name')]]) + 1, 'invalid name', NULL),
        ds_desc = switch(is.null(iv_gen$validate()[[ns('ds_desc')]]) + 1, 'invalid description', NULL),
        ds_lic = switch((input$ds_access == 'public' && is.null(input$ds_license)) + 1, 'missing license', NULL)
      )

      # column-wise validation checks on the author table
      df_aut <- author_data_out()
      validation_results$author_table <- sapply(
        colnames(df_aut),
        function(col_name) {
          validate_column(df_aut[[col_name]], author_tbl$tbl_config[[col_name]])},
        simplify = FALSE, USE.NAMES = TRUE)

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
    # observeEvent(input$btn_save_aut, {
    #   data <- author_data$df_out
    #
    #   # TODO: file download rather than predefined file?
    #   # Save to CSV
    #   write.csv(data, file = "author_data.csv", row.names = FALSE)
    #   showModal(modalDialog(
    #     title = "Success",
    #     "Data saved successfully!",
    #     easyClose = TRUE,
    #     footer = NULL
    #   ))
    # })


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
      # reactive(
        list(
          input_meta = list(
            ds_data = list(
              ds_name = reactive(input$ds_name),
              ds_desc = reactive(input$ds_desc),
              ds_access = reactive(input$df_access),
              ds_license = reactive(input$ds_license),
              ds_embargoed = reactive(input$ds_embargoed)
            ),
            author_data = author_data_out,
            funding_data = funding_data_out
          ),
          val_check = validation_results
        )
      #)
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

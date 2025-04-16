dataset_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(

    # sidebar ----
    sidebar = sidebar(
      title = "Instructions",

      card(
        card_header(
          class = 'bg-secondary',
          'Input data'),
        "If you would like to continue from a partially completed metadata
        export, you can load it here.",
        fileInput(ns("file_upload"), "Load metadata from file (json)", accept = ".json")
      ),

      hr(),
      tags$ol(
        class = 'custom-indent',
        tags$li("Please provide a name and description of your dataset.")),
      hr(),
      tags$ol(
        class = 'custom-indent', start =2,
        tags$li("Please list all authors (data owners) of the dataset. Use the
              ROR search tool to find the Research Organization Registry ID of
              an affiliation.")
      ),
      card(
        class = 'card-note',
        card_header(
          class = 'bg-primary',
          span(icon("exclamation", style = "color: white"),'Note')),
        "The order of authors provided here will be used as
        the order of authorship."
      ),
      hr(),
      card(
        class = 'card-note',
        card_header(
          id = ns('val_check_header'),
          class = 'bg-primary',
          span(icon("exclamation", style = "color: white"),'Validation check summary')),
        uiOutput(ns('validation_check')),
      ),
      hr(),
      layout_columns(
        actionButton(ns('btn_prev'), 'Previous', icon = icon('angle-double-left')),
        actionButton(ns('btn_next'), 'Next', icon = icon('angle-double-right'))
      ),
      verbatimTextOutput(ns("testing")),
    ),


    # main content ----
    accordion(
      open = c('Dataset: General Info','Dataset: Authors & Funding'),

      ## Dataset info
      accordion_panel(
        "Dataset: General Info",
        layout_columns(
          textInput(ns("ds_name"), "Dataset name", value = NA,
                    placeholder = "Specify a name for your dataset (max 64 char.)"),
          textAreaInput(ns("ds_desc"), "Description", rows = 4,
                        placeholder = "Provide a brief description of your dataset")
        ),
        layout_columns(
          radioButtons(ns("ds_access"), "Dateset access rights",
                       choices = c("Private" = "private", "Public" = "public"),
                       selected = "private"),

          div(
            # Conditional panel for public dataset: license
            conditionalPanel(
              condition = "input.ds_access == 'public'",
              selectInput(ns("ds_license"),
                          "Select a license for the public dataset:",
                          choices = c("CC BY 4.0", "CC BY-SA 4.0", "CC BY-NC 4.0", "CC BY-NC-SA 4.0"),
                          selectize = TRUE,
                          selected = "CC BY 4.0"),
              ns=NS(id)),
            # Conditional panel for private dataset: Embargo date
            conditionalPanel(
              condition = "input.ds_access == 'private'",
              dateInput(ns("ds_embargoed"),
                        "If applicable, select an embargo date:",
                        value = as.Date(NA)),
              ns=NS(id))
          )
        ),
      ),

      ## Authors
      accordion_panel(
        'Dataset: Authors & Funding',

        ## ROR search tool
        accordion(
          id = ns("ror_search_acc"),
          class = "accordion-tert",
          open = FALSE, # NOTE: does not work, fixed with panel_close event in server
          accordion_panel(
            'ROR search tool',
            layout_columns(
              card(
                selectInput(ns("country_code"), "Select country:",
                            choices = c(Choose='', get_country_codes()),
                            selectize = TRUE),
                textInput(ns("search_string"), "Enter search string:"),
                actionButton(ns("search_ror"), "Search for ROR", class = "btn btn-info")
                #shiny::selectizeInput("result_choice", "Select Result:", choices = NULL)
              ),
              card(
                h6('ROR search results:'),
                DT::DTOutput(ns("ror_results")),
                max_height = '350px'
              ),
              col_widths = c(3,9)
            )
          )
        ),

        ## Authors input table
        hr(),
        h5('Author information:'),

        div(style='float: right',
            fileInput(ns('file_authors'), "Load author data from file", accept = ".csv")),

        div(style='display: flex; justify-content: flex-start; margin-top: 30pt',
            actionButton(ns("add_author_btn"), "Add author", style = "width: 120px",
                         class = "btn btn-secondary"),
            actionButton(ns("del_author_btn"), "Delete author", style = "width: 120px",
                         class = "btn btn-danger")),

        br(),

        rhandsontable::rHandsontableOutput(ns("author_table")),
        br(),

        actionButton(ns('btn_save_aut'), "Save author data", icon = icon('save')),

        hr(),

        ## Funding info
        h5('Funding information:'),

        # another way to o
        div(style="display: flex; justify-content: space-between; align-items: center;",
          div(style="display: flex; gap: 10px;",
             actionButton(ns("add_fund_btn"), "Add funding", style = "width: 120px",
                          class = "btn btn-secondary"),
             actionButton(ns("del_fund_btn"), "Delete funding", style = "width: 120px",
                          class = "btn btn-danger")),

            div(fileInput(ns('file_funding'), "Load funding data from file", accept = ".csv"), style = "margin-left: auto; margin-bottom: 0;")
        ),

        rhandsontable::rHandsontableOutput(ns("funding_table")),
        br(),

        actionButton(ns('btn_save_fund'), "Save funding data", icon = icon('save')),

      )
    )

  ) # end of layout_sidebar
}










# card(
#   class="border border-0",
#   card_body(
#     fillable = FALSE,
#     actionButton("add_author_btn", "Add author", style = "width: 100px",
#                  class = "btn btn-primary"),
#     actionButton("del_author_btn", "Delete author", style = "width: 100px",
#                  class = "btn btn-danger"),
#     actionButton('save_authors_btn', "Save data", icon = icon('save'), style = "width: 100px"),
#
#   )
# )


# layout_column_wrap(
#   card(
#     # dynamic contact person
#     h5('Contact person'),
#     uiOutput('contact_person')
#   ),
#   card(
#     h5('Author details'),
#     # first author (we always require at least one .author)
#     author_input(1),
#     # the dynamic author inputs
#     uiOutput("author_inputs")
#   ),
#   width = NULL,
#   style = css(grid_template_columns = "1fr 3fr")
# ),


dataset_ui <- function(id, countries_list) {
  ns <- NS(id)

  layout_sidebar(

    # sidebar ----
    sidebar = sidebar(
      title = "Instructions",

      tags$ol(
        class = 'custom-indent',
        tags$li("Please provide a name and description for your dataset and specify access rights.")
      ),
      hr(),
      tags$ol(
        class = 'custom-indent', start = 2,
        tags$li("List all authors (data owners) of the dataset. You can use the
                ROR and ORCID search tools to look up required information on
                authors, affiliations and/or funding organizations.")
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
      tags$ol(
        class = 'custom-indent', start = 3,
        tags$li("Disclose any funding sources which provided financial support
                related to the generation, collection, curation or processing of
                the dataset.")
      ),
      hr(),
      tags$ol(
        class = 'custom-indent', start = 4,
        tags$li("If applicable, list any publications or datasets which are
                 related to this dataset.")
      ),
      hr(),

      card(
        class = 'card-note',
        card_header(
          id = ns('val_check_header'),
          class = 'bg-primary',
          span(icon("exclamation", style = "color: white"),'Validation check summary')),
        uiOutput(ns('validation_check'))
      ),

      hr(),
      layout_columns(
        actionButton(ns('btn_prev'), 'Previous', icon = icon('angle-double-left')),
        actionButton(ns('btn_next'), 'Next', icon = icon('angle-double-right'))
      ),
      verbatimTextOutput(ns("testing"))
    ),


    # main content ----
    accordion(
      open = c('General Info','Authors & Funding'),

      ## Dataset info
      accordion_panel(
        "General Info",
        h5('1. Dataset information'),
        layout_column_wrap(
          width = 1/2,
          textInput(ns("ds_name"), "Dataset name", value = NA,
                    placeholder = "Specify a name for your dataset (max 64 char.)"),
          textAreaInput(ns("description"), "Description", rows = 4,
                        placeholder = "Provide a brief description of your dataset")
        ),
        layout_column_wrap(
          width = 1/2,
          radioButtons(ns("ds_access"), "Dateset access rights",
                       choices = c("public", "restricted"),
                       selected = "public"),

          div(
            # Conditional panel for public dataset: license
            conditionalPanel(
              condition = "input.ds_access == 'public'",
              selectizeInput(ns("ds_license"),
                          "Select a license for the public dataset",
                          choices = c("CC BY 4.0", "CC BY-SA 4.0", "CC BY-NC 4.0",
                                      "CC BY-NC-SA 4.0", "CC BY-ND 4.0", "CC BY-NC-ND 4.0",
                                      "CCO 1.0"),
                          selected = "CC BY 4.0",
                          options = list(create = TRUE)),
              span("CC licenses are preferred (cf. ",
                   tags$a("Creative Commons License Chooser", href="https://chooser-beta.creativecommons.org/", target='_blank'),
                   "for more information) but a different license option can be added if required."),
              ns=NS(id)),
            # Conditional panel for private dataset: Embargo date
            conditionalPanel(
              condition = "input.ds_access == 'restricted'",
              dateInput(ns("embargoed_until"),
                        "If applicable, select an embargo date",
                        value = Sys.Date() + 365),
              span("*Access rights will only be changed after explicit confirmation by the data owners."),
              ns=NS(id))
          )
        ),
        layout_column_wrap(
          width = 1/2,
          textAreaInput(
            ns('acknowledgements'), "Acknowledgements", rows = 3,
            placeholder = "If applicable, acknowledge the contributions of specific colleagues, institutions, or agencies that aided these research efforts. Note that all authors and funding sources must be listed separately below.")
        )
      ),

      ## Authors and funding
      accordion_panel(
        'Authors & Funding',

        ### search tools
        accordion(
          id = ns("search_tools"),
          class = "accordion-tert",
          open = FALSE, # NOTE: does not work, fixed with panel_close event in server

          #### ROR search tool
          accordion_panel(
            'ROR search tool',
            layout_columns(
              card(
                class = "card-tert",
                selectInput(ns("ror_search_country"), "Select country",
                            choices = c(Choose='', countries_list),
                            selectize = TRUE),
                textInput(ns("ror_search_string"), "Enter search string"),
                actionButton(ns("btn_ror_search"), "Search for ROR", class = "btn btn-info")
                #shiny::selectizeInput("result_choice", "Select Result:", choices = NULL)
              ),
              card(
                class = "card-tert",
                h6('ROR search results'),
                htmlOutput(ns("ror_instr")),
                DT::DTOutput(ns("ror_results")),
                max_height = '350px'
              ),
              col_widths = c(3,9)
            )
          ),

          #### ORCID search tool
          accordion_panel(
            'ORCID search tool',
            layout_columns(
              card(
                class = "card-tert",
                # TODO: improve ORCID tool functionality
                textInput(ns("orcid_search_string"), "Enter search string", placeholder = "Search by name or ORCID..."),
                actionButton(ns("btn_orcid_search"), "Run search", class = "btn btn-info"),
                hr(),
                span(tags$i("If you have already entered author names in the table below, click this button to search for their ORCIDs.")),
                actionButton(ns("btn_orcid_tbl"), "Find ORCIDs for author table", class = "btn btn-info")
              ),
              card(
                class = "card-tert",
                h6('ORCID search results'),
                htmlOutput(ns("orcid_instr")),
                DT::DTOutput(ns("orcid_results")),
                max_height = '350px'
              ),
              col_widths = c(3,9)
            )
          )
        ),


        ### Authors input table
        hr(),
        h5('2. Author information'),

        div(style="display: flex; justify-content: space-between; align-items: center;",
            div(style="display: flex; gap: 10px;",
                actionButton(ns("btn_add_author"), "Add author", style = "width: 130px",
                             class = "btn btn-secondary", icon = icon('plus', lib = "glyphicon")),
                actionButton(ns("btn_del_author"), "Delete author", style = "width: 130px",
                             class = "btn btn-secondary", icon = icon('trash', lib = "glyphicon"))),

            div(style = "margin-left: auto; margin-bottom: 0;",
                fileInput(ns('file_authors'), "Load author data from file", accept = ".csv"))
        ),

        br(),

        rhandsontable::rHandsontableOutput(ns("author_table")),
        br(),

        #actionButton(ns('btn_save_aut'), "Save author data", icon = icon('save')),


        ### Funding info table
        hr(),
        h5('3. Funding information'),

        # another way to organize the layout
        div(style="display: flex; justify-content: space-between; align-items: center;",
          div(style="display: flex; gap: 10px;",
             actionButton(ns("btn_add_fund"), "Add source", style = "width: 130px",
                          class = "btn btn-secondary", icon = icon('plus', lib = "glyphicon")),
             actionButton(ns("btn_del_fund"), "Delete source", style = "width: 130px",
                          class = "btn btn-secondary", icon = icon('trash', lib = "glyphicon"))),

            div(style = "margin-left: auto; margin-bottom: 0;",
                fileInput(ns('file_funding'), "Load funding data from file", accept = ".csv"))
        ),

        rhandsontable::rHandsontableOutput(ns("funding_table")),
        br()

        #actionButton(ns('btn_save_fund'), "Save funding data", icon = icon('save')),

      ),

      ## Related resources
      accordion_panel(
        'Related resources',
        h5('4. Related publications and datasets'),

        card(
          class = "card-tert",
          layout_columns(
            textInput(ns("doi"), "Either enter DOI", placeholder = "e.g., 10.3389/fpls.2016.00781"),
            textAreaInput(ns("citation"), "Or enter full citation", "", rows = 3,
                          placeholder = "e.g., von Arx, G. et al., Quantitative Wood Anatomy—Practical Guidelines. Front. Plant Sci. 7, 781 (2016)."),
            textInput(ns("xcellid"), "Or enter QWA dataset ID", placeholder = "123"),
            col_widths = c(4,4,4)
          ),
          span(tags$i('If a valid DOI is entered, a citation will
                      automatically be generated via the DOI API.
                      Otherwise, the provided citation text is used directly.')),
          actionButton(ns("btn_add_pub"), "Add item", style = "width: 130px",
                       class = "btn btn-secondary", icon = icon('plus', lib = "glyphicon"))
          #shiny::selectizeInput("result_choice", "Select Result:", choices = NULL)
        ),

        hr(),
        p(tags$i("Add related publications or datasets by providing either DOI, full citation or dataset ID.")),

        DT::DTOutput(ns("rel_resources"))

      )
    ), # end of accordion

    # export progress to file button
    div(
      style = "text-align: center; margin-top: 20px;", # Centering and adding margin
      downloadButton(ns('btn_save'), "Export progress to file",
                     style = "font-size: 1.1rem; padding: 8px 15px;") # Bigger button styling
    ),


  ) # end of layout_sidebar
}








# hr(),
#
# fluidRow(
#   actionButton("btn_add_rel", "Add", icon("plus")),
#   actionButton("btn_edit_rel", "Edit", icon("edit")),
#   actionButton("btn_del_rel", "Delete", icon("trash-alt"))
# ),
# DT::DTOutput(ns("relatedDT"))



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


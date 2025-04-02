# library(shiny)
# library(shinysurveys)
# library(shinyjs)
# library(bslib)
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(reactable)
# library(reactable.extras)
# library(shinyvalidate) # NOTE: need remote mnaegelin/shinyvalidate@remove_rules


# allow to create new options in dropdown select:
# selectizeInput(
#   'e3', '3. Item creation', choices = state.name,
#   options = list(create = TRUE)
# ),

# searchable choose inputs (e.g. for species)
# selectInput('in4', 'Options', c(Choose='', state.name), selectize=TRUE)
# and should also be able to customize labels to show species code
# see https://shiny.posit.co/r/articles/build/selectize/



theme <- NULL
theme <- bs_theme(version = 5, primary = prim_col, secondary = sec_col,
                  info = tert_col, font_scale = 0.8, preset = "zephyr") %>%
  bs_add_rules(HTML(paste0("
    .btn-secondary {
      color: white;
    }
    .sidebar {
      background-color: ", prim_col_grad[6], " !important;
    }
    .bslib-navs-card-title {
      background-color: ", prim_col, ";
      color: white;
      font-size: 12pt;
    }
    .nav-link {
      font-size: 10pt;
      color: ", prim_col_grad[3], ";
      --bs-nav-link-hover-color: white;
      --bs-nav-underline-link-active-color: white;
    }
    .jstree-proton .jstree-clicked {
      background: ", prim_col, " !important;
    }
    .accordion .accordion-header {
      --bs-accordion-active-bg: ", prim_col_grad[2], " !important;
    }
    .accordion-button {
      background-color: ", prim_col_grad[5], ";
    }
    .accordion-secondary .accordion-header {
      --bs-accordion-active-bg: ", tert_col_grad[2], " !important;
    }
    .dataTables_wrapper .dataTable td {
      padding: 2px 2px !important;
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      padding: 2px 2px !important;  /* Adjust padding to make buttons smaller */
    }
    ol.custom-indent {
      padding-left: 10px; /* Adjust the left padding to reduce indentation */
    }
    .handsontable th {
      background-color: ", sec_col_grad[5], " !important;
    }
  ")))

# ".jstree-default .jstree-clicked {
#   background: #C299B8 !important;
#   background: -webkit-linear-gradient(top, #E0CCDB 0%, #C299B8 100%) !important;
#                                         background: linear-gradient(to bottom, #E0CCDB 0%, #C299B8 100%) !important;
# }
# .jstree-default .jstree-wholerow-clicked {
#   background: #C299B8 !important;
#   background: -webkit-linear-gradient(top, #E0CCDB 0%, #C299B8 100%) !important;
#                                         background: linear-gradient(to bottom, #E0CCDB 0%, #C299B8 100%) !important;
# }"


# Define UI --------------------------------------------------------------------
ui <- page_fluid(

  # preliminaries
  shinyjs::useShinyjs(),  # Include shinyjs
  #reactable.extras::reactable_extras_dependency(),  # Include reactable.extras

  # theme
  theme = theme,

  # additional style vars
  # tags$head(
  #   tags$style(HTML(
  #     ""))),


  # MAIN PANEL -----------------------------------------------------------------
  navset_card_underline( # navset_card_pill, page_navbar?
    id = 'tabs',
    selected = tab_general, # TODO: for testing, set to tab_start
    # navbar_options = navbar_options(collapsible = FALSE),
    # fillable = FALSE,

    # TITLE --------------------------------------------------------------------
    title = "rxs2xcell: Contribute metadata", #HTML('<h4 style="color: #006268; font-weight: bold;">rxs2xcell: Contribute metadata</h4>'),

    # SIDEBAR ------------------------------------------------------------------
    # sidebar = sidebar(
    #   id = "sidebar",
    #   open = FALSE,
    #   "Sidebar"
    # ),

    # TAB: prefill metadata ----------------------------------------------------
    nav_panel(
      title = tab_start,

      layout_sidebar(

        # sidebar: Input data
        sidebar = sidebar(
          card(
            card_header(
              class = 'bg-secondary',
              'Input data'),
            span(
              icon("info", style = paste("color:", sec_col)),
              "If QWA metadata is available in the current R environment,
              it will be used directly. Alternatively, you can browse for a
              saved QWA metadata file."),
            span(
              span('Source of shown data:', style=paste("font-weight:bold; color:", sec_col)),
              htmlOutput("file_status")
            ),
            fileInput("file_upload", "Load metadata from file (csv)", accept = ".csv"),
            #textOutput("file_status") #span(textOutput("file_status"), style="font-weight:bold; color: #006268"),
          ),
          hr(),
          span('If the structure and metadata shown on the right are as you
               would expect them for your dataset, click the button below to
               proceed to the next tab.'),
          actionButton('btn_next_meta', 'Next', icon = icon('angle-double-right'))
        ),

        # main content
        h5('Overview of input metadata'),
        accordion(
          open = c("Data structure", 'DT Available metadata'),

          accordion_panel(
            title = "Data structure",
            p("Use this data.tree to explore the inferred structure of the
               provided dataset and filter the metadata table below."),
            #verbatimTextOutput("testing1"),
            #networkD3::diagonalNetworkOutput("radial_network"),
            #networkD3::radialNetworkOutput("radial_network"),
            #verbatimTextOutput('selectedNode')
            shinyTree::shinyTree("tree", theme="proton", checkbox = TRUE, tie_selection = TRUE,
                                 whole_node = TRUE, three_state = TRUE,
                                 wholerow = FALSE)
          ),

          accordion_panel(
            title = "DT Available metadata",
            fillable = FALSE,
            DT::DTOutput('DTmeta')
          )

          # accordion_panel(
          #   title = "Available metadata",
          #   fillable = FALSE,
          #   reactable::reactableOutput("partial_meta_out")
          #   # card(
          #   #   card_header('Overview of input metadata'),
          # )

        ) # end of accordion
      ) # end of layout_sidebar
    ), # end of nav_panel

    # TAB: general -------------------------------------------------------------
    nav_panel(
      title = tab_general,

      layout_sidebar(

        # sidebar
        sidebar = sidebar(
          verbatimTextOutput("testing2"),
          card(
            card_header(
              class = 'bg-secondary',
              span(icon("info", style = "color: white"),'Instructions')),
            tags$ol(
              class = 'custom-indent',
              tags$li("Please provide a name and description of your dataset."),
              tags$li("Please list all authors (data owners) of the dataset. Use the
              ROR search tool to find the Research Organization Registry ID of
              an affiliation.")
            ),
            span("Note that the order of authors provided here will be used as
              the order of authorship.", class = "text-secondary"),
          ),
          layout_columns(
            actionButton('btn_prev_general', 'Previous', icon = icon('angle-double-left')),
            actionButton('btn_next_general', 'Next', icon = icon('angle-double-right'))
          )
        ),

        # main content
        accordion(
          open = c('Dataset: Authors'),

          ## Dataset info
          accordion_panel(
            "Dataset: General Info",
            layout_columns(
              textInput("ds_name", "Dataset name", value = NA,
                        placeholder = "Specify a name for your dataset (max 64 char.)"),
              textAreaInput("ds_desc", "Description", rows = 4,
                            placeholder = "Describe your dataset")
            )
          ),

          ## Authors
          accordion_panel(
            'Dataset: Authors',

            ## ROR search tool
            accordion(
              class = "accordion-secondary",
              open = FALSE, # does not work, to fix: https://stackoverflow.com/questions/77196229/nested-accordion-does-not-seem-adhere-to-multiple-false-parameter
              multiple = FALSE,
              accordion_panel(
                'ROR search tool',
                layout_columns(
                  card(
                    selectInput("country_code", "Select country:",
                                choices = c(Choose='', get_country_codes()),
                                selectize = TRUE),
                    textInput("search_string", "Enter search string:"),
                    actionButton("search_ror", "Search for ROR", class = "btn btn-primary")
                    #shiny::selectizeInput("result_choice", "Select Result:", choices = NULL)
                  ),
                  card(
                    h6('ROR search results:'),
                    DT::DTOutput("ror_results"),
                    max_height = '350px'
                  ),
                  col_widths = c(3,9)
                )
              )
            ),

            ## ROR search tool
            hr(),
            h5('Author information:', style = 'color: #69004F'),

            div(style='float: right',
                fileInput('file_authors', "Load author data from file", accept = ".csv")),

            div(style='display: flex; justify-content: flex-start; margin-top: 30pt',
                actionButton("add_author_btn", "Add author", style = "width: 120px",
                             class = "btn btn-secondary"),
                actionButton("del_author_btn", "Delete author", style = "width: 120px",
                             class = "btn btn-danger")),

            br(),
            rhandsontable::rHandsontableOutput("author_table"),
            br(),

            actionButton('save_authors_btn', "Save author data", icon = icon('save'))

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

          )



        ) # end of main content
      ) # end of layout_sidebar
    ) # end of tab

    # TAB: new

      # nav_panel("Sites",
      #           uiOutput("site_inputs"),
      #           verbatimTextOutput("site_data"),
      #           next_button('next_btn_sites')
      #           ),
      #
      # nav_panel("Trees",
      #           DT::DTOutput("tree_table"),
      #           next_button('next_btn_trees')
      # ),


      # TAB: study details -------------------------------------------------------
      # nav_panel("Study Details",
      #                selectInput("study_type", "Study Type", choices = c("Experimental", "Observational", "Survey")),
      #                numericInput("sample_size", "Sample Size", value = 10, min = 1),
      #                dateInput("study_date", "Study Date", value = Sys.Date())
      #       ),
      # nav_panel("Locations",
      #                uiOutput("dynamic_location_inputs")  # Dynamically generated inputs
      #       ),
      # nav_panel("Preview & Save",
      #                tableOutput("preview_table"),
      #                actionButton("save_btn", "Save Metadata", icon = icon("save")),
      #                textOutput("save_status")
      #
      #                           )
  )
) # end of page_fluid


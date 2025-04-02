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





# Define UI --------------------------------------------------------------------
ui <- page_fluid(
  # preliminaries
  shinyjs::useShinyjs(),  # Include shinyjs
  #reactable.extras::reactable_extras_dependency(),  # Include reactable.extras

  # theme
  theme = bs_theme(primary = "#006268", secondary = "#69004F", # navbar_bg = 'darkslategrey',
                   font_scale = 0.8, preset = "cosmo"),

  tags$head(
    tags$style(HTML(
      ".jstree-proton .jstree-clicked {
        background: #006268 !important;
      }
      .accordion .accordion-secondary .accordion-header {
        --bs-accordion-active-bg: #99a5c2 !important;
      }"
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
  ))),


  # MAIN PANEL -----------------------------------------------------------------
  navset_card_pill( # navset_card_pill, page_navbar?
    id = 'tabs',
    selected = "General", # TODO: for testing, set to "Start"
    # navbar_options = navbar_options(collapsible = FALSE),
    # fillable = FALSE,

    # TITLE --------------------------------------------------------------------
    title = HTML('<h4 style="color: #006268; font-weight: bold;">rxs2xcell: Contribute metadata</h4>'),

    # SIDEBAR ------------------------------------------------------------------
    # sidebar = sidebar(
    #   id = "sidebar",
    #   open = FALSE,
    #   "Sidebar"
    # ),

    # TAB: prefill metadata ----------------------------------------------------
    nav_panel(
      title = 'Start',

      layout_sidebar(

        # sidebar: Input data
        sidebar = sidebar(
          card(
            card_header('Input data'),
            span("If QWA metadata is available in the current R environment,
                 it will be used directly. Alternatively, you can browse for a
                 saved QWA metadata file."),
            fileInput("file_upload", "Load metadata from file (csv)", accept = ".csv"),
            span(
              span('Source of shown data:', style="font-weight:bold; color: #006268"),
              htmlOutput("file_status")
            )
            #textOutput("file_status") #span(textOutput("file_status"), style="font-weight:bold; color: #006268"),
          ),
          actionButton('btn_next_meta', 'Next', icon = icon('angle-double-right'))
        ),

        # main content
        h5('Overview of input metadata',style="font-weight:bold; color: #006268"),
        accordion(
          open = c("Data structure", 'DT Available metadata'),

          accordion_panel(
            title = "Data structure",
            p( "Use this data.tree to explore the inferred structure of the
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

        )

      )
    ),

    # TAB: general -------------------------------------------------------------
    nav_panel(
      title = "General",

      layout_sidebar(

        # sidebar
        sidebar = sidebar(
          verbatimTextOutput("testing2"),
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
            textInput("ds_name", "Dataset name", value = NA,
                      placeholder = "Specify a name for your dataset (max 64 char.)"),
            textAreaInput("ds_desc", "Description", rows = 4,
                          placeholder = "Describe your dataset"),
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
                    h4('ROR search results:'),
                    DT::DTOutput("ror_results"),
                    max_height = '400px'
                  ),
                  col_widths = c(3,9)
                )
              )
            ),

            hr(),
            p(span("Please list all authors (data owners) of the dataset. Use the
            ROR search tool to find the Research Organization Registry ID of
            an affiliation."),
            strong("Note that the order of authors provided here will be used as
                 the order of authorship.")),

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

            rhandsontable::rHandsontableOutput("author_table"),

            card(
              class="border border-0",
              card_body(
                fillable = FALSE,
                actionButton("add_author_btn", "Add author", style = "width: 100px",
                             class = "btn btn-primary"),
                actionButton("del_author_btn", "Delete author", style = "width: 100px",
                             class = "btn btn-danger"),
                actionButton('save_authors_btn', "Save data", icon = icon('save'), style = "width: 100px"),
                fileInput('file_authors', "Import data", accept = ".csv")
              )
            )
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


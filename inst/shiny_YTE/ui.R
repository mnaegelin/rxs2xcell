
# library(plotly)
# library(bslib)
# library(shiny)
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(reactable)
# library(reactable.extras)
# library(ggiraph)
# library(DT)
# TODO: as separate package? and imports?


#source('../../R/clean_raw_data.R')

#https://forum.posit.co/t/shiny-app-matrix-checkbox/108251/3
#https://rstudio.github.io/DT/extensions.html

# get necessary data
# TODO: should be read from file? or ok bc already in environment?
# path_data <- './out'
# df_structure <- read.csv(file.path(path_data, 'QWA_Arzac2024_meta.csv'))
# df_rings <-  read.csv(file.path(path_data, 'QWA_Arzac2024_rings.csv'))


# add button to open image
# save flags to file
# save cov plots to file

# UI
ui <- page_sidebar(
  shinyjs::useShinyjs(),
  # theme = "bootstrap_wsl.css",
  theme = bs_theme(primary = "#006268", secondary = "#69004F",
                   font_scale = 0.8, preset = "cosmo") %>%
    bs_add_rules(
      sass::as_sass(
        # " table.dataTable tbody tr.active td { background: pink !important; }"
        " table.dataTable thead tr { background: #CCE0E0 !important; }" # color for table header
        )),
  # tags$head(
  #   tags$style(HTML(
  #     ".done {list-style-type:square}
  #      .notdone {list-style-type:circle}"
  #   ))),
  # tags$style(HTML(
  #   ".dt-row-group {
  #     background-color: green !important;
  #   }"
  # )),

  # tags$head(
  #   # Note the wrapping of the string in HTML(),
  #   # use the following to avoid dropdown overlapping stuff
  #   # .selectize-control .selectize-dropdown {
  #   #   position: static !important;
  #   # }
  #   tags$style(HTML("
  #     .sidebar {
  #       height: 90vh;
  #       display: flex;
  #       flex-direction: column;
  #       justify-content: space-between;
  #       width: 250px; /* Adjust based on the width of the sidebar */
  #
  #       position: fixed;
  #     }
  #     .sidebar-content {
  #       flex-grow: 1;
  #     }
  #     .sidebar-footer {
  #       margin-bottom: 20px;
  #     }
  #     .main {
  #       margin-left: 270px; /* Adjust based on the width of the sidebar */
  #     }
  #   "))
  # ),

  # TITLE ----------------------------------------------------------------------
  title = "rxs2xcell: Inspect Yearly Coverage",

  # SIDEBAR  -------------------------------------------------------------------
  sidebar = sidebar(

    # INPUT DATA ---------------------------------------------------------------
    card(
      card_header("Input data"),
      span("If QWA data is available in the current R environment, it will be used directly.
        Alternatively, you can browse for a saved QWA data file."),
      fileInput("file_upload", "Load QWA data from file (csv)", accept = ".csv"),
      strong('Source of shown data:'),
      textOutput("data_source")
    ),

    # ANALYSIS CONTROLS --------------------------------------------------------
    card(
      card_header("Analysis controls"),

      # INPUT WOODPIECE --------------------------------------------------------
      selectInput(
        "woodpiece",
        label = 'Select woodpiece (core) to inspect:',
        choices = "No data yet"
      ),

      HTML("<hr>"),

      # RESET BUTTON -----------------------------------------------------------
      span("Reset the flags for the current woodpiece back to input data:"),
      actionButton("btn_reset", "Reset current flags",
                   class = "btn btn-secondary"),

      HTML("<hr>"),

      # USER FLAG VALIDATION ---------------------------------------------------
      strong("Validity check for the manual changes:"),
      span("The following conditions need to be met for user input to be valid:"),
      uiOutput("flags_check")

    ),


    # # INPUT WOODPIECE --------------------------------------------------------
    # p("Select woodpiece (core) to inspect"),
    # selectInput(
    #   "woodpiece",
    #   label = NULL,
    #   choices = unique(df_structure$woodpiece_code),
    #   selected = unique(df_structure$woodpiece_code)[1]
    # ),
    #
    # HTML("<hr>"),
    #
    # # RESET BUTTON -----------------------------------------------------------
    # p("Reset the flags for the current woodpiece"),
    # actionButton("btn_reset", "Reset current flags",
    #              class = "btn btn-primary"),
    #
    #
    # SUBMIT BUTTON ------------------------------------------------------------
    # p("Save the changes made for the current woodpiece and go to next"),
    # actionButton("btn_submit", "Next",
    #              class = "btn btn-primary"),
    # textOutput("progress_txt"),
    #
    # HTML("<hr>"),


    # SAVE BUTTON --------------------------------------------------------------
    card(
      card_header("Save data"),
      p("Once all the flags are set for all woodpieces, export the data and
        close the app."),
      downloadButton("btn_save", "Export and close app",
                     class = "btn btn-primary")
    ),

    # TEXT OUTPUT FOR TESTING
    verbatimTextOutput('testing'),

  ), # end of sidebar


  # MAIN PANEL -----------------------------------------------------------------
  accordion(
    open = c("Coverage plot", 'Data'),

    # COVERAGE PLOT ------------------------------------------------------------
    # accordion_panel(
    #   "Coverage plot",
    #   girafeOutput("covPlot")
    # ),

    # COVERAGE PLOT ------------------------------------------------------------
    accordion_panel(
      "Coverage plot",
      plotly::plotlyOutput("covPlot")
    ),

    # DATA TABLE ---------------------------------------------------------------
    accordion_panel(
      "Data",
      DT::DTOutput('tbl')
    ),

    # DF TABLE -----------------------------------------------------------------
    # accordion_panel(
    #   "Data2",
    #   reactable.extras::reactable_extras_dependency(),
    #   reactable::reactableOutput("reactTable")
    # )

  ) # end of accordion

) # end of page_sidebar

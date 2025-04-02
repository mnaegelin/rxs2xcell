# library(shiny)
# library(bslib)
# library(sass)
# library(shinyjs)
# library(DT)
# library(ggplot2)
# library(plotly)
# library(checkmate)
# library(shinyalert)

# TODO:
# add button to open image
# save flags to file
# save cov plots to file
# validity check: at least one sel flag per overlap

ui <- page_sidebar(
  # preliminaries
  shinyjs::useShinyjs(),

  # theme
  theme = bs_theme(primary = "#006268", secondary = "#69004F",
                   font_scale = 0.8, preset = "cosmo") %>%
    bs_add_rules(
      sass::as_sass(
        " table.dataTable thead tr { background: #CCE0E0 !important; }" # color for table header
        )),

  # to add css directly:
  # tags$head(
  #   tags$style(HTML( ... )))
  # list icon styles
  #   ".done {list-style-type:square}
  #      .notdone {list-style-type:circle}"
  # dt styling
  #   ".dt-row-group {
  #     background-color: green !important;}"
  # to avoid dropdown overlapping stuff
  # ".selectize-control .selectize-dropdown {
  #   position: static !important;}"


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
    accordion_panel(
      "Coverage plot",
      plotly::plotlyOutput("covPlot")
    ),

    # accordion_panel(
    #   "Coverage plot",
    # first try with girafe plot for clickable images, but does not resize properly
    #   girafeOutput("covPlot")
    # ),

    # DATA TABLE ---------------------------------------------------------------
    accordion_panel(
      "Data",
      DT::DTOutput('tbl')
    ),

    # accordion_panel(
    #   "Data",
    # first try with reactable for collapsible groups, but with DT can click cells to edit
    #   reactable.extras::reactable_extras_dependency(),
    #   reactable::reactableOutput("reactTable")
    # )

  ) # end of accordion

) # end of page_sidebar

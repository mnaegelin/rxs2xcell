library(plotly)
library(bslib)
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(reactable)
library(reactable.extras)
library(ggiraph)
library(DT)
# TODO: as separate package? and imports?


#source('../../R/clean_raw_data.R')

#https://forum.posit.co/t/shiny-app-matrix-checkbox/108251/3
#https://rstudio.github.io/DT/extensions.html

# get necessary data
# TODO: should be read from file? or ok bc already in environment?
# path_data <- './out'
# df_structure <- read.csv(file.path(path_data, 'QWA_Arzac2024_meta.csv'))
# df_rings <-  read.csv(file.path(path_data, 'QWA_Arzac2024_rings.csv'))




# add button to update df_rings and cov_plot (-> make reactive)
# add button to open image
# save flags to file
# save cov plots to file

# UI
ui <- page_sidebar(
  # theme = "bootstrap_wsl.css",
  theme = bs_theme(primary = "#006268", secondary = "#00919A",
                   font_scale = 0.8, preset = "cosmo"),

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

    # INPUT WOODPIECE ----------------------------------------------------------
    p("Select woodpiece (core) to inspect"),
    selectInput(
      "woodpiece",
      label = NULL,
      choices = unique(df_structure$woodpiece_code),
      selected = unique(df_structure$woodpiece_code)[1]
    ),

    HTML("<hr>"),

    # RESET BUTTON -------------------------------------------------------------
    p("Reset the flags from the current woodpiece"),
    actionButton("btn_reset", "Reset flags",
                 class = "btn btn-primary"),

    HTML("<hr>"),

    # SAVE BUTTON --------------------------------------------------------------
    p("Once all the flags are set, save to a file"),
    actionButton("btn_save", "Save user flags",
                 class = "btn btn-primary"),



    verbatimTextOutput('testing')




  ), # end of sidebar

  # MAIN PANEL -----------------------------------------------------------------
  accordion(
    open = c("Coverage plot", 'Data'),

    # COVERAGE PLOT ------------------------------------------------------------
    accordion_panel(
      "Coverage plot",
      girafeOutput("covPlot")
    ),

    # COVERAGE PLOT ------------------------------------------------------------
    accordion_panel(
      "Coverage plot2",
      plotlyOutput("covPlot2")
    ),

    # DF TABLE -----------------------------------------------------------------
    # accordion_panel(
    #   "Data",
    #   reactable.extras::reactable_extras_dependency(),
    #   reactableOutput("reactTable")
    # ),

    accordion_panel(
      "Data",
      DTOutput('tbl')
    ),

  ) # end of accordion

) # end of page_sidebar

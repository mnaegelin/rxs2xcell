site_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(

    # sidebar
    sidebar = sidebar(
      title = "Instructions",
      card(
        class = 'card-note',
        card_header(
          class = 'bg-primary',
          span(icon("exclamation", style = "color: white"),'Note')),
        "The tables here are partially filled with information from the input data you provided in the Start tab."
      ),
      hr(),
      tags$ol(
        class = 'custom-indent',
        tags$li("Please provide information related to how the QWA data in this dataset where obtained and generated, at the site-, tree-, woodpiece- and slide-level."),),
      hr(),
      tags$ol(
        class = 'custom-indent', start = 2,
        tags$li("Please list all ...")
      ),
      hr(),
      card(
        class = 'card-note',
        card_header(
          class = 'bg-primary',
          span(icon("exclamation", style = "color: white"),'Validation check summary')),
        uiOutput(ns('validation_check')),
      ),
      hr(),
      layout_columns(
        actionButton(ns('btn_prev'), 'Previous', icon = icon('angle-double-left')),
        actionButton(ns('btn_next'), 'Next', icon = icon('angle-double-right'))
      ),
      verbatimTextOutput(ns("testing"))
    ),

    # main content
    accordion(
      open = c('Sites'),

      accordion_panel(
        'Sites',

        accordion(
          id = ns("map_acc"),
          class = "accordion-tert",
          open = FALSE, # NOTE: does not work, fixed with panel_close event in server
          accordion_panel(
            "Map",
            leaflet::leafletOutput(ns("site_map"))
          )
        ),

        hr(),


        h5('Site information'),

        div(style='float: right',
            fileInput(ns('file_sites'), "Load site data from file", accept = ".csv")),

        div(style="min-height:200px;height:auto;",

        rhandsontable::rHandsontableOutput(ns("site_table"))),

        verbatimTextOutput(ns('no_data_site'))

      ),

      accordion_panel(
        'Trees',

        h5('Tree information'),

        div(style='float: right',
            fileInput(ns('file_trees'), "Load tree data from file", accept = ".csv")),

        br(),
        div(style="min-height:200px;height:auto;",
        rhandsontable::rHandsontableOutput(ns("tree_table"))),

        verbatimTextOutput(ns('no_data_tree'))

      ),

      accordion_panel(
        'Woodpieces',

        h5('Woodpiece information'),

        div(style='float: right',
            fileInput(ns('file_wp'), "Load woodpiece data from file", accept = ".csv")),

        br(),

        rhandsontable::rHandsontableOutput(ns("wp_table")),

        verbatimTextOutput(ns('no_data_wp'))


      ),

      accordion_panel(
        'Slides',

        h5('Slide information'),

        div(style='float: right',
            fileInput(ns('file_slide'), "Load slide data from file", accept = ".csv")),

        br(),

        rhandsontable::rHandsontableOutput(ns("slide_table")),

        verbatimTextOutput(ns('no_data_slide'))

      )

    ) # end of main content
  ) # end of layout_sidebar
}


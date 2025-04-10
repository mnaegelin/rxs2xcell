site_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(

    # sidebar
    # sidebar
    sidebar = sidebar(
      title = "Instructions",
      card(
        class = 'card-note',
        card_header(
          class = 'bg-primary',
          span(icon("exclamation", style = "color: white"),'Note')),
        "The site table is partially filled with information from the df_meta as provided in the Start tab."
      ),
      hr(),
      tags$ol(
        class = 'custom-indent',
        tags$li("Please provide ...")),
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
        "Map",
        leaflet::leafletOutput(ns("site_map"))
      ),

      accordion_panel(
        'Sites',

        h5('Site information:', style = paste0('color: ',  sec_col)),

        # card(
        #   height = 300,
        #   card_body(
        #     fillable = FALSE,
        div(style='float: right',
            fileInput(ns('file_sites'), "Load site data from file", accept = ".csv")),

        rhandsontable::rHandsontableOutput(ns("site_table")),
          # )

        br(),

        actionButton(ns('save_btn'), "Save site data", icon = icon('save')),

      )

    ) # end of main content
  ) # end of layout_sidebar
}


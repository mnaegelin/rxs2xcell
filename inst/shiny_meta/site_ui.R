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
        'Sites',

        h5('Site information:', style = paste0('color: ',  sec_col)),


        br(),
        # card(
        #   height = 300,
        #   card_body(
        #     fillable = FALSE,
        div(style = "margin-right: 5px !important; height = 100px;",
            rhandsontable::rHandsontableOutput(ns("site_table"))
          # )
        ),
        br(),

        actionButton(ns('save_btn'), "Save site data", icon = icon('save')),

      )

    ) # end of main content
  ) # end of layout_sidebar
}


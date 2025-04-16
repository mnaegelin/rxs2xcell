summary_ui <- function(id) {
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
        "The overview here combines all information provided in tabs 1-4."
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

      actionButton(ns('btn_prev'), 'Previous', icon = icon('angle-double-left')),

      hr(),

      verbatimTextOutput(ns("testing"))
    ),

    # main content
    accordion(
      open = c('Overview'),

      accordion_panel(
        'Overview',

        h5('Data overview:', style = paste0('color: ',  sec_col)),

        card(
          class = 'card-note',
          card_header(
            class = 'bg-primary',
            span(icon("exclamation", style = "color: white"),'Validation check summary')),
          "to be implemented: summary of provided data and validation checks",
          uiOutput(ns('validation_check')),
        ),
        hr(),

        actionButton(ns('save_btn'), "Export ALL data", icon = icon('save')),

      )

    ) # end of main content
  ) # end of layout_sidebar
}

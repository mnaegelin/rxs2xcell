tree_ui <- function(id) {
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
        "The tree table is partially filled with information from the df_meta as provided in the Start tab."
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
      open = c('Trees'),

      accordion_panel(
        'Trees',

        h5('Tree information:', style = paste0('color: ',  sec_col)),

        div(style='float: right',
            fileInput(ns('file_trees'), "Load tree data from file", accept = ".csv")),

        br(),
        # card(
        #   height = 300,
        #   card_body(
        #     fillable = FALSE,

        rhandsontable::rHandsontableOutput(ns("tree_table")),
        # )

        br(),

        actionButton(ns('save_btn_tree'), "Save tree data", icon = icon('save')),

      ),

      accordion_panel(
        'Woodpieces',

        h5('Woodpiece information:', style = paste0('color: ',  sec_col)),

        div(style='float: right',
            fileInput(ns('file_wp'), "Load woodpiece data from file", accept = ".csv")),

        br(),
        # card(
        #   height = 300,
        #   card_body(
        #     fillable = FALSE,

        rhandsontable::rHandsontableOutput(ns("wp_table")),
        # )

        br(),

        actionButton(ns('save_btn_wp'), "Save woodpiece data", icon = icon('save')),

      ),

      accordion_panel(
        'Slides',

        h5('Slide information:', style = paste0('color: ',  sec_col)),

        div(style='float: right',
            fileInput(ns('file_slide'), "Load slide data from file", accept = ".csv")),

        br(),
        # card(
        #   height = 300,
        #   card_body(
        #     fillable = FALSE,

        rhandsontable::rHandsontableOutput(ns("slide_table")),
        # )

        br(),

        actionButton(ns('save_btn_slide'), "Save slide data", icon = icon('save')),

      )

    ) # end of main content
  ) # end of layout_sidebar
}

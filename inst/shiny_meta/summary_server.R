summary_server <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {
    # Previous button
    observeEvent(input$btn_prev, {
      nav_select(id = 'tabs', selected = tab_tree, session = main_session)
    })
  })
}



# Selecting a tab
if (FALSE) {
  shinyApp(
    page_fluid(
      radioButtons("item", "Choose", c("A", "B")),
      navset_hidden(
        id = "container",
        nav_panel_hidden("A", "a"),
        nav_panel_hidden("B", "b")
      )
    ),
    function(input, output) {
      observe(nav_select("container", input$item))
    }
  )
}

# Inserting and removing
if (FALSE) {
  ui <- page_fluid(
    actionButton("add", "Add 'Dynamic' tab"),
    actionButton("remove", "Remove 'Foo' tab"),
    navset_tab(
      id = "tabs",
      nav_panel("Hello", "hello"),
      nav_panel("Foo", "foo"),
      nav_panel("Bar", "bar tab")
    )
  )
  server <- function(input, output) {
    observeEvent(input$add, {
      nav_insert(
        "tabs", target = "Bar", select = TRUE,
        nav_panel("Dynamic", "Dynamically added content")
      )
    })
    observeEvent(input$remove, {
      nav_remove("tabs", target = "Foo")
    })
  }
  shinyApp(ui, server)
}

# my example
if (FALSE) {
  # Sample dataframe
  df <- data.frame(woodpiece_code = c("A123", "B456", "C789"))

  # Extract unique woodpiece codes
  unique_codes <- unique(df$woodpiece_code)

  ui <- page_fluid(
    do.call(navset_pill_list, c(
      list(id = "tabs"),
      lapply(unique_codes, function(code) {
        nav_panel(paste("Woodpiece", code), paste("Content for woodpiece", code))
      })
    ))
  )

  server <- function(input, output) {
  }

  shinyApp(ui, server)
}

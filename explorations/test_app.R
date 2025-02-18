library(shiny)
library(shinyjs)


# Define UI
inputUI <- function(id){
  ns <- NS(id) # Initialize shinyjs


  # Disable the Remove UI button initially
  tagList(
    # fluidRow(div(id = ns("box1"), box(
    #   fileInput(ns("plotFile1"), "Upload 1")
    # ))),
    # fluidRow(div(id = ns("box2"), box(
    #   fileInput(ns("plotFile2"), "Upload 2")
    # ))),
    actionButton(ns("add"), "Add"),
    actionButton(ns("remove"), "Remove last input")
  )
}

# Server logic
inputServer <- function(id) {
  library(shinyjs)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    added <- reactive({
      print(added_ui_count())
      added_ui_count()
    })

    # Reactive value to track the added UI elements
    added_ui_count <- reactiveVal(2)  # Initialize with 2

    # Create a condition for enabling/disabling the "Add UI" button
    enable_add_button <- reactive({
      added_ui_count() < 4  # Adjust the number as needed
    })

    enable_remove_button <- reactive({
      added_ui_count() > 2  # Adjust the number as needed
    })

    observeEvent(enable_add_button(), {
      # Enable or disable the "Add UI" button based on the condition
      if (enable_add_button()) {
        enable("add")  # Enable the button
      } else {
        disable("add")  # Disable the button
      }
    })

    observeEvent(enable_remove_button(), {
      # Enable or disable the "Remove UI" button based on the condition
      if (enable_remove_button()) {
        enable("remove")  # Enable the button
      } else {
        disable("remove")  # Disable the button
      }
    })

    observeEvent(input$add, {
      if (enable_add_button()) {
        # Increment the count of added UI elements
        added_ui_count(added_ui_count() + 1)

        # Generate a unique ID for the new UI element
        ui_id <- paste0("box", added_ui_count())

        insertUI(
          selector = paste0("#",ns("add")),
          where = "afterEnd",
          ui = tags$div(
            id = ui_id,
            box(
              textInput(paste0("txt", added_ui_count()), "Insert some text")
            )
          )
        )
      }
    })

    observeEvent(input$remove, {
      if (enable_remove_button()) {
        current_count <- added_ui_count()

        # Remove the UI element with the corresponding ID
        removeUI(
          selector = paste0("#box", current_count)
        )

        # Decrement the count of added UI elements
        added_ui_count(current_count - 1)
      }
    })
  })
}

ui <- fluidPage(
  useShinyjs(), # Initialize ShinyJS
  inputUI("module1"),
  # Other UI components
)

server <- function(input, output, session) {
  inputServer("module1")
  # Other server logic
}

# Complete app with UI and server components
shinyApp(ui, server)

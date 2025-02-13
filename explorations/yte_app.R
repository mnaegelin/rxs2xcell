library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)



# Sample data
set.seed(123)
df <- data.frame(
  slide_number = rep(1:5, each = 5),
  image_number = rep(1:5, times = 5),
  year = rep(2000:2004, times = 5),
  incomplete_ring = sample(c(TRUE, FALSE), 25, replace = TRUE),
  missing_ring = sample(c(TRUE, FALSE), 25, replace = TRUE),
  duplicate_ring = sample(c(TRUE, FALSE), 25, replace = TRUE)
)

ui <- fluidPage(
  titlePanel("Tree Ring Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4("Modify Flags"),
      selectInput("flag_type", "Select Issue Type",
                  choices = c("Incomplete Ring" = "incomplete_ring",
                              "Missing Ring" = "missing_ring",
                              "Duplicate Ring" = "duplicate_ring",
                              "Custom Flag" = "custom_flag")),
      textInput("custom_flag_name", "Custom Flag Name (if selected)", ""),
      actionButton("add_flag", "Add Flag"),
      DTOutput("data_table")
    ),
    mainPanel(
      plotlyOutput("plot"),
      textOutput("click_info")
    )
  )
)

server <- function(input, output, session) {
  # Reactive dataframe
  df_reactive <- reactiveVal(df)

  # Plot function
  output$plot <- renderPlotly({
    df_data <- df_reactive()

    p <- ggplot(df_data, aes(x = year, y = as.factor(image_number))) +
      geom_segment(aes(xend = year + 1, yend = as.factor(image_number)), size = 5) +
      geom_point(data = df_data %>% filter(incomplete_ring), aes(x = year, y = as.factor(image_number)), color = "red", shape = 4, size = 5) +
      geom_point(data = df_data %>% filter(missing_ring), aes(x = year, y = as.factor(image_number)), color = "blue", shape = 4, size = 5) +
      geom_point(data = df_data %>% filter(duplicate_ring), aes(x = year, y = as.factor(image_number)), color = "green", shape = 4, size = 5) +
      labs(title = "Annual Rings with Issues", x = "Year", y = "Image Number") +
      theme_minimal()

    ggplotly(p, source = "click") %>%
      layout(clickmode = "event+select")
  })

  # Click event for modifying flags
  observeEvent(event_data("plotly_click", source = "click"), {
    click_data <- event_data("plotly_click", source = "click")

    if (!is.null(click_data)) {
      selected_year <- round(click_data$x)
      selected_image <- click_data$y

      df_data <- df_reactive()

      # Toggle flag based on user selection
      flag_col <- input$flag_type
      if (flag_col == "custom_flag") {
        flag_col <- input$custom_flag_name
        if (flag_col == "") return()
        if (!(flag_col %in% colnames(df_data))) {
          df_data[[flag_col]] <- FALSE
        }
      }

      # Toggle flag
      df_data[df_data$year == selected_year & df_data$image_number == as.numeric(selected_image), flag_col] <-
        !df_data[df_data$year == selected_year & df_data$image_number == as.numeric(selected_image), flag_col]

      df_reactive(df_data)
    }
  })

  # Display updated data table
  output$data_table <- renderDT({
    datatable(df_reactive(), editable = TRUE)
  })
}

shinyApp(ui, server)

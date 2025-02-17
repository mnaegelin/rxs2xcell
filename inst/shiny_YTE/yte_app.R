library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(reactable)
library(reactable.extras)
library(ggiraph)


# source('../R/clean_raw_data.R')

#https://forum.posit.co/t/shiny-app-matrix-checkbox/108251/3

# get necessary data
# TODO: should be read from file? or ok bc already in environment?
path_data <- '../../out'
df_structure <- read.csv(file.path(path_data, 'QWA_data_structure.csv'))
df_rings <-  read.csv(file.path(path_data, 'QWA_data_rings.csv'))

df_rings <- df_rings %>%
  select(woodpiece_code, slide_code, image_code, YEAR, n_cells, MRW,
         incomplete_ring, missing_ring, duplicate_ring)

# add button to update df_rings and cov_plot (-> make reactive)
# add button to open image
# save flags to file
# save cov plots to file

# UI
ui <- fluidPage(
  titlePanel("Woodpiece Data Viewer"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "woodpiece",
        "Select Woodpiece Code:",
        choices = unique(df_structure$woodpiece_code),
        selected = unique(df_structure$woodpiece_code)[1]
      ),
      actionButton("click", "Click me!"),
      width = 3
    ),
  mainPanel(
      verbatimTextOutput('clickTest'),
      girafeOutput("covPlot"),
      reactable.extras::reactable_extras_dependency(),
      reactableOutput("reactTable"),
    )
  )
  )

# Server
server <- function(input, output, session) {

  df_filt <- reactive({
    df_rings %>% filter(woodpiece_code == input$woodpiece)
  })

  df_yte <- reactive({
    df_filt() %>%
      select(image_code, YEAR, n_cells, MRW, incomplete_ring, duplicate_ring, missing_ring) %>%
      mutate(user_flag = "")
  })

  p <- reactive({
    p_cov <- plot_woodpiece_coverage(input$woodpiece, df_filt(), save_plot = FALSE)
    p <- girafe(ggobj = p_cov,,
                options = list(
                  opts_hover(css = "fill:red;cursor:pointer;"),
                  opts_selection(type = "single")
                ))
    })

  path_to_img <- reactive({
    req(input$covPlot_selected)
    org_img <- df_structure %>% filter(image_code == input$covPlot_selected) %>% pull(fname_image)
    str_replace(org_img, "\\.jpg", "annotated.jpg")
  })
  output$clickTest <- renderPrint(path_to_img())

  output$covPlot <- renderGirafe({
    p()
  })

  output$reactTable <- renderReactable({
    reactable(df_yte(),
              groupBy = 'image_code',
              pagination = FALSE,
              columns = list(
                image_code = colDef(name = "Image", minWidth = 200),
                incomplete_ring = colDef(name = "incomplete",
                                         cell = function(value) {
                                           if (value) "\u274c" else ""}),
                missing_ring = colDef(name = "missing",
                                      cell = function(value) {
                                        if (value) "\u274c" else ""}),
                duplicate_ring = colDef(name = "duplicate",
                                        cell = function(value) {
                                          if (value) "\u274c" else ""}),
                user_flag = colDef(
                  cell = dropdown_extra(
                  "dropdown",
                  c(' ', '\u274c flag2', '\u2714 flag3'),
                  class = "dropdown-extra"
                ))
                )
              )
  })

#   # Reactive values to store data
#   tableData <- reactiveVal(data)
#
#   # Render DataTable
#   output$filtData <- renderDT({
#     subset(tableData(), woodpiece_code == input$woodpiece) %>%
#       datatable(selection = "single", editable = TRUE)
#   }, server = FALSE)
#
#   # Observe cell selection and toggle missing_ring values
#   observeEvent(input$filtData_cell_edit, {
#     info <- input$filtData_cell_edit
#
#     if (!is.null(info)) {
#       new_data <- tableData()
#       selected_rows <- which(new_data$woodpiece_code == input$woodpiece)
#
#       row_index <- selected_rows[info$row]
#       col_name <- colnames(new_data)[info$col + 1]  # Adjust for zero-based indexing
#
#       if (col_name == "missing_ring") {
#         new_data[row_index, col_name] <- ifelse(is.na(new_data[row_index, col_name]), "ok", NA)
#         tableData(new_data)
#       }
#     }
#   })
}

# Run the application

shinyApp(ui, server)

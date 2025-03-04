library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(reactable)
library(reactable.extras)
library(ggiraph)


#source('../../R/clean_raw_data.R')

#https://forum.posit.co/t/shiny-app-matrix-checkbox/108251/3
#https://rstudio.github.io/DT/extensions.html

# get necessary data
# TODO: should be read from file? or ok bc already in environment?
# path_data <- './out'
# df_structure <- read.csv(file.path(path_data, 'QWA_Arzac2024_meta.csv'))
# df_rings <-  read.csv(file.path(path_data, 'QWA_Arzac2024_rings.csv'))

df_structure <- df_structure
df_rings <- QWA_data$rings %>%
  select(woodpiece_code, slide_code, image_code, year, cno, mrw,
         incomplete_ring, missing_ring, duplicate_ring)

# add button to update df_rings and cov_plot (-> make reactive)
# add button to open image
# save flags to file
# save cov plots to file

# UI
ui <- fluidPage(
  # theme = "bootstrap_wsl.css",
  theme = bs_theme(primary = "#006268", secondary = "#00919A",
                   font_scale = 0.8, preset = "cosmo"),

  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      .selectize-control .selectize-dropdown {
        position: static !important;
      }"))
    ),

  titlePanel("Inspect Year Coverage of Included Woodpieces"),

  card(
    card_header("Select woodpiece (core) to inspect"),
    card_body(
      fillable = FALSE,
      selectInput(
        "woodpiece",
        "Woodpiece Code:",
        choices = unique(df_structure$woodpiece_code),
        selected = unique(df_structure$woodpiece_code)[1]
      ),
      # imageOutput("image"),

    )
  ),

  # verbatimTextOutput('clickTest'),
  card(
    title = "Coverage Plot",
    card_body(
      min_height = 300,
    girafeOutput("covPlot"))
  ),

  card(
      reactable.extras::reactable_extras_dependency(),
      reactableOutput("reactTable"),
  ),

  card(
    card_body(
      fillable = FALSE,
      actionButton("click", "Save user flags"),
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
      select(image_code, year, cno, mrw, incomplete_ring, duplicate_ring, missing_ring) %>%
      mutate(user_flag = "") %>%
      arrange(year)
  })

  p <- reactive({
    covplot <- plot_woodpiece_coverage(input$woodpiece, df_filt())
    p <- girafe(ggobj = covplot$p,
                width_svg = covplot$image_width/300,
                height_svg = covplot$image_height/300,
                options = list(
                  opts_hover(css = "fill:red;cursor:pointer;"),
                  opts_selection(type = "single"),
                  opts_zoom(min = .5, max = 4)
                ))
    })

  # path_to_img <- reactive({
  #   req(input$covPlot_selected)
  #   org_img <- df_structure %>% filter(image_code == input$covPlot_selected) %>% pull(fname_image)
  #   str_replace(org_img, "\\.jpg", "_annotated.jpg") %>% str_replace("..", "../../..")
  # })
  # output$clickTest <- renderPrint({
  #   c(path_to_img(),
  #   file.exists(path_to_img()))
  #   })

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
                  c(' ', '\u2714 remove incomplete flag',
                    '\u2714 remove duplicate flag',
                    '\u274c add incomplete flag',
                    '\u274c add duplicate flag',
                    '\u274c add other issue flag'
                    ),
                  class = "dropdown-extra"
                ))
                )
              )
  })


  # output$image <- renderImage({
  #   req(input$covPlot_selected)
  #   list(src = path_to_img(), width = "25%")
  #   },
  #   deleteFile = FALSE
  # )

  observeEvent(input$covPlot_selected, {
    org_img <- df_structure %>% filter(image_code == input$covPlot_selected) %>% pull(fname_image)
    img_path <- str_replace(org_img, "\\.jpg", "_annotated.jpg") %>% str_replace("..", "../../..")

    if (file.exists(img_path)) {
      # Open image based on OS
      if (.Platform$OS.type == "windows") {
        system2("cmd", c("/c", "start", shQuote(img_path)), wait = FALSE)
      } else if (Sys.info()["sysname"] == "Darwin") {
        system(paste("open", shQuote(img_path)))
      } else {
        system(paste("xdg-open", shQuote(img_path)))
      }
    } else {
      showNotification("Image could not be opened!", type = "error")
    }
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

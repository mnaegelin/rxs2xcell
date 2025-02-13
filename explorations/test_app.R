library(shiny)
library(ggplot2)
library(ggiraph)


shinyApp(ui = fluidPage(

  sidebarLayout(

    sidebarPanel(
      # test input overlay page should not be shown when input changes
      selectInput("test",
                  "This is a test input",
                  choices = c("Model A", "Model B", "Model C"))
    ),
    mainPanel(
      # plot
      girafeOutput("plot")
    ))),

  server = function(input, output) {

    # dynamic overlay page
    # preferably I want to build this page inside R (and not javascriptto,)
    observeEvent(input$plot_selected, {

      info <- subset(mtcars, rownames(mtcars) == input$plot_selected)

      showModal(shiny::modalDialog(

        tags$table(id = "profile",
                   style="width:80%",
                   tags$tr(
                     tags$th(colspan = 3,
                             rownames(info)
                     )
                   ),
                   tags$tr(
                     tags$td(colnames(info)[1]),
                     tags$td(info[, 1])
                   ),
                   tags$tr(
                     tags$td(colnames(info)[2]),
                     tags$td(info[, 2])
                   ),
                   tags$tr(
                     tags$td(colnames(info)[3]),
                     tags$td(info[, 3])
                   ),
                   tags$tr(
                     tags$td(colnames(info)[4]),
                     tags$td(info[, 4])
                   )
        ),
        easyClose = TRUE,
        footer = NULL
      ))
    })

    # plot
    output$plot <- renderGirafe({

      data <- mtcars

      p <- ggplot(aes(x = wt,
                      y = mpg,
                      data_id = row.names(mtcars)
      ),
      data = data) +
        geom_point_interactive(size = 3) +
        theme_minimal()

      girafe(
        ggobj = p,
        options = list(
          opts_hover(css = "fill:red;cursor:pointer;"),
          opts_selection(type = "single")
        )
      )
    })

  }

)

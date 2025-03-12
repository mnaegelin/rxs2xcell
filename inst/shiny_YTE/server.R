#' @import plotly
#' @import shiny
#' @import bslib
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import reactable
#' @import reactable.extras
#' @import ggiraph
#' @import DT


# globals
# TODO: what if data is not available in environment?
df_structure <- df_structure
df_rings <- QWA_data$rings %>%
  select(woodpiece_code, slide_code, image_code, year, cno, mrw,
         incomplete_ring, missing_ring, duplicate_ring)


# reformat data for plot
shiny_cov_plot <- function(df){
  woodpiece <- df$woodpiece_code[1]

  df_plot <- df %>%
    dplyr::mutate(incomplete_ring = dplyr::case_when(incomplete_ring ~ year),
                  missing_ring = dplyr::case_when(missing_ring ~ year),
                  duplicate_ring = dplyr::case_when(duplicate_ring ~ year)) %>%
    # correct order of images
    dplyr::arrange(desc(year)) %>%
    dplyr::mutate(image_code = factor(image_code, levels = unique(image_code)),
                  slide_code = factor(slide_code, levels = rev(unique(slide_code))))

  # PLOTTING
  p_cov <- df_plot %>%
    ggplot2::ggplot(ggplot2::aes(x=year, y=image_code, group=image_code)) +
    # add horizontal lines for the years covered by each image
    ggplot2::geom_line(linewidth = 4, color = "grey90", lineend = 'round',
                       position = ggplot2::position_nudge(y=-0.1)) +
    # with interactive functionality for use in shiny
    # ggiraph::geom_line_interactive(
    #   ggplot2::aes(data_id = image_code),
    #   tooltip = 'Click to open image!',
    #   linewidth = 4, color = "grey90", lineend = 'round',
    #   position = ggplot2::position_nudge(y=-0.1)) +
    #onclick = "window.open(\"https://davidgohel.github.io/ggiraph/\")") +
    # add points colored by the number of cells for each year
    ggplot2::geom_point(ggplot2::aes(x=year, y=image_code, fill=cno),
                        shape=21, size = 3, stroke = 0.1) +
    ggplot2::scale_fill_steps(low='lightskyblue', high='blue4',
                              name = "N cells") +
    # add symbols for the flagged years
    ggnewscale::new_scale_color() +
    ggplot2::geom_point(
      ggplot2::aes(x=missing_ring, y=image_code, col="missing"),
      shape=0, stroke = 0.6, size=7, na.rm = TRUE) +
    ggplot2::geom_point(
      ggplot2::aes(x=duplicate_ring, y=image_code, col="duplicate"),
      shape=1, stroke = 0.7, size = 6, na.rm = TRUE) +
    ggplot2::geom_point(
      ggplot2::aes(x=incomplete_ring, y=image_code, col="incomplete"),
      shape=23, stroke = 0.7, size=4, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c('red4','firebrick1','darkorange2'),
                                breaks = c('incomplete', 'duplicate', 'missing'),
                                name = 'Ring flags') +
    # group images of each slide together
    ggplot2::facet_grid(rows = ggplot2::vars(slide_code),scales = 'free_y') +
    # theme and labels
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_line(colour = "grey25")) +
    ggplot2::labs(
      title = paste("Year coverage for woodpiece", woodpiece),
      x = "Year", y = "Image")
  return(p_cov)
}


# Server
server <- function(input, output, session) {



  # path_to_img <- reactive({
  #   req(input$covPlot_selected)
  #   org_img <- df_structure %>% filter(image_code == input$covPlot_selected) %>% pull(fname_image)
  #   str_replace(org_img, "\\.jpg", "_annotated.jpg") %>% str_replace("..", "../../..")
  # })
  # output$clickTest <- renderPrint({
  #   c(path_to_img(),
  #   file.exists(path_to_img()))
  #   })




  # table
  # reactive values to store the data frames for each woodpiece_code
  df_yte_list <- reactiveValues()

  # get the dataframe for the current woodpiece_code
  get_df_yte <- reactive({
    sel_wp <- input$woodpiece
    if (!is.null(df_yte_list[[sel_wp]])) {
      return(df_yte_list[[sel_wp]])
    } else {
      # if it doesn't yet exist, initialize from df_rings
      df <- df_rings %>%
        filter(woodpiece_code == sel_wp) %>%
        select(woodpiece_code, slide_code,
               image_code, year, cno, mrw,
               incomplete_ring, duplicate_ring, missing_ring) %>%
        mutate(user_flag = FALSE) %>%
        arrange(desc(image_code), year)
      return(df)
    }
  })

  reset_df_yte <- observeEvent(input$btn_reset, {
    sel_wp <- input$woodpiece
    df_yte_list[[sel_wp]] <- NULL
  })

  output$testing <- renderPrint({
    input$tbl_cells_selected
  })

  # update the table when cells in flag columns are clicked
  observeEvent(req(input$tbl_cells_selected), {
    sel_wp <- input$woodpiece
    df <- get_df_yte()
    cell <- input$tbl_cells_selected
    if (!is.null(cell)) {
      row <- cell[1]
      col <- cell[2]
      if (col %in% c(4,5,6,7)) { # make changes only for flags columns
        # NOTE: the column index is 0-based bc of JS, and +2 for removed woodpiece_code, slide_code columns
        df[row, col+3] <- !df[row, col+3] # toggle the value
      }
      df_yte_list[[sel_wp]] <- df # update the reactive value
    }
  })


  # plot
  p <- reactive({
    covplot <- plot_woodpiece_coverage(input$woodpiece, get_df_yte())
    p <- girafe(ggobj = covplot$p,
                width_svg = covplot$image_width/300,
                height_svg = covplot$image_height/300,
                options = list(
                  opts_hover(css = "fill:red;cursor:pointer;"),
                  opts_selection(type = "single"),
                  opts_zoom(min = .5, max = 4)
                ))
  })

  output$covPlot <- renderGirafe({
    p()
  })

  p2 <- reactive({
    covplot <- shiny_cov_plot(get_df_yte())
  })

  output$covPlot2 <- renderPlotly(
    ggplotly(p2())
  )



  # render the table
  output$tbl <- renderDT({
    datatable(get_df_yte() %>% select(-woodpiece_code, -slide_code),
              extensions = 'RowGroup',
              rownames = FALSE,
              colnames = c('Image', 'Year', 'Nr of Cells', 'MRW',
                           'Incomplete', 'Duplicate', 'Missing', 'User flag'),
              selection = list(mode = "single", target = 'cell'),
              escape = FALSE, # to show icons
              options = list(
                rowGroup = list(dataSrc = 0), # group by image_code (first col)
                searching = FALSE,
                scrollY = "400px", # Set the height of the scrollable area
                paging = FALSE, # Disable pagination
                columnDefs = list(
                  list(targets = "_all", orderable = FALSE),
                  list(targets = c(4,5,6,7), className = 'dt-center'),
                  list(targets = c(4,5,6,7), render = JS(
                    "function(data, type, row, meta) {",
                    "return data ? '<span class=\"glyphicon glyphicon-remove\"></span>' : '';",
                    "}")
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

# shinyApp(ui, server)

# shinyApp(ui, server)


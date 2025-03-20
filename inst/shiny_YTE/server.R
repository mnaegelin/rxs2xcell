

# globals
# TODO: what if data is not available in environment?
df_structure <- df_structure

df_rings <- QWA_data$rings
# duplicate_sel is the one with best duplicate_rank out of all per
# woodpiece_code and year group, prioritizing non missing / incomplete rings
df_rings <- df_rings %>%
  dplyr::group_by(woodpiece_code, year) %>%
  dplyr::mutate(
    dupl_candidates = dplyr::if_else(!missing_ring & !incomplete_ring,
                             duplicate_rank, 100+duplicate_rank), # penalty for issues
    duplicate_sel = dplyr::if_else(dupl_candidates == min(dupl_candidates),
                           TRUE, FALSE)) %>%
  dplyr::select(-dupl_candidates) %>%
  dplyr::ungroup()
# initialize the other issues cols
df_rings <- df_rings %>%
  dplyr::mutate(other_issues = FALSE,
                other_reason = NA_character_) %>%
  dplyr::select(woodpiece_code, slide_code, image_code,
                year, cno, mrw, missing_ring, duplicate_ring,
                duplicate_sel, incomplete_ring, other_issues, other_reason,
                duplicate_rank)

# reformat data for plot
shiny_cov_plot <- function(df){
  #woodpiece <- df$woodpiece_code[1]

  # reformat data for the plot
  df_plot <- df %>%
    dplyr::mutate(
      # set duplicate to FALSE if duplicate_sel is TRUE (we plot duplicate_sel instead)
      duplicate_ring = dplyr::if_else(duplicate_sel | is.na(duplicate_sel),
                                      FALSE, duplicate_ring),
      # labels for the hover text
      lbl_inc = dplyr::case_when(incomplete_ring ~ "incomplete"),
      lbl_dupl = dplyr::case_when(duplicate_ring ~ "duplicate"),
      lbl_dupl_sel = dplyr::case_when(duplicate_sel ~ "best duplicate"),
      lbl_miss = dplyr::case_when(missing_ring ~ "missing"),
      lbl_other = dplyr::case_when(other_issues ~ "other"),
      # to plot the years with the respective flags
      incomplete_ring = dplyr::case_when(incomplete_ring ~ year),
      duplicate_ring = dplyr::case_when(duplicate_ring ~ year),
      duplicate_sel = dplyr::case_when(duplicate_sel ~ year),
      missing_ring = dplyr::case_when(missing_ring ~ year),
      other_issues = dplyr::case_when(other_issues ~ year)) %>%
    # to create the hover labels
    tidyr::unite(col = "labels", lbl_inc, lbl_dupl, lbl_dupl_sel, lbl_miss, lbl_other,
                 na.rm = TRUE, sep = ", ", remove = TRUE) %>%
    dplyr::mutate(
      labels = dplyr::if_else(labels == "", "none", labels),
      labels = paste0("flags: ", labels)) %>%
    # correct order of images to start with earliest
    dplyr::arrange(desc(year)) %>%
    dplyr::mutate(image_code = factor(image_code, levels = unique(image_code)),
                  slide_code = factor(slide_code, levels = rev(unique(slide_code))))

  # x limits for plot
  min_year <- floor(min(df_plot$year, na.rm = TRUE) / 5) * 5
  max_year <- ceiling(max(df_plot$year, na.rm = TRUE) / 5) * 5
  #max_cno <- ceiling(max(df_plot$cno, na.rm = TRUE) / 100) * 100

  # PLOTTING
  p_cov <- suppressWarnings({ # to avoid the unknown aes 'text' warning
    df_plot %>%
      ggplot2::ggplot(ggplot2::aes(x=year, y=image_code, group=image_code)) +
      #add horizontal lines for the years covered by each image
      ggplot2::geom_line(linewidth = 4, color = "grey85", lineend = 'round',
                         position = ggplot2::position_nudge(y=-0.1)) +
      # add points colored by the number of cells for each year
      ggplot2::geom_point(ggplot2::aes(fill = cno, text = labels),
                          shape=21, size = 3, stroke = 0.1) +
      ggplot2::scale_fill_gradient(low='lightskyblue', high='blue4') +
      # add symbols individually for all the flagged years
      ggplot2::geom_point(
        ggplot2::aes(x=incomplete_ring, y=image_code),
        color = 'red4', shape=23, stroke = 0.5, size=4, na.rm = TRUE) +
      ggplot2::geom_point(
        ggplot2::aes(x=duplicate_ring, y=image_code),
        color = 'firebrick1', shape=1, stroke = 0.5, size = 5, na.rm = TRUE) +
      ggplot2::geom_point(
        ggplot2::aes(x=duplicate_sel, y=image_code),
        color = 'springgreen4', shape=1, stroke = 0.5, size = 5, na.rm = TRUE) +
      ggplot2::geom_point(
        ggplot2::aes(x=missing_ring, y=image_code),
        color = 'darkorange2', shape=0, stroke = 0.4, size=6, na.rm = TRUE) +
      ggplot2::geom_point(
        ggplot2::aes(x=other_issues, y=image_code),
        color = 'gold', shape=1, stroke = 0.5, size=4, na.rm = TRUE) +
      # ggplot2::geom_point(
      #   ggplot2::aes(x=other_issues, y=image_code),
      #   color = 'green', shape=1, stroke = 0.5, size=3, na.rm = TRUE) +
      # 5-year ticks on x axis
      ggplot2::scale_x_continuous(breaks = seq(min_year, max_year, by = 5)) +
      # group images of each slide together
      ggplot2::facet_grid(rows = ggplot2::vars(slide_code),scales = 'free_y') +
      # theme and labels
      ggplot2::theme_minimal() +
      ggplot2::theme(strip.text = ggplot2::element_blank(),
                     axis.line.x = ggplot2::element_line(colour = "grey25"),
                     axis.text.y = ggplot2::element_text(size = 7),
                     axis.title.y = ggplot2::element_blank()) +
      ggplot2::labs(x = "Year", fill = 'Cell count')
  })

  p_cov <- plotly::ggplotly(p_cov, tooltip = c('x','y','labels'))
  return(p_cov)
}


# Server
server <- function(input, output, session) {

  # Reactive logics -----------------------------------------------------------

  # initialize list of reactive values to store the data for each woodpiece_code
  df_yte_list <- reactiveValues()
  for (sel_wp in unique(df_rings$woodpiece_code)){
    df_yte_list[[sel_wp]] <- df_rings %>%
        dplyr::filter(woodpiece_code == sel_wp) %>%
        dplyr::arrange(desc(image_code), year)
    }


  # reset the dataframe for the current woodpiece_code when RESET btn is clicked
  reset_df_yte <- observeEvent(input$btn_reset, {
    sel_wp <- input$woodpiece
    df_yte_list[[sel_wp]] <- df_rings %>%
      dplyr::filter(woodpiece_code == sel_wp) %>%
      dplyr::arrange(desc(image_code), year)
  })

  # update the table when cells in flag columns are clicked
  observeEvent(req(input$tbl_cells_selected), {
    sel_wp <- input$woodpiece
    # df <- get_df_yte()
    df <- df_yte_list[[sel_wp]]
    cell <- input$tbl_cells_selected
    if (!is.null(cell)) {
      row <- cell[1]
      col <- cell[2]

      if (col %in% c(6,7,8)) { # make changes only for flags columns
        # NOTE: the column index is 0-based bc of JS, and +2 for removed woodpiece_code, slide_code columns
        df[row, col+3] <- !df[row, col+3] # toggle the value

        df_yte_list[[sel_wp]] <- df # update the reactive value

        # from trying to avoid a table refresh on edit (did not help)
        # #Send proxy (no need to refresh whole table)
        # df_tbl()[row,col+1] <- isolate(DT::coerceValue(val, df_tbl()[row+1,col+1]))
        #output$testing <- renderPrint({c(row, col, val, df_tbl$df[row,col+1])})
        #df_tbl$df[row, col+1] <<- DT::coerceValue(val, df_tbl$df[row, col+1])
        #df_tbl$df[row, col+1] <- isolate(DT::coerceValue(val, df_tbl$df[row, col+1]))
        #DT::replaceData(proxy, df_tbl$df, resetPaging = FALSE)
        # df_tbl()[row,col+1] <- isolate(DT::coerceValue(val, df_tbl()[row,col+1]))
        # df_tbl() <- isolate(DT::editData(df_tbl(), info = list(list(row = row, col = col+1, value = val)), proxy = proxy))
        #DT::replaceData(proxy, df_tbl(), resetPaging = FALSE)
      }
    }
  })

  # The proxy to update the DT (for avoiding reload on edit, but didn't help)
  # proxy <- DT::dataTableProxy('tbl')


  # update the table when comment cells are edited
  observeEvent(req(input$tbl_cell_edit), {
    sel_wp <- input$woodpiece
    # df <- get_df_yte()
    df <- df_yte_list[[sel_wp]]
    info <- input$tbl_cell_edit
    if (!is.null(info)) {
      row <- info$row
      col <- info$col
      val <- info$value

      if (col == 9) { # make changes only for comment column
        df[row, col+3] <- val # update the value
        df_yte_list[[sel_wp]] <- df
      }
    }
  })


  # VALIDITY CHECK when changing the selected woodpiece
  # the validation check function
  validate_data <- function(df_wp){
    comments_for_all_userflags <- df_wp %>%
      dplyr::filter(other_issues & (is.na(other_reason) | other_reason == "")) %>%
      nrow() == 0
    comments_only_for_userflags <- df_wp %>%
      dplyr::filter(!other_issues & !is.na(other_reason) & other_reason != "") %>%
      nrow() == 0
    one_sel_dupl_group <- df_wp %>%
      dplyr::filter(duplicate_sel) %>%
      dplyr::count(woodpiece_code,year) %>% dplyr::filter(n>1) %>% nrow() == 0

    return(comments_for_all_userflags & comments_only_for_userflags & one_sel_dupl_group)
  }

  # # keep track of the previous and current sel_wp
  # old_value <- reactiveVal(NULL)
  # disable_observer <- reactiveVal(FALSE)
  #
  # observeEvent(input$woodpiece, {
  #   # abort if the observer is disabled (to avoid re-triggering alert with updateSelectInput)
  #   if (disable_observer()) {
  #     disable_observer(FALSE)
  #     return()
  #   }
  #
  #   # if we are on first woodpiece, just store the value
  #   if (is.null(old_value())) {
  #     old_value(input$woodpiece)
  #     return()
  #   }
  #
  #   # run the check
  #   val_check <- validate_data(df_yte_list[[old_value()]])
  #
  #   if (!val_check) {
  #     # TODO: update error message based on the failed checks
  #     shinyalert::shinyalert("Invalid flags!",
  #                            "Please check the flags for the current woodpiece.",
  #                            type = "error")
  #     disable_observer(TRUE)
  #     updateSelectInput(session, "woodpiece", selected = old_value())
  #     return()
  #   } else {
  #     old_value(input$woodpiece)
  #   }
  #
  # })

  # save the current flags
  # TODO:
  # observeEvent(input$btn_submit, {
  #   check validty
  #   save to a fixed df?
  # })

  # output$progress_txt <- renderText({
  #   paste(unique(df_rings$woodpiece_code), collapse = '\n')
  # })

  # save the data to a file
  # TODO:
  # observeEvent(input$btn_save, {
  #   get_df_yte() %>%
  #     write.csv('test_rings.csv', row.names = FALSE)
  # })

  # TODO: disable download button if validation fails
  # df_all <- reactiveValuesToList(df_yte_list) %>% dplyr::bind_rows()
  # val_check <- validate_data(df_all)
  # if (!val_check) {
  #   shinyalert::shinyalert("Invalid flags!",
  #                          "Please check the flags for all woodpieces.",
  #                          type = "error")
  #   return()
  # } else {

  combined_df <- reactive({
    reactiveValuesToList(df_yte_list) %>% dplyr::bind_rows()
  })

  # TODO: add message on what checks are failed
  observe({
    val_check <- validate_data(combined_df())
    if(!val_check) {
      shinyjs::disable('btn_save')
      shinyjs::disable('woodpiece')
    } else {
      shinyjs::enable('btn_save')
      shinyjs::enable('woodpiece')
    }
  })

  output$btn_save <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_QWA_rings_with_flags.csv")
    },
    content = function(file) {
        write.csv(combined_df(), file, row.names = FALSE)
    }
  )


  # the plot -----------------------------------------------------------
  df_plot <- reactive({
    df_yte_list[[input$woodpiece]]
  })

  output$covPlot <- plotly::renderPlotly(
    shiny_cov_plot(df_plot())
    # plotly::ggplotly(
    #   shiny_cov_plot(df_plot()),
    #   tooltip = "text")
    #%>% plotly::layout(yaxis = list(automargin = FALSE, title = list(standoff = 200)))
    # %>% plotly::layout(yaxis = list(tickfont = list(size = 8))), would need to specify for all facets
  )

  # the table -----------------------------------------------------------
  df_tbl <- reactive({
    sel_wp <- input$woodpiece
    df_yte_list[[sel_wp]] %>% dplyr::select(-woodpiece_code, -slide_code)
  })

  # render the table
  output$tbl <- DT::renderDT({
    #sel_wp <- input$woodpiece
    DT::datatable(df_tbl(),
                  style = 'default',
                  extensions = c('RowGroup'),
                  rownames = FALSE,
                  colnames = c('Image', 'Year', 'Nr of Cells', 'MRW',
                              'Missing', 'Duplicate', 'Selected Dupl.', 'Incomplete', 'Other Issues', 'Comment', 'duplicate_rank'),
                  selection = list(mode = "single", target = 'cell'),
                  editable = list(target = "cell", disable = list(columns = 0:8)),
                  escape = FALSE, # to show icons
                  options = list(
                    rowGroup = list(dataSrc = 0), # group by image_code (first col)
                    searching = FALSE,
                    scrollY = "500px", # Set the height of the scrollable area
                    paging = FALSE, # Disable pagination
                    columnDefs = list(
                      list(targets = "_all", orderable = FALSE),
                      list(targets = c(4,5,6,7,8), className = 'dt-center'),
                      list(targets = c(4,5), render = DT::JS(
                        "function(data, type, row, meta) {",
                        "return data ? '<span class=\"glyphicon glyphicon-remove\"></span>' : '';",
                        "}")
                      ),
                      list(targets = c(6), render = DT::JS(
                        "function(data, type, row, meta) {",
                        "return data ? '<span class=\"glyphicon glyphicon-ok\" style=\"color: green\"></span>' : '';",
                        "}")
                      ),
                      list(targets = c(7,8), render = DT::JS(
                        "function(data, type, row, meta) {",
                        "return data ? '<span class=\"glyphicon glyphicon-remove\" style=\"color: red\"></span>' : '';",
                        "}")
                      ),
                      list(targets = c(10), visible = FALSE) # hide duplicate_rank column from view
                    )
                  )
    ) %>% DT::formatStyle(
      columns = c('image_code', 'year'),
      backgroundColor = '#CCE0E0'
    ) %>% DT::formatStyle(
      'duplicate_sel', 'duplicate_ring',
      backgroundColor = DT::styleEqual(FALSE, '#E6F0F0')
    ) %>% DT::formatStyle(
      columns = c('cno', 'mrw', 'missing_ring', 'duplicate_ring'),
      backgroundColor = '#E6F0F0'
    )
  })

  # output$testing <- renderPrint({
  #   req(input$tbl_cells_selected)
  #   df_yte_list[[input$woodpiece]] %>%
  #     dplyr::select(image_code, year, missing_ring, other_issues)
  # })

}


  # # new try ------
  # # initialize reactive df_list from df_rings
  # df_list <- reactiveValues()
  # for (sel_wp in unique(df_rings$woodpiece_code)){
  #   df_list[[sel_wp]] <- df_rings %>%
  #     dplyr::filter(woodpiece_code == sel_wp) %>%
  #     dplyr::select(woodpiece_code, slide_code,
  #                   image_code, year, cno, mrw,
  #                   incomplete_ring, duplicate_ring, missing_ring, other_issues) %>%
  #     dplyr::arrange(desc(image_code), year)
  # }
  #
  # # click on reset button overwrites the df_list entry for the current wp with original data
  # reset_df_wp <- observeEvent(input$btn_reset, {
  #   sel_wp <- input$woodpiece
  #   df_list[[sel_wp]] <- df_rings %>%
  #     dplyr::filter(woodpiece_code == sel_wp) %>%
  #     dplyr::select(woodpiece_code, slide_code,
  #                   image_code, year, cno, mrw,
  #                   incomplete_ring, duplicate_ring, missing_ring, other_issues) %>%
  #     dplyr::arrange(desc(image_code), year)
  # })
  #
  #
  # df_all <- reactiveVal(
  #   df_rings %>%
  #     dplyr::select(woodpiece_code, slide_code,
  #                   image_code, year, cno, mrw,
  #                   incomplete_ring, duplicate_ring, missing_ring, other_issues) %>%
  #     dplyr::arrange(desc(image_code), year)
  # )
  #
  # observe({input$woodpiece
  #   # Filter data
  #   data <- df_all() %>% dplyr::filter(woodpiece_code == input$woodpiece) %>%
  #     dplyr::select(-woodpiece_code, -slide_code)
  #   reactable::updateReactable("reactTable",
  #     data = data)
  # })
  #
  #
  # output$reactTable <- reactable::renderReactable({
  #   #sel_wp <- input$woodpiece
  #   reactable::reactable(
  #     df_all() %>% dplyr::select(-woodpiece_code, -slide_code),
  #     groupBy = 'image_code',
  #     pagination = FALSE,
  #     columns = list(
  #       image_code = reactable::colDef(name = "Image", minWidth = 150),
  #       incomplete_ring = reactable::colDef(
  #         name = "Incomplete",
  #         cell = reactable.extras::checkbox_extra("check_inc", class = "checkbox-extra"),
  #         align = "center"
  #       ),
  #       duplicate_ring = reactable::colDef(
  #         name = "Duplicate",
  #         cell = reactable.extras::checkbox_extra("check_dupl", class = "checkbox-extra"),
  #         align = "center"
  #       ),
  #       missing_ring = reactable::colDef(
  #         name = "Missing",
  #         cell = reactable.extras::checkbox_extra("check_miss", class = "checkbox-extra"),
  #         align = "center"
  #       ),
  #       other_issues = reactable::colDef(
  #         name = "Other",
  #         cell = reactable.extras::checkbox_extra("check_other", class = "checkbox-extra"),
  #         align = "center"
  #       )
  #     )
  #   )
  # })




  # output$image <- renderImage({
  #   req(input$covPlot_selected)
  #   list(src = path_to_img(), width = "25%")
  #   },
  #   deleteFile = FALSE
  # )

  # observeEvent(input$covPlot_selected, {
  #   org_img <- df_structure %>% dplyr::filter(image_code == input$covPlot_selected) %>% dplyr::pull(fname_image)
  #   img_path <- stringr::str_replace(org_img, "\\.jpg", "_annotated.jpg") %>% stringr::str_replace("..", "../../..")
  #
  #   if (file.exists(img_path)) {
  #     # Open image based on OS
  #     if (.Platform$OS.type == "windows") {
  #       system2("cmd", c("/c", "start", shQuote(img_path)), wait = FALSE)
  #     } else if (Sys.info()["sysname"] == "Darwin") {
  #       system(paste("open", shQuote(img_path)))
  #     } else {
  #       system(paste("xdg-open", shQuote(img_path)))
  #     }
  #   } else {
  #     showNotification("Image could not be opened!", type = "error")
  #   }
  # })





    # # Reactive values to store data
    # tableData <- reactiveVal(data)
    #
    # # Render DataTable
    # output$filtData <- renderDT({
    #   subset(tableData(), woodpiece_code == input$woodpiece) %>%
    #     datatable(selection = "single", editable = TRUE)
    # }, server = FALSE)
    #
    # # Observe cell selection and toggle missing_ring values
    # observeEvent(input$filtData_cell_edit, {
    #   info <- input$filtData_cell_edit
    #
    #   if (!is.null(info)) {
    #     new_data <- tableData()
    #     selected_rows <- which(new_data$woodpiece_code == input$woodpiece)
    #
    #     row_index <- selected_rows[info$row]
    #     col_name <- colnames(new_data)[info$col + 1]  # Adjust for zero-based indexing
    #
    #     if (col_name == "missing_ring") {
    #       new_data[row_index, col_name] <- ifelse(is.na(new_data[row_index, col_name]), "ok", NA)
    #       tableData(new_data)
    #     }
    #   }
    # })




# path_to_img <- reactive({
#   req(input$covPlot_selected)
#   org_img <- df_structure %>% filter(image_code == input$covPlot_selected) %>% pull(fname_image)
#   str_replace(org_img, "\\.jpg", "_annotated.jpg") %>% str_replace("..", "../../..")
# })
# output$clickTest <- renderPrint({
#   c(path_to_img(),
#   file.exists(path_to_img()))
#   })

# plot
# p <- reactive({
#   covplot <- plot_woodpiece_coverage(input$woodpiece, get_df_yte())
#   p <- girafe(ggobj = covplot$p,
#               width_svg = covplot$image_width/300,
#               height_svg = covplot$image_height/300,
#               options = list(
#                 opts_hover(css = "fill:red;cursor:pointer;"),
#                 opts_selection(type = "single"),
#                 opts_zoom(min = .5, max = 4)
#               ))
# })
#
# output$covPlot <- renderGirafe({
#   p()
# })


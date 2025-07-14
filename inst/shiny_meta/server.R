# server -----------------------------------------------------------------------
library(magrittr)
library(bslib)
server <- function(input, output, session) {

  start_info <- start_server('start', session)

  #table_configs <- jsonlite::read_json("./inst/shiny_meta/www/table_configs.json")
  table_configs <- jsonlite::read_json("www/table_configs.json")

  # TODO: add: species info, countries_sf vars as inputs
  dataset_info <- dataset_server('ds', session, start_info, countries_list,
                                 table_configs$author_tbl, table_configs$funding_tbl)

  site_info <- site_server('site', session, start_info, countries_list,
                           table_configs$site_tbl, table_configs$tree_tbl,
                           table_configs$woodpiece_tbl, table_configs$slide_tbl)

  summary_server('summary', session, start_info, dataset_info, site_info)


   output$debug <- renderPrint({
     example_run
   })

  observeEvent(input$contact_link, {
    showModal(modalDialog(
      title = "Publisher and contact information",
      "Swiss Federal Institute for Forest, Snow and Landscape Research WSL",
      tags$br(),
      "Zürcherstrasse 111",
      tags$br(),
      "8903 Birmensdorf",
      tags$br(),
      "Switzerland",
      tags$br(),
      tags$br(),
      strong("Contact:"), "Mara Nägelin, ",
      tags$a(href = "mailto:mara.naegelin@wsl.ch","mara.naegelin(at)wsl.ch"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

}


#tree_info <- tree_server('tree', session, start_info)
# RADIALNETWORK TRY
# Modified JavaScript code to send the selected node to Shiny for the networkD3 plot
# clickJS <- '
# d3.selectAll(".node").on("click", function(d){
#   Shiny.onInputChange("selected_node", d.data.name);
# })
# '

# dtree_list <- data.tree::ToListExplicit(dtree, unname = TRUE)
# dtree_list

# })

# output$radial_network <- networkD3::renderDiagonalNetwork({
#   onRender(networkD3::diagonalNetwork(dtree_list()), clickJS)
# })

# Display the selected node in verbatimTextOutput
# output$selectedNode <- renderPrint({
#   input$selected_node
# })

# SUNBURST TRY
# df_sb <- data.tree::ToDataFrameNetwork(dtree) %>%
#   dplyr::rename(parents = from, ids = to) %>%
#   dplyr::mutate(labels = stringr::str_split_i(ids, "/", -1))
# df_sb[df_sb$parents == 'dataset', 'parents'] = ""

# fig <- plotly::plot_ly(
#   df_sb, ids = ~ids, labels = ~labels, parents = ~parents,
#   type = 'sunburst',
#   insidetextorientation='tangential'
#   ) %>% plotly::layout(showlegend = TRUE)
# fig
#

# DENDROGRAM TRY
# tree_data <- ggdendro::dendro_data(as.dendrogram(dtree))
#
# p <- ggplot2::ggplot() +
#   ggplot2::geom_segment(data = tree_data$segments, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
#                colour = "blue", alpha = 0.5) +
#   ggplot2::geom_text(data = tree_data$labels,
#             ggplot2::aes(x = x, y = y, label = label), vjust = -0.5, size = 3) +
#   # ggplot2::geom_text(data = ggdendro::leaf_label(tree_data),
#   #           ggplot2::aes(x = x, y = y, label = label), vjust = 0.5, size = 2) +
#   ggplot2::coord_flip() +
#   ggdendro::theme_dendro()
# p

# shinyTree
#   # helper to get the path of the highest level selected in the tree
# get_selected_highest <- function(tree, ancestry = "", selected = c()) {
#   for (leaf in names(tree)){
#     a <- attr(tree[[leaf]], 'stselected', TRUE)
#     if (!is.null(a) && a == TRUE) {
#       selected <- c(selected, paste0(ancestry, "/", leaf))
#     } else {
#       selected <- get_selected_highest(tree[[leaf]], paste0(ancestry, "/", leaf), selected)
#     }
#   }
#   return(selected)
# }

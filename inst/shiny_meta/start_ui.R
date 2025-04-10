start_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(

    # sidebar: Input data
    sidebar = sidebar(
      title = "Instructions", #paste(icon("info", style = paste("color:", prim_col)),
      span("If QWA metadata is available in the current R environment,
            it will be used directly. Alternatively, you can browse for a
            saved QWA metadata file."),
      card(
        card_header(
          class = 'bg-secondary',
          'Input data'),
        fileInput(ns("file_upload"), "Load metadata from file (csv)", accept = ".csv"),
        span(
          span('Source of shown data:', style=paste("font-weight:bold; color:", sec_col)),
          htmlOutput(ns("file_status"))
        ),
        #textOutput("file_status") #span(textOutput("file_status"), style="font-weight:bold; color: #006268"),
      ),
      hr(),
      span('If the structure and metadata shown on the right are as you
            expect them for your dataset, click the button below to
            proceed to the next tab.'),
      actionButton(ns('btn_next'), 'Next', icon = icon('angle-double-right'))
    ),

    # main content
    h5('Overview of input metadata'),
    accordion(
      open = c("Data structure", 'DT Available metadata'),

      accordion_panel(
        title = "Data structure",
        p("Use this data.tree to explore the inferred structure of the
                 provided dataset and filter the metadata table below."),
        #verbatimTextOutput("testing1"),
        #networkD3::diagonalNetworkOutput("radial_network"),
        #networkD3::radialNetworkOutput("radial_network"),
        #verbatimTextOutput('selectedNode')
        shinyTree::shinyTree(ns("tree"), theme="proton", checkbox = TRUE, tie_selection = TRUE,
                             whole_node = TRUE, three_state = TRUE,
                             wholerow = FALSE)
      ),

      accordion_panel(
        title = "DT Available metadata",
        fillable = FALSE,
        DT::DTOutput(ns('DTmeta'))
      )

      # accordion_panel(
      #   title = "Available metadata",
      #   fillable = FALSE,
      #   reactable::reactableOutput("partial_meta_out")
      #   # card(
      #   #   card_header('Overview of input metadata'),
      # )

    ) # end of accordion
  ) # end of layout_sidebar

}

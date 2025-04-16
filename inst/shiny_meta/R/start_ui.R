start_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(

    # sidebar: Input data
    sidebar = sidebar(
      title = "Instructions", #paste(icon("info", style = paste("color:", prim_col)),

      card(
        class = 'card-note',
        card_header(
          class = 'bg-secondary',
          'Input data'),
        radioButtons(
          inputId = ns("input_src"),
          label = "Choose a data source:",
          choices = list(
            "Use df_meta available in R environment" = "df_meta_env",
            "Use df_meta loaded from .csv" = "df_meta_csv",
            "Use partially completed metadata contribution from .json" = "df_meta_json"
          ),
          selected = character(0)
        ),
        fileInput(
          inputId = ns("file_input"),
          label = "Upload file:",
          accept = c(".csv", ".json")
        ),
        actionButton(
          inputId = ns("btn_load_input"),
          label = "Load data"
        )
      ),

      # card(
      #   class = 'card-note',
      #   card_header(
      #     class = 'bg-secondary',
      #     'Input data'),
        # span("If extracted QWA metadata (", code('df_meta'),  ") is available
        # in the current R environment, it will be used directly.
        # Alternatively, you can browse for a .csv of extracted QWA metadata.
        # If you have previously started the process of contributing dataset
        # metadata and want to continue from a partial export, provide the .json
        # of partial dataset metadata."),
        # fileInput(ns("file_upload"), "Load metadata from file (csv or json)", accept = c(".csv",'json')),
        # span(
        #   span('Source of shown data:', style=paste("font-weight:bold; color:", sec_col)),
        #   htmlOutput(ns("file_status"))
        # ),
        #textOutput("file_status") #span(textOutput("file_status"), style="font-weight:bold; color: #006268"),
      # ),

      hr(),
      span(
        span('Source of shown data:', style=paste("font-weight:bold; color:", prim_col)),
        htmlOutput(ns("file_status"))
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
      open = c("Data structure", 'Metadata extracted from files'),

      accordion_panel(
        title = "Data structure",
        p("Use this data.tree (site > tree > woodpiece > slide > image) to explore the inferred structure of the
                 provided dataset and filter the metadata table below."),
        #verbatimTextOutput("testing1"),
        #networkD3::diagonalNetworkOutput("radial_network"),
        #networkD3::radialNetworkOutput("radial_network"),
        shinyTree::shinyTree(ns("tree"), theme="proton", checkbox = TRUE, tie_selection = TRUE,
                             whole_node = TRUE, three_state = TRUE,
                             wholerow = FALSE),
        verbatimTextOutput(ns('notree'))
      ),

      accordion_panel(
        title = "Metadata extracted from files",
        fillable = FALSE,
        DT::DTOutput(ns('DTmeta')),
        verbatimTextOutput(ns('noDTmeta'))
      )

      # accordion_panel(
      #   title = "Available metadata",
      #   fillable = FALSE,
      #   reactable::reactableOutput("partial_meta_out")
      #   # card(
      #   #   card_header('Overview of input metadata'),
      # )

    ), # end of accordion

    verbatimTextOutput(ns("testing"))
  ) # end of layout_sidebar

}

start_ui <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(

    # sidebar: Input data
    sidebar = bslib::sidebar(
      title = "Instructions", #paste(icon("info", style = paste("color:", prim_col)),

      span("We start from the raw metadata inferred from images and ROXAS settings
           files with the", code('rxs2xcell'), 'package.'),
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

      hr(),
      span(
        span('Source of shown data:', style=paste("font-weight:bold; color:", prim_col)),
        htmlOutput(ns("file_status"))
      ),

      hr(),
      span('If the structure and metadata shown on the right are as you
            expect them for your dataset, check the box and proceed to the next tab.'),
      checkboxInput(ns("check_raw"), "Inferred structure and metadata are correct.", value = FALSE),
      actionButton(ns('btn_next'), 'Next', icon = icon('angle-double-right'))
    ),

    # main content
    h5('Overview of input metadata'),
    accordion(
      open = c("Data structure", 'Metadata extracted from files'),

      accordion_panel(
        title = "Data structure",
        p("Use this data.tree (site > tree > woodpiece > slide > image) to
           explore the inferred structure of the provided dataset and filter the
           metadata table below. Note that filtering here does not exclude any
           images from the dataset."),

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

    ), # end of accordion

    verbatimTextOutput(ns("testing"))
  ) # end of layout_sidebar

}


# old:
#reactable::reactableOutput("partial_meta_out")
#networkD3::diagonalNetworkOutput("radial_network"),
#networkD3::radialNetworkOutput("radial_network"),

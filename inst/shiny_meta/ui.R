# library(shiny)
# library(bslib)

# allow to create new options in dropdown select:
# selectizeInput(
#   'e3', '3. Item creation', choices = state.name,
#   options = list(create = TRUE)
# ),

# searchable choose inputs (e.g. for species)
# selectInput('in4', 'Options', c(Choose='', state.name), selectize=TRUE)
# and should also be able to customize labels to show species code
# see https://shiny.posit.co/r/articles/build/selectize/


#' @importFrom magrittr %>%
library(magrittr)
library(bslib)

theme <- NULL

theme <- bslib::bs_theme(version = 5, primary = prim_col, secondary = sec_col,
                  info = tert_col, font_scale = 0.8, preset = "zephyr",
                  "body-bg" = prim_col_grad[5], "card-border-width" = 0,
                  "focus-ring-color" = sec_col_grad[4])
theme <-  bslib::bs_add_rules(theme, HTML(paste0("
    .btn-secondary {
      color: white;
    }
    .btn-info {
      background-color: ", tert_col, ";
    }
    .sidebar {
      background-color: ", prim_col_grad[4], " !important;
      color: #212529 !important;
    }


    /* styling the navbar: primary bg color, white font, hover, active*/
    .bslib-navs-card-title {
      background-color: ", prim_col, ";
      color: white;
      font-size: 12pt;
    }
    .nav-link {
      font-size: 10pt;
      color: ", prim_col_grad[3], ";
      --bs-nav-link-hover-color: white;
      --bs-nav-underline-link-active-color: white;
    }



    /* class for a code style output*/
    .code-output {
      display:block;
      padding:9.5px;
      margin:0 0 10px;
      margin-top:10px;
      line-height:20px;
      word-break:break-all;
      word-wrap:break-word;
      white-space:pre-wrap;
      border:1px solid rgba(0,0,0,0.15);
      border-radius:4px;
      font-family:monospace;
      background-color:", prim_col_grad[5], " !important;
    }

    /* class for a note stlye card */
    .card-note .card-header{
      padding: 8px;
    }
    .card-note .card-body{
      padding: 8px;
    }

    /* styling the shinyTree resp. jstree */
    .jstree-proton .jstree-clicked {
      background: ", prim_col_grad[1], " !important;
    }

    /* styling the accoridons */
    .accordion-button:not(.collapsed) {
      background-color: ", prim_col_grad[1], " !important;
      color: white !important;
    }
    .accordion-button {
      background-color: ", prim_col_grad[4], ";
    }
    .accordion-tert .accordion-button:not(.collapsed) {
      background-color: ", tert_col_grad[2], " !important;
      color: white !important;
    }
    .accordion-tert .accordion-button {
      background-color: ", tert_col_grad[4], ";
    }
    .accordion-tert .accordion-item {
      background-color: ", tert_col_grad[5], " !important;
    }

    .card-tert {
      background-color: ", tert_col_grad[6], " !important;
    }

    /* datatable styling */
    .dataTables_wrapper .dataTable td {
      padding: 2px 2px !important;
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      padding: 2px 2px !important;  /* Adjust padding to make buttons smaller */
    }

    /* Adjust the list spacing*/
    ol.custom-indent {
      padding-left: 15px;
      padding-bottom: 0px;
      margin-top: 0px;
      margin-bottom: 0px;
    }
    .custom-indent .li {
      margin-bottom: 0px;
    }

    /* change header color of ht */
    .handsontable th {
      background-color: ", sec_col_grad[5], " !important;
    }

    /* borders of selected input fields */
    .form-control:focus {
      border-color: ", sec_col, " !important;
    }



    .btn-file {
      background-color: ", tert_col, " !important;
      border-color: ", tert_col, " !important;
      color: white !important;
    }

  ")))




# bs-btn-disabled-bg: #551345;
# .handsontable .autocompleteEditor .htMacScroll {
#   overflow: visible !important;
# }
# .accordion-tert .accordion-header {
#   --bs-accordion-active-bg: ", tert_col_grad[2], " !important;
# }
# ".jstree-default .jstree-clicked {
#   background: #C299B8 !important;
#   background: -webkit-linear-gradient(top, #E0CCDB 0%, #C299B8 100%) !important;
#                                         background: linear-gradient(to bottom, #E0CCDB 0%, #C299B8 100%) !important;
# }
# .jstree-default .jstree-wholerow-clicked {
#   background: #C299B8 !important;
#   background: -webkit-linear-gradient(top, #E0CCDB 0%, #C299B8 100%) !important;
#                                         background: linear-gradient(to bottom, #E0CCDB 0%, #C299B8 100%) !important;
# }"

# .handsontableEditor .ht_master {
#   height: 300px;
#   overflow-y: visible !important;
# }
# .handsontableEditor .ht_clone_top {
#   transform: none !important;
# }
# .htColumnHeaders {
#   overflow: visible !important;
# }

# Define UI --------------------------------------------------------------------
ui <- bslib::page_fluid(

  # preliminaries
  shinyjs::useShinyjs(),  # Include shinyjs
  #reactable.extras::reactable_extras_dependency(),  # Include reactable.extras

  # theme
  theme = theme,

  # add additional style or script vars here
  # tags$head(
  #   tags$style(HTML(
  #     ""))),
  tags$head(
    tags$style(HTML("
      .handsontable td.htInvalid {
        background-color: pink !important;
      }
      .form-control, .selectize-input, .form-control-shiny {
        background-color: white !important;
      }
      .shiny-input-container .radio input:not(:checked)  {
        border-color: ", sec_col, " !important;
        background-color: white !important;
      }
      .shiny-input-container .radio input:checked  {
        border-color: ", sec_col, " !important;
        background-color: ", sec_col, " !important;
      }
      .shiny-input-container .checkbox input:not(:checked)  {
        border-color: ", sec_col, " !important;
        background-color: white !important;
      }
      .shiny-input-container .checkbox input:checked  {
        border-color: ", sec_col, " !important;
        background-color: ", sec_col, " !important;
      }

      /* style for input DT similar to hot*/
        .inputDT thead th {
          background-color: ", sec_col_grad[5], " !important;
        }
      .inputDT tbody td {
        background-color: white;
      }


    .btn-file {
             background-color: grey !important;
             border-color: grey !important;
             }

    ")), # EXAMPLE: greyed out file input button


    # for the tippy tooltip
    tags$script(src = "https://unpkg.com/@popperjs/core@2"),
    tags$script(src = "https://unpkg.com/tippy.js@6"),
    # tags$script(src = "https://kit.fontawesome.com/<you>.js")
  ),


  # MAIN PANEL -----------------------------------------------------------------
  bslib::navset_card_underline( # navset_card_pill, page_navbar?
    id = 'tabs',
    selected = tab_start, # TODO: for testing, set to tab_start
    # navbar_options = navbar_options(collapsible = FALSE),
    # fillable = FALSE,

    # TITLE --------------------------------------------------------------------
    title = "QWA database: Contribute metadata [interactive preview]",

    #HTML('<h4 style="color: #006268; font-weight: bold;">rxs2xcell: Contribute metadata</h4>'),


    # TAB: Start (prefilled metadata) ------------------------------------------
    bslib::nav_panel(
      title = tab_start,
      start_ui('start')
    ),

    # TAB: general (dataset and authors) ---------------------------------------
    bslib::nav_panel(
      title = tab_general,
      dataset_ui('ds', countries_list = countries_list)
    ),

    # TAB: sites ---------------------------------------------------------------
    bslib::nav_panel(
      title = tab_site,
      site_ui('site')
    ),

    # # TAB: trees ---------------------------------------------------------------
    # nav_panel(
    #   title = tab_tree,
    #   tree_ui('tree')
    # ),

    # TAB: summary -------------------------------------------------------------
    bslib::nav_panel(
      title = tab_summary,
      summary_ui('summary')
    )

  ), # end of tabs

  verbatimTextOutput("debug"), # for debugging

  tags$footer(
    style = "
    left: 0;
    bottom: 0;
    width: 100%;
    background: #006268;
    color: white;
    padding: 10px 30px;
    z-index: 1000;
    text-align: left;
    box-shadow: 0 -2px 4px rgba(0,0,0,0.05);
    ",
    tags$img(src = "WSL_Logo_neg.png", height = "30px", style = "vertical-align:middle; margin-right: 10px;"),
    actionLink("contact_link", "Contact", style = "color: white; margin-left: 20px; text-decoration: none;"),
  ),

) # end of ui


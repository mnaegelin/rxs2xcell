# library(shiny)
# library(shinysurveys)
# library(shinyjs)
# library(bslib)
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(reactable)
# library(reactable.extras)
# library(shinyvalidate) # NOTE: need remote mnaegelin/shinyvalidate@remove_rules


# allow to create new options in dropdown select:
# selectizeInput(
#   'e3', '3. Item creation', choices = state.name,
#   options = list(create = TRUE)
# ),

# searchable choose inputs (e.g. for species)
# selectInput('in4', 'Options', c(Choose='', state.name), selectize=TRUE)
# and should also be able to customize labels to show species code
# see https://shiny.posit.co/r/articles/build/selectize/



theme <- NULL
theme <- bs_theme(version = 5, primary = prim_col, secondary = sec_col,
                  info = tert_col, font_scale = 0.8, preset = "zephyr") %>%
  bs_add_rules(HTML(paste0("
    .btn-secondary {
      color: white;
    }
    .btn-info {
      background-color: ", tert_col, ";
    }
    .sidebar {
      background-color: ", prim_col_grad[6], " !important;
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
      background-color:#F5F5F5;
      border:1px solid rgba(0,0,0,0.15);
      border-radius:4px;
      font-family:monospace;
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
      background: ", prim_col, " !important;
    }
    .accordion .accordion-header {
      --bs-accordion-active-bg: ", prim_col_grad[2], " !important;
    }
    .accordion-button {
      background-color: ", prim_col_grad[5], ";
    }
    .accordion-tert .accordion-button {
      background-color: ", tert_col_grad[5], ";
    }
    .accordion-tert .accordion-button:not(.collapsed) {
      background-color: ", tert_col_grad[2], " !important;
    }
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
    .handsontable th {
      background-color: ", sec_col_grad[5], " !important;
    }
  ")))

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
ui <- page_fluid(

  # preliminaries
  shinyjs::useShinyjs(),  # Include shinyjs
  #reactable.extras::reactable_extras_dependency(),  # Include reactable.extras

  # theme
  theme = theme,

  # additional style vars here (or in theme)
  # tags$head(
  #   tags$style(HTML(
  #     ""))),


  # MAIN PANEL -----------------------------------------------------------------
  navset_card_underline( # navset_card_pill, page_navbar?
    id = 'tabs',
    selected = tab_site, # TODO: for testing, set to tab_start
    # navbar_options = navbar_options(collapsible = FALSE),
    # fillable = FALSE,

    # TITLE --------------------------------------------------------------------
    title = "rxs2xcell: Contribute metadata",
    #HTML('<h4 style="color: #006268; font-weight: bold;">rxs2xcell: Contribute metadata</h4>'),


    # TAB: Start (prefilled metadata) ------------------------------------------
    nav_panel(
      title = tab_start,
      start_ui('start')
    ),

    # TAB: general (dataset and authors) ---------------------------------------
    nav_panel(
      title = tab_general,
      dataset_ui('ds')
    ),

    # TAB: sites ---------------------------------------
    nav_panel(
      title = tab_site,
      site_ui('site')
    )

  ) # end of tabs

) # end of ui


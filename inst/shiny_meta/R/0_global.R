# THEME ------
# define the names of the tabs
tab_start <- '1: Start'
tab_general <- '2: General'
tab_site <- '3: Sites'
tab_tree <- '4: Trees'
tab_summary <- '5: Summary'

# define color range
prim_col <- "#006268"
sec_col <- "#69004F"
tert_col <- "#00206E"
prim_col_grad <- c("#338585", "#66A3A3", "#99C2C2", "#CCE0E0", "#E6F0F0", "#F2F7F7")
sec_col_grad <- c("#853270", "#A36794", "#C299B8", "#E0CCDB", "#F0E6ED", "#F7F2F6")
tert_col_grad <- c("#324a85", "#6778a3", "#99a5c2", "#ccd2e0", "#e6e9f0", "#f2f4f7")

# reactable_theme <- reactable::reactableTheme(
#   backgroundColor = "#dfe7e8"
# )


# UI ELEMENTS ------
# next_button <- function(btn_id) {
#   card(
#     class="border border-0",
#     card_body(
#       fillable = FALSE,
#       actionButton(btn_id, 'Next', icon = icon('angle-double-right')))
#   )}
#
#
# author_input <- function(author_nr){
#   tagList(
#     span(paste0('Author ', author_nr)), #,style="font-weight:bold; color: #006268"
#     layout_columns(
#       textInput(paste0('autname_', author_nr), NULL, "", placeholder = "Last name, first name"),
#       textInput(paste0('autmail_', author_nr), NULL, "", placeholder = "name@example.com"),
#       textInput(paste0('autror_', author_nr), NULL, "", placeholder = "https://ror.org/05a28rw58"),
#       textInput(paste0('autaff_', author_nr), NULL, "", placeholder = "University of ABC")
#     )
#   )
# }

# UTILS ------
get_country_codes <- function(){
  file_path <- system.file("extdata", "country_ISO3166-1-alpha-2_20241205.csv", package = "rxs2xcell")
  iso_countries <- read.csv(file_path, stringsAsFactors = FALSE, na.strings=c(""))
  country_list <- stats::setNames(iso_countries$Code,
                                  paste(iso_countries$Name, "  (",
                                        iso_countries$Code, ")", sep = ""))
  return(country_list)
}

max_char_limit <- function(value, limit) {
  if (nchar(value) > limit) paste0("Input exceeds ", limit, " characters.")
}


# start tab, server, shinyTree ----
# helper to get the list of all selected images in the shinyTree
get_selected_imgs <- function(tree, selected = c()) { #ancestry = "",
  for (leaf in names(tree)){
    if (is.null(names(tree[[leaf]]))) {
      a <- attr(tree[[leaf]], 'stselected', TRUE)
      if (!is.null(a) && a == TRUE) {
        selected <- c(selected, leaf) # paste0(ancestry, "/", leaf) # since image_codes are unique, we don't need the path
      }
    } else {
      selected <- get_selected_imgs(tree[[leaf]], selected) #paste0(ancestry, "/", leaf)
    }
  }
  return(selected)
}

# source('input_tables_utils.R')
# source('ht_render_utils.R')
#
#
#
#
# # LOAD MODULES -----
# source('start_ui.R')
# source('start_server.R')
# source('dataset_ui.R')
# source('dataset_server.R')
# source('site_ui.R')
# source('site_server.R')
# source('tree_ui.R')
# source('tree_server.R')
# source('summary_ui.R')
# source('summary_server.R')


# # Aggregating df_rings to get the required data for the editable datatable
# df_edit <- QWA_data$rings %>%
#   dplyr::group_by(tree_code) %>%
#   dplyr::summarise(n_samples = dplyr::n()) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(species = NA, dbh = NA, age = NA, height = NA, origin = NA)
#
# callback <- c(
#   "var tbl = $(table.table().node());",
#   "var id = tbl.closest('.datatables').attr('id');",
#   "table.on('autoFill', function(e, datatable, cells){",
#   "  var out = Array(cells.length);",
#   "  for(var i=0; i<cells.length; ++i){",
#   "    var c = cells[i][0];",
#   "    var value = c.set === null ? '' : c.set;", # null causes problem in R
#   "    out[i] = {row: c.index.row+1, col: c.index.column, value: value};",
#   # if you want a color for the autofilled cells:
#   "    $(table.cell(c.index.row, c.index.column).node())",
#   "      .css('background-color', 'yellow');",
#   "  }",
#   "  Shiny.setInputValue(id + '_cells_filled:DT.cellInfo', out);",
#   "  table.rows().invalidate();", # this updates the column type
#   "});"
# )

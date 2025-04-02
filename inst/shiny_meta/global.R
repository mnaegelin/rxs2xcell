next_button <- function(btn_id) {
  card(
    class="border border-0",
    card_body(
      fillable = FALSE,
      actionButton(btn_id, 'Next', icon = icon('angle-double-right')))
  )}

get_country_codes <- function(){
  file_path <- system.file("extdata", "country_ISO3166-1-alpha-2_20241205.csv", package = "rxs2xcell")
  iso_countries <- read.csv(file_path, stringsAsFactors = FALSE, na.strings=c(""))
  country_list <- setNames(iso_countries$Code,
                           paste(iso_countries$Name, "  (",
                                 iso_countries$Code, ")", sep = ""))
  return(country_list)
}

# move to global
author_input <- function(author_nr){
  tagList(
    span(paste0('Author ', author_nr)), #,style="font-weight:bold; color: #006268"
    layout_columns(
      textInput(paste0('autname_', author_nr), NULL, "", placeholder = "Last name, first name"),
      textInput(paste0('autmail_', author_nr), NULL, "", placeholder = "name@example.com"),
      textInput(paste0('autror_', author_nr), NULL, "", placeholder = "https://ror.org/05a28rw58"),
      textInput(paste0('autaff_', author_nr), NULL, "", placeholder = "University of ABC")
    )
  )
}

max_char_limit <- function(value, limit) {
  if (nchar(value) > limit) paste0("Input exceeds ", limit, " characters.")
}



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


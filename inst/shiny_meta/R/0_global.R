# THEME ------
# define the names of the tabs
tab_start <- '1: Start'
tab_general <- '2: General'
tab_site <- '3: Samples'
#tab_tree <- '4: Trees'
tab_summary <- '4: Summary'

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
## GENERAL FUNCTIONS ----
get_country_codes <- function(){
  file_path <- system.file("extdata", "country_ISO3166-1-alpha-2_20241205.csv", package = "rxs2xcell")
  iso_countries <- read.csv(file_path, stringsAsFactors = FALSE, na.strings=c(""))
  country_list <- stats::setNames(iso_countries$Code,
                                  paste(iso_countries$Name, "  (",
                                        iso_countries$Code, ")", sep = ""))
  return(country_list)
}

# source the var, we need it everywhere
countries_list <- get_country_codes()

get_species_info <- function(){
  file_path <- system.file("extdata", "species_ITRDB_20250423.csv", package = "rxs2xcell")
  df_species <- vroom::vroom(file_path, show_col_types = FALSE)
  return(df_species)
}

species_info <- get_species_info()



max_char_limit <- function(value, limit) {
  if (nchar(value) > limit) paste0("Input exceeds ", limit, " characters.")
}



#' A column of delete buttons for each row in the data frame for the first column
#'
#' @param df data frame
#' @param id id prefix to add to each actionButton. The buttons will be id'd as id_INDEX.
#' @return A DT::datatable with escaping turned off that has the delete buttons in the first column and \code{df} in the other
deleteButtonColumn <- function(df, id, ns, ...) {
  # function to create one action button as string
  f <- function(i) {
    # https://shiny.rstudio.com/articles/communicating-with-js.html
    as.character(actionButton(
      ns(paste(id, i, sep="_")), label = NULL,
      icon = icon('trash', lib = "glyphicon"),
      style='padding:7px; font-size:80%',
      onclick = sprintf('Shiny.setInputValue("%s", this.id, {priority: "event"})', ns('deletePressed'))))
  }

  deleteCol <- unlist(lapply(seq_len(nrow(df)), f))

  # Return a data table
  DT::datatable(cbind(delete = deleteCol, df),
                # Need to disable escaping for html as string to work
                escape = FALSE,
                style = 'default',
                selection = "none",
                rownames = FALSE,
                colnames = c(" ", colnames(df)),
                options = list(
                  dom = 't',
                  #odering = FALSE
                  # Disable sorting for the delete column
                  columnDefs = list(list(targets = c(1,2,3), sortable = FALSE))
                ))
}

parseDeleteEvent <- function(idstr) {
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}


## START SERVER -----
# helper function to load input data df_meta
load_meta_env <- function(){
  # check if we have df_meta in the environment
  if (exists('df_meta')){
    return(df_meta)
  } else {
    showModal(
      modalDialog(
        title = "Warning",
        "No df_meta available in the environment. Please provide a metadata file.",
        easyClose = TRUE
      )
    )
    return(NULL)
  }
}

load_meta_csv <- function(datapath){
  # TODO: try catch error
  df <- read.csv(datapath, stringsAsFactors = FALSE)
  # TODO: improve validation, ROXAS AI
  if (setdiff(c(cols_structure,cols_img,cols_settings,cols_paths, cols_other),
              colnames(df)) %>% length() > 0) {
    showModal(modalDialog(
      title = "Error",
      span("Required columns missing in data. Please provide another input data source."),
      easyClose = TRUE
    ))
    return(NULL)
  } else {
    return(df)
  }
}

load_meta_json <- function(datapath){
  # TODO
}

load_whole_json <- function(datapath){
  # TODO
}

load_input_data <- function(input_src, file_input = NULL) {
  switch(
    input_src,
    df_meta_env = {
      df <- load_meta_env()
      source <- ifelse(is.null(df), "Please provide and load input source",
                       "df_meta from R environment")
    },
    df_meta_csv = {
      df <- load_meta_csv(file_input$datapath)
      source <- ifelse(is.null(df), "Please provide and load input source",
                       paste0("read from file ", file_input$name))
    },
    df_meta_json = {
      df <- load_meta_json(file_input$datapath)
      source <- ifelse(is.null(df), "Please provide and load input source",
                       paste0("read from file ", file_input$name))
    }
  )
  list(df = df, source = source)
}


# load_input_data <- function(input_src, file_input, input_meta) {
#
#   # from environment
#   if (input_src == 'df_meta_env') {
#     if (exists('df_meta')) {
#       input_meta$df <- df_meta
#       input_meta$source <- 'df_meta from R environment'
#     } else {
#       showModal(modalDialog(
#         title = "Error",
#         span("No", code('df_meta'), "available in the R environment. Please provide another input data source."),
#         easyClose = TRUE
#       ))
#       input_meta$df <- NULL
#       input_meta$source <- 'Please provide and load input source'
#     }
#
#     # from csv
#   } else if (input_src == 'df_meta_csv') {
#     df <- read.csv(file_input$datapath, stringsAsFactors = FALSE)
#     # TODO: improve validation, ROXAS AI
#     if (setdiff(c(cols_structure,cols_img,cols_settings,cols_paths, cols_other),
#                 colnames(df)) %>% length() > 0) {
#       showModal(modalDialog(
#         title = "Error",
#         span("Required columns missing in data. Please provide another input data source."),
#         easyClose = TRUE
#       ))
#       input_meta$df <- NULL
#       input_meta$source <- 'Please provide and load input source'
#     } else {
#       input_meta$df <- df
#       input_meta$source <- paste0('read from file ', file_input$name)
#     }
#   }
#
#   # from json
#   # TODO: add read from json, iincluding distribution to other tabs!
#   # else if (input$input_src == 'df_meta_json') {
#   #   req(input$file_input)
#   #   input_meta$meta_json <- jsonlite::fromJSON(input$file_input$datapath)
#   #   input_meta$df <- input_meta$meta_json$image_table
#   #   input_meta$source <- paste0('read from file ', input$file_input$name)
#   # }
#
#   return(input_meta) # Return the updated input_meta
# }

# helper to get the list of all selected images in the shinyTree
get_selected_imgs <- function(tree, selected = c()) { #ancestry = "",
  for (leaf in names(tree)){
    if (is.null(names(tree[[leaf]]))) {
      # if no more children, get stselected attribute
      a <- attr(tree[[leaf]], 'stselected', TRUE)
      if (!is.null(a) && a == TRUE) {
        selected <- c(selected,  attr(tree[[leaf]], 'image_code', TRUE)) # paste0(ancestry, "/", leaf) # since image_codes are unique, we don't need the path
        #img_codes <- c(img_codes, attr(tree[[leaf]], 'image_code', TRUE))
      }
    } else {
      selected <- get_selected_imgs(tree[[leaf]], selected) #paste0(ancestry, "/", leaf)
    }
  }
  return(selected)
}


## DATASET SERVER -----






# old -----
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

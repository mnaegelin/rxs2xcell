# THEME ------
# define the names of the tabs
tab_start <- '1: Start'
tab_general <- '2: General'
tab_site <- '3: Samples'
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
countries_sf <- rnaturalearth::ne_load(
  scale=50, type='countries',
  destdir = system.file("extdata/ne_countries_data", package="rxs2xcell"))

country_from_coords <- function(lng, lat, countries_sf = NULL){
  point <- sf::st_as_sf(data.frame(lon = lng, lat = lat), coords = c("lon", "lat"), crs = 4326)
  # if no countries are provided, load them from the package
  if (is.null(countries_sf)) {
    dir_path <- system.file("extdata/ne_countries_data", package="rxs2xcell")
    countries_sf <- rnaturalearth::ne_load(scale=50, type='countries', destdir = dir_path)
  }
  country <- sf::st_join(point, countries_sf, join = sf::st_within)
  return(country$ISO_A2_EH)
}

get_species_info <- function(){
  file_path <- system.file("extdata", "species_ITRDB_20250423.csv", package = "rxs2xcell")
  df_species <- vroom::vroom(file_path, show_col_types = FALSE)
  return(df_species)
}

species_info <- get_species_info()



max_char_limit <- function(value, limit) {
  if (nchar(value) > limit) paste0("Input exceeds ", limit, " characters.")
}

# need only the names of fields with validation checks,
# can take table column names from tbl_configs
input_field_names <- c(
  ds_data = 'Dataset',
  ds_name = 'Dataset name',
  ds_desc = 'Dataset description',
  author_data = 'Authors',
  funding_data = 'Funding',
  site_data = 'Sites',
  tree_data = 'Trees',
  wp_data = 'Woodpieces',
  slide_data = 'Slides')


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
                editable = list(target = 'cell', disable = list(columns = c(0,1))),
                rownames = TRUE,
                colnames = c(" ", colnames(df)),
                options = list(
                  dom = 't',
                  #odering = FALSE
                  # Disable sorting for the delete column
                  columnDefs = list(list(targets = c(1,2,3,4), sortable = FALSE))),
                class = 'inputDT')  %>%
      DT::formatStyle( # change bg color for the rowname and delete button cols
        columns = c(0,1),
        backgroundColor = sec_col_grad[5]
      )
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
  meta_json <- jsonlite::fromJSON(datapath, flatten = TRUE)
  return(meta_json$df_meta)
}

load_whole_json <- function(datapath){
  meta_json <- jsonlite::fromJSON(datapath, flatten = TRUE)
  meta_json$df_meta <- NULL
  return(meta_json)
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
ror_api_request <- function(search_string, country_code){
  search_url <- sprintf(
    'https://api.ror.org/v2/organizations?query=%s&filter=country.country_code:%s',
    URLencode(search_string), country_code)
  ror_res <- httr::GET(search_url, httr::timeout(5))

  if (httr::status_code(ror_res) == 200) {
    ror_data <- jsonlite::fromJSON(rawToChar(ror_res$content))

    if (ror_data$number_of_results > 0) {
      # get the names (assuming that there is always exactly one ror_display name)
      res_names <- ror_data$items$names %>%
        dplyr::bind_rows() %>%
        dplyr::filter(grepl('ror_display', types)) %>%
        dplyr::rename(Name = value) %>%
        dplyr::select(Name)

      # get the locations
      res_locs <- ror_data$items$locations %>%
        dplyr::bind_rows() %>%
        dplyr::pull(geonames_details) %>%
        tidyr::unite(col = 'Location', name, country_name, sep = ', ', remove = FALSE) %>%
        dplyr::rename(city = name) %>%
        dplyr::select(Location, country_code, city)

      res_df <- cbind(res_names, res_locs)

      # get the ror ids and corresponding hyperlinks
      res_df <- res_df %>%
        dplyr::mutate(
          RORID = gsub('https://ror.org/', '', ror_data$items$id),
          Link = paste0("<a href='",ror_data$items$id,"' target='_blank'>",ror_data$items$id,"</a>")
        )
      return(res_df)

    } else {
      showNotification("No ROR results found. Try again.", type = "message")
      return(NULL)
    }

  } else {
    showNotification("ROR API request failed. Try again.", type = "error")
    return(NULL)
  }
}



orcid_api_request <- function(search_string = NULL, last_name = NULL, first_name = NULL){
  # EITHER query by search string
  if (!is.null(search_string)) {
    # query either orcid or by names, depending on the format of the search_string
    if (grepl("^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$", search_string)) {
      # query by orcid
      query <- sprintf('?q=orcid:%s',search_string)
    } else {
      # query by names
      search_terms <- URLencode(gsub(" ", "+AND+", search_string))
      query <- sprintf('?q=given-and-family-names:(%s)+OR+other-names:(%s)',
                       search_terms, search_terms)
    }

    # OR query by first and last names explicitly
  } else if (!(is.null(last_name) || last_name == "" || is.na(last_name)) ||
             !(is.null(first_name) || first_name == "" || is.na(first_name))) {

    query_ln <- ifelse(!(is.null(last_name) || last_name == "" || is.na(last_name)),
                       sprintf('(family-name:(%s))', URLencode(gsub(" ", "+AND+", last_name))),
                       "")
    query_fn <- ifelse(!(is.null(first_name) || first_name == "" || is.na(first_name)),
                       sprintf('(given-names:(%s))', URLencode(gsub(" ", "+AND+", first_name))),
                       "")
    query <- paste0('?q=', query_ln, ifelse(nchar(query_ln)>0&&nchar(query_fn)>0, '+AND+', ""), query_fn)

  } else {
    # no valid search terms provided, abort
    # showNotification("Could not generate query. Provide either search_string or last_name, first_name arguments.", type = "error")
    return(NULL)
  }

  search_url <- paste0(
    'https://pub.orcid.org/v3.0/csv-search/', query,
    '&fl=family-name,given-names,email,orcid,current-institution-affiliation-name,other-names', # the fields we want
    '&rows=50') # limit to 50 results

  # GET request
  orcid_res <- httr::GET(search_url, httr::timeout(5))

  if (httr::status_code(orcid_res) == 200) {
    orcid_data <- read.table(text = rawToChar(orcid_res$content),
                             sep =",", header = TRUE,
                             stringsAsFactors = FALSE, allowEscapes = TRUE)

    if (nrow(orcid_data) > 0) {
      orcid_data <- orcid_data %>%
        dplyr::rename(
          last_name = 'family.name',
          first_name = 'given.names',
          orcid_id = 'orcid',
          org_name = 'current.institution.affiliation.name',
          other_names = 'other.names') %>%
        # only use the first entry for email and affiliation
        tidyr::separate(email, into = c("email"), sep = ",(?!\\s)", extra = "drop") %>%
        tidyr::separate(org_name, into = c("org_name"), sep = ",(?!\\s)", extra = "drop") %>%
        dplyr::mutate(
          # create orcid hyperlinks
          orcid = paste0("<a href='https://orcid.org/", orcid_id, "' target='_blank'>",orcid_id,"</a>"))

      return(orcid_data)

    } else {
      showNotification("No ORCID results found. Try again.", type = "message")
      return(NULL)
    }

  } else {
    showNotification("ORCID API request failed. Try again.", type = "error")
    return(NULL)
  }

}





# old -----

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

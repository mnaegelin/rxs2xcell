write_data_to_db <- function(complete_metadata,
                             QWA_data,
                             dbcon, schema){

  # PERSONS

  person_data <- data.frame(
    first_name = c("John", "Janice", "Alice", "John", "Steven"),
    last_name = c("Dixie", "Beaty", "Johnson", "Doe", "Muller"),
    email = c("john.dixie@example.com", "janice.beaty@example.com", "alice.johnson2@example.com","john.doe@example.com", "steve.mueller@example.com"),
    stringsAsFactors = FALSE
  )

  person_data <- complete_metadata$author_table[c('lastname', 'firstname', 'email', 'orcid')]
  person_data <- person_data %>%
    dplyr::rename(
      last_name = lastname,
      first_name = firstname
    )

  person_table <- DBI::dbReadTable(conn = dbcon, name = c(schema, 'person'))

  person_new <- person_data %>%
    dplyr::anti_join(person_table, by = c("first_name", "last_name")) %>%
    dplyr::filter(!(email %in% person_table$email))

  existing_name <- person_data %>%
    dplyr::semi_join(person_table[c('first_name', 'last_name')],
                     by = c("first_name", "last_name")) %>%
    dplyr::mutate(in_db = 'name')

  existing_email <- person_data %>%
    dplyr::filter(email %in% person_table$email) %>%
    dplyr::mutate(in_db = 'email')

  person_old <- rbind(existing_name, existing_email) %>%
    dplyr::group_by(first_name, last_name, email) %>%
    dplyr::summarise(in_db = paste(in_db, collapse = ", "), .groups = "drop")

  if (nrow(person_old) > 0){
    message("The following authors or emails already exist in the database.
             Only new authors will be added.
             Edits to existing authors' data should be handled manually or in DB directly.")
    print(person_old)
  }

  if (nrow(person_new) > 0){
    tryCatch(
      {
        DBI::dbWriteTable(
          conn = dbcon,
          name = c(schema, 'person'),
          value = person_new,
          append = TRUE, overwrite = FALSE, row.names = FALSE)
        message(sprintf("%s persons added to the DB, table `person`.", nrow(person_new)))
      },
      error = function(e) {
        message("Error while writing to DB, table person: ", e$message)
      }
    )
  } else {
    message("No new authors to be added to the database.")
  }




  pers_data <- complete_metadata$author_table[c('lastname', 'firstname', 'email', 'orcid')]
  colnames(pers_data) <- c('last_name', 'first_name', 'email', 'orcid')

  # append person data
  DBI::dbWriteTable(
    conn = dbcon,
    name = c(schema, 'person'),
    value = pers_data,
    append = TRUE, overwrite = FALSE, row.names = FALSE)


  # DATASET
  now <- Sys.time()

  ds_data <- lapply(complete_metadata$ds_info, function(x) {
    if (length(x) == 0) {
      return(rep(NA, max(sapply(complete_metadata$ds_info, length))))
    } else {
      return(x)
    }
  })

  # Convert the list to a data frame
  # TODO: FIX
  ds_data <- as.data.frame(ds_data)
  ds_data <- ds_data %>%
    dplyr::rename(
      description = ds_desc) %>% dplyr::select(
        ds_name, description
      ) %>% dplyr::mutate(
        contact_person_id = 1, # FK: person
        entry_created_at = now,
        last_modified_at = now,
        locked = FALSE,
        access_id = 1 # FK: data_access
      )
  ds_data


  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'dataset'),
                    value = ds_data,
                    row.names = FALSE, overwrite = FALSE, append = TRUE)

  # ORGANIZATIONS
  # AFFILIATION
  # AUTHORSHIP
  # FUNDING
  # (LICENCSE, RELATED)


  # SITE
  site_data <- complete_metadata$site
  site_data <- site_data %>%
    dplyr::rename(
      site_label = sitecode,
      site_name = sitename,
      country_code = country,
      description = sitedesc
    )
  site_data$elevation = c(100,200)

  cols <- DBI::dbListFields(conn = dbcon, name = c(schema, 'site'))
  site_data <- site_data %>% dplyr::select(dplyr::any_of(cols))
  site_data

  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'site'),
                    value = site_data,
                    row.names = FALSE, overwrite = FALSE, append = TRUE)

  # TREE
  tree_data <- complete_metadata$tree
  cols <- DBI::dbListFields(conn = dbcon, name = c(schema, 'tree'))
  tree_data <- tree_data %>%
    dplyr::rename(
      tree_label = treecode,
      species_code = speciescode,
      site_label = sitecode
    )

  where_clause <- paste0("'", unique(tree_data$site_label), "'", collapse = ", ")
  # Complete SQL query
  query <- paste0("SELECT site_id, site_label FROM ", schema, ".site WHERE site_label IN (", where_clause, ")")
  result <- DBI::dbGetQuery(dbcon, query)

  tree_data <- tree_data %>% dplyr::left_join(result, by = 'site_label')
  tree_data <- tree_data %>% dplyr::select(dplyr::any_of(cols))

  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'tree'),
                    value = tree_data,
                    row.names = FALSE, overwrite = FALSE, append = TRUE)



  # WOODPIECE
  wp_data <- complete_metadata$wp_table

  wp_data <- wp_data %>%
    dplyr::rename(
      wp_label = wpcode,
      tree_label = treecode
    )


  cols <- DBI::dbListFields(conn = dbcon, name = c(schema, 'woodpiece'))
  where_clause <- paste0("'", unique(wp_data$tree_label), "'", collapse = ", ")
  # Complete SQL query
  query <- paste0("SELECT tree_id, tree_label FROM ", schema, ".tree WHERE tree_label IN (", where_clause, ")")
  result <- DBI::dbGetQuery(dbcon, query)

  wp_data <- wp_data %>% dplyr::left_join(result, by = 'tree_label')
  wp_data <- wp_data %>% dplyr::select(dplyr::any_of(cols))

  wp_data$collected_at <- now
  wp_data$organ <- 'stem'

  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'woodpiece'),
                    value = wp_data,
                    row.names = FALSE, overwrite = FALSE, append = TRUE)


  # SLIDE
  slide_data <- complete_metadata$slide_table

  slide_data <- slide_data %>%
    dplyr::rename(
      slide_label = slidecode,
      wp_label = wpcode
    )

  cols <- DBI::dbListFields(conn = dbcon, name = c(schema, 'slide'))
  where_clause <- paste0("'", unique(slide_data$wp_label), "'", collapse = ", ")
  # Complete SQL query
  query <- paste0("SELECT wp_id, wp_label FROM ", schema, ".woodpiece WHERE wp_label IN (", where_clause, ")")
  result <- DBI::dbGetQuery(dbcon, query)

  slide_data <- slide_data %>% dplyr::left_join(result, by = 'wp_label')
  slide_data <- slide_data %>% dplyr::select(dplyr::any_of(cols))

  slide_data$created_at <- now

  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'slide'),
                    value = slide_data,
                    row.names = FALSE, overwrite = FALSE, append = TRUE)

  # IMAGE
  image_data <- complete_metadata$image_table

  image_data <- image_data %>%
    dplyr::rename(
      slide_label = slide_code,
      img_label = image_code
    )

  cols <- DBI::dbListFields(conn = dbcon, name = c(schema, 'image'))
  where_clause <- paste0("'", unique(image_data$slide_label), "'", collapse = ", ")
  # Complete SQL query
  query <- paste0("SELECT slide_id, slide_label FROM ", schema, ".slide WHERE slide_label IN (", where_clause, ")")
  result <- DBI::dbGetQuery(dbcon, query)

  image_data <- image_data %>% dplyr::left_join(result, by = 'slide_label')
  image_data <- image_data %>% dplyr::select(dplyr::any_of(cols))

  image_data$created_at <- now

  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'image'),
                    value = image_data,
                    row.names = FALSE, overwrite = FALSE, append = TRUE)

  # IMG ANALYSIS

  where_clause <- paste0("'", unique(image_data$img_label), "'", collapse = ", ")
  # Complete SQL query
  query <- paste0("SELECT img_id, img_label FROM ", schema, ".image WHERE img_label IN (", where_clause, ")")
  result <- DBI::dbGetQuery(dbcon, query)


  img_analysis_data <- data.frame(
    img_id = result$img_id, # LOOKUP
    analysis_nr = 1,
    ds_id = 3, # FK: LOOKUP
    created_at = now,
    software = 'ROXAS',
    sw_version = 'vtest' # FROM DATA
  )

  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'image_analysis'),
                    value = img_analysis_data,
                    row.names = FALSE, overwrite = FALSE, append = TRUE)


  # RING
  ring_data <- QWA_data$rings

  ring_key <- result
  ring_key$analysis_nr <- 1

  ring_data <- ring_data %>% dplyr::select(image_code, year) %>%
    dplyr::left_join(ring_key, by = c('image_code' = 'img_label'))


  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'ring'),
                    value = ring_data %>% dplyr::select(-image_code),
                    row.names = FALSE, overwrite = FALSE, append = TRUE)


  ring_attr <- DBI::dbReadTable(conn = dbcon, name = c(schema, 'ring_attribute'))
  ring_measurements <- QWA_data$rings %>% dplyr::select(image_code, year, mrw)

  ring_measurements <- ring_measurements %>%
    dplyr::left_join(ring_key, by = c('image_code' = 'img_label')) %>%
    tidyr::pivot_longer(
      cols = c('mrw'),
      names_to = 'ring_attr_name',
      values_to = 'val'
    )

  ring_measurements <- ring_measurements %>%
    dplyr::mutate(ring_attr_name = toupper(ring_attr_name)) %>%
    dplyr::left_join(ring_attr, by = c('ring_attr_name' = 'attr_label'))

  ring_measurements <- ring_measurements %>%
    dplyr::select(-ring_attr_name, -unit, -description, -encoding) %>% dplyr::filter(!is.na(val))

  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'ring_data'),
                    value = ring_measurements %>% dplyr::select(-image_code, -attr_name),
                    row.names = FALSE, overwrite = FALSE, append = TRUE)


  # CELL
  cell_data <- QWA_data$cells %>% dplyr::select(image_code, year, xpix, ypix) %>%
    dplyr::left_join(ring_key, by = c('image_code' = 'img_label'))


  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'cell'),
                    value = cell_data %>% dplyr::select(-image_code),
                    row.names = FALSE, overwrite = FALSE, append = TRUE)


  cell_attr <- DBI::dbReadTable(conn = dbcon, name = c(schema, 'cell_attribute'))
  cell_measurements <- QWA_data$cells %>% dplyr::select(image_code, year, xpix, ypix, raddistr, la, cwtpi)

  cell_measurements <- cell_measurements %>%
    dplyr::left_join(ring_key, by = c('image_code' = 'img_label')) %>%
    tidyr::pivot_longer(
      cols = c('raddistr', 'la', 'cwtpi'),
      names_to = 'cell_attr_name',
      values_to = 'val'
    )

  cell_measurements <- cell_measurements %>%
    dplyr::mutate(cell_attr_name = toupper(cell_attr_name)) %>%
    dplyr::left_join(cell_attr, by = c('cell_attr_name' = 'attr_label')) %>%
    dplyr::select(-attr_name, -unit, -description, -encoding) %>% dplyr::filter(!is.na(val))

  DBI::dbWriteTable(conn = dbcon,
                    name = c(schema, 'cell_data'),
                    value = cell_measurements %>% dplyr::select(-image_code, -cell_attr_name, -year),
                    row.names = FALSE, overwrite = FALSE, append = TRUE)

  # DBI::dbDisconnect(dbcon)
}

# tbl_name <- 'person'
# DBI::dbListFields(conn = dbcon, name = c(schema, tbl_name))
# DBI::dbReadTable(conn = dbcon, name = c(schema, tbl_name))






# saxor:
# xcldb_get_constrains <- function(dbcon, sch =  schema_id){
#
#   suppressWarnings(
#     r <- DBI::dbSendQuery(dbcon, "select conrelid::regclass AS table, pg_get_constraintdef(c.oid) AS constrains from   pg_constraint c where  contype in ('p','u') order by contype")
#   )
#
#   dbConstrains <- DBI::dbFetch(r)
#   DBI::dbClearResult(r)
#
#   suppressWarnings(dbConstrains %>%
#                      tibble::as_tibble() %>%
#                      #dplyr::filter( substr(constrains, 1, 6) %in% 'UNIQUE') %>%
#                      dplyr::mutate( constrains = gsub('UNIQUE [(]|[)]|["]|PRIMARY KEY [(]', '', constrains)) %>%
#                      tidyr::separate(constrains, letters, sep = ',') %>%
#                      tidyr::gather(lett, constrains, -table) %>%
#                      dplyr::mutate(constrains = trimws(constrains)) %>%
#                      dplyr::filter(!is.na(constrains), !constrains %in% 'id') %>%
#                      dplyr::select(-lett) %>%
#                      tidyr::separate(table, c('schema', 'table'), sep = '\\.') %>%
#                      dplyr::filter(schema %in% sch) %>%
#                      dplyr::arrange(table)) ->
#     constr.df
#
#   return(constr.df)
# }

# read table
# xcldb_tbl_x <- function(t = 'site', s = 'v1') {
#   dplyr::tbl(dbcon, dplyr::sql( paste0( "select * from ", s, '.', t) ) )
# }

#' Function first check if some id's need to be renamed: check if there are data
#' already in DB and keep id's for that part, and add the new ones
# xcldb_check_append_db

# xcldb_append_data <- function(d_table, table.name, sch =  schema_id){
#
#   if( nrow(d_table) > 0 ){
#
#     # rearange the columns
#     colls <- xcldb_tbl_x(table.name, sch) %>% colnames()
#
#     d_table <- dplyr::select(d_table, colls)
#
#     print(paste0('Adding ', nrow(d_table), ' entries to ', table.name, ' table.'))
#     DBI::dbWriteTable(conn = dbcon, name = c(sch, table.name), value = d_table, overwrite = FALSE, append = TRUE, row.names=FALSE)
#
#   }else{
#
#     print(paste0('No data to be added to ',  table.name, ' table.'))
#
#   }
#
# }

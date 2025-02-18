

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

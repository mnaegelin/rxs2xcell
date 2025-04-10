# Function to align df_authors to the structure of author_tbl_str
align_to_structure <- function(target_structure, source_data) {
  # Get the column names of the target and source
  target_cols <- names(target_structure)
  source_cols <- names(source_data)
  missing_cols <- c()

  # Initialize a new dataframe with the target structure
  result <- target_structure[rep(1, nrow(source_data)), ]
  rownames(result) <- NULL  # Reset row names

  # For each column in the target structure
  for (col in target_cols) {
    if (col %in% source_cols) {
      # If the column exists in the source, copy and convert its type
      result[[col]] <- as(source_data[[col]], class(target_structure[[col]]))
    } else {
      missing_cols <- c(missing_cols, col)
      # If the column is missing in the source, fill with default values
      result[[col]] <- target_structure[[col]]
    }
  }

  return(list(data = result, missing_cols = missing_cols))
}

# validation check functions
# cf. the corresponding JS renderers in ht_render_utils.R
validate_char_column <- function(column, col_config) {
  # Default values for validation rules
  check_required <- ifelse(is.null(col_config$required), FALSE, col_config$required)
  minl <- ifelse(is.null(col_config$min_length), -1, col_config$min_length)
  maxl <- ifelse(is.null(col_config$max_length), 10000, col_config$max_length)
  check_regex <- !is.null(col_config$regex_pattern)
  regp <- ifelse(is.null(col_config$regex_pattern), "", col_config$regex_pattern)
  check_unique <- ifelse(is.null(col_config$unique), FALSE, col_config$unique)

  # Initialize the result list
  #validation_results <- vector("list", length(column))
  validation_results <- c()

  if (check_required && any(is.na(column) | column == "")) {
    validation_results <- c(validation_results, "missing values")
  }

  column_nonmissing <- column[!(is.na(column) | column == "")]

  if (any(nchar(column_nonmissing) < minl)) {
    validation_results <- c(validation_results, "invalid lengths")
  } else if (any(nchar(column_nonmissing) > maxl)) {
    validation_results <- c(validation_results, "invalid lengths")
  }

  if (check_regex && any(!grepl(regp, column_nonmissing))) {
    validation_results <- c(validation_results, "invalid formats")
  }

  if (check_unique && any(duplicated(column_nonmissing))) {
    validation_results <- c(validation_results, "non-unique values")
  }

  return(validation_results)
}

validate_num_column <- function(column, col_config) {
  check_required <- ifelse(is.null(col_config$required), FALSE, col_config$required)
  check_min_val <- ifelse(is.null(col_config$min_val), FALSE, TRUE)
  minv <- col_config$min_val
  check_max_val <- ifelse(is.null(col_config$max_val), FALSE, TRUE)
  maxv <- col_config$max_val

  validation_results <- c()
  column_numeric <- suppressWarnings(as.numeric(column))

  if (check_required && any(is.na(column_numeric))) {
    validation_results <- c(validation_results, "missing values")
  }

  if (check_min_val && any(column_numeric < minv, na.rm = TRUE)) {
    validation_results <- c(validation_results, "out of range values")
  } else if (check_max_val && any(column_numeric > minv, na.rm = TRUE)) {
    validation_results <- c(validation_results, "out of range values")
  }

  return(validation_results)
}

validate_drop_column <- function(column, col_config){
  check_required <- ifelse(is.null(col_config$required), FALSE, col_config$required)
  val_options <- col_config$val_options

  validation_results <- c()

  if (check_required && any(is.na(column) | column == '')) {
    validation_results <- c(validation_results, "missing values")
  }

  column_nonmissing <- column[!(is.na(column) | column == "")]

  if (!all(column_nonmissing %in% val_options)) {
    validation_results <- c(validation_results, "invalid values")
  }

  return(validation_results)
}

validate_cb_column <- function(column, col_config){
  # if required, we need at least one checkbox checked, else 0
  mincb <- ifelse(is.null(col_config$required), 0, ifelse(col_config$required, 1, 0))
  # if min_checks is given, we update mincb
  mincb <- ifelse(is.null(col_config$min_checks), mincb, col_config$min_checks)
  maxcb <- ifelse(is.null(col_config$max_checks), 10000, col_config$max_checks)

  validation_results <- c()

  true_count <- sum(column)

  if (true_count < mincb) {
    validation_results <- c(validation_results, "too few checked")
  } else if (true_count > maxcb) {
    validation_results <- c(validation_results, "too many checked")
  }

  return(validation_results)
}

validate_date_column <- function(column, col_config){
  check_required <- ifelse(is.null(col_config$required), FALSE, col_config$required)

  if (check_required && any(is.na(column) | column == '')) {
    validation_results <- c(validation_results, "missing values")
  }

  column_nonmissing <- column[!(is.na(column) | column == "")]
  column_date <- as.Date(column_nonmissing, format = "%Y-%m-%d")

  if (any(is.na(column_date))) {
    validation_results <- c(validation_results, "invalid date formats")
  }

  return(validation_results)
}

validate_column <- function(column, col_config){
  switch(col_config$type,
         "character" = validate_char_column(column, col_config),
         "numeric" = validate_num_column(column, col_config),
         "dropdown" = validate_drop_column(column, col_config),
         "checkbox" = validate_cb_column(column, col_config),
         "date" = validate_date_column(column, col_config),
         stop("Unknown column type")
  )
}

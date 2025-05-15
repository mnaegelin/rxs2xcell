# Function to align df_authors to the structure of author_tbl_str
align_to_structure <- function(target_structure, source_data, colHeaders = NULL, ignore_colnames = FALSE) {
  # Get the column names of the target and source
  target_cols <- names(target_structure)

  if (ignore_colnames){
    names(source_data)[1:length(target_cols)] <- target_cols
  }

  source_cols <- names(source_data)
  missing_cols <- c()

  # Map alternative column names (if provided)
  if (!is.null(colHeaders)) {
    # Reverse the named vector to match alternative names to standard names
    alt_to_standard <- setNames(names(colHeaders), colHeaders)
    # Replace alternative names in source_cols with their standard names
    source_cols <- ifelse(source_cols %in% names(alt_to_standard),
                          alt_to_standard[source_cols],
                          source_cols)
  }

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
validate_required <- function(data, columns){
  data %>%
    dplyr::select(dplyr::all_of(columns)) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), \(x) sum(is.na(x) | x == ""))) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "column", values_to = "missing_count") %>%
    dplyr::filter(missing_count > 0)
}

validate_unique <- function(data, columns){
  data %>%
    dplyr::select(dplyr::all_of(columns)) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        \(x) sum(!is.na(x) & x != "") - dplyr::n_distinct(x[!is.na(x) & x != ""]))) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "column", values_to = "duplicate_count") %>%
    dplyr::filter(duplicate_count > 0)
}

validate_char_length <- function(data, length_bounds) {
  # `length_bounds` is a named list (not vector!) where each element is a vector c(min_length, max_length)
  # Example: list(column1 = c(5, 10), column2 = c(3, 8))
  data %>%
    dplyr::select(dplyr::all_of(names(length_bounds))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        \(x) sum(nchar(x[!is.na(x) & x != ""]) < length_bounds[[dplyr::cur_column()]][1] | nchar(x[!is.na(x) & x != ""]) > length_bounds[[dplyr::cur_column()]][2]))) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "column", values_to = "inv_length_count") %>%
    dplyr::filter(inv_length_count > 0)

}

validate_regex <- function(data, reg_patterns){
  # `reg_patterns` is a named list (not vector!) where each element is regular expression
  # Example: list(email_column = '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$')
  data %>%
    dplyr::select(dplyr::all_of(names(reg_patterns))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        \(x) sum(!grepl(reg_patterns[[dplyr::cur_column()]], x[!is.na(x) & x != ""])))) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "column", values_to = "inv_pattern_count") %>%
    dplyr::filter(inv_pattern_count > 0)
}

validate_options <- function(data, opt_list){
  # `opt_list` is a named list (not vector!) where each element is a vector of valid options
  # Example: list(column1 = c("option1", "option2"), column2 = c("optionA", "optionB"))
  data %>%
    dplyr::select(dplyr::all_of(names(opt_list))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        \(x) sum(!x[!is.na(x) & x != ""] %in% opt_list[[dplyr::cur_column()]]))) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "column", values_to = "inv_option_count") %>%
    dplyr::filter(inv_option_count > 0)
}



get_required_cols <- function(tbl_config){
  req_cols <- c()
  for (col in names(tbl_config)) {
    if (col != 'colHeaders'){
      if (tbl_config[[col]][['required']]) {
        req_cols <- c(req_cols, col)
      }
    }
  }
  return(req_cols)
}



# validate_required_mulitcol
# should return something like:
# col1, col2; "missing values, provide at least one of these"





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
    validation_results <- c(validation_results, "Required")
  }

  column_nonmissing <- column[!(is.na(column) | column == "")]

  if (any(nchar(column_nonmissing) < minl)) {
    validation_results <- c(validation_results, "Invalid lengths")
  } else if (any(nchar(column_nonmissing) > maxl)) {
    validation_results <- c(validation_results, "Invalid lengths")
  }

  if (check_regex && any(!grepl(regp, column_nonmissing))) {
    validation_results <- c(validation_results, "Invalid formats")
  }

  if (check_unique && any(duplicated(column_nonmissing))) {
    validation_results <- c(validation_results, "Non-unique values")
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
    validation_results <- c(validation_results, "Required")
  }

  if (check_min_val && any(column_numeric < minv, na.rm = TRUE)) {
    validation_results <- c(validation_results, "Out of range values")
  } else if (check_max_val && any(column_numeric > maxv, na.rm = TRUE)) {
    validation_results <- c(validation_results, "Out of range values")
  }

  return(validation_results)
}

validate_drop_column <- function(column, col_config){
  check_required <- ifelse(is.null(col_config$required), FALSE, col_config$required)
  val_options <- get_options(col_config$options)

  validation_results <- c()

  if (check_required && any(is.na(column) | column == '')) {
    validation_results <- c(validation_results, "Required")
  }

  column_nonmissing <- column[!(is.na(column) | column == "")]

  if (!all(column_nonmissing %in% val_options)) {
    validation_results <- c(validation_results, "Invalid options")
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

  true_count <- sum(column, na.rm = TRUE) # NA count a s FALSE

  if (true_count < mincb) {
    validation_results <- c(validation_results, "Too few checked")
  } else if (true_count > maxcb) {
    validation_results <- c(validation_results, "Too many checked")
  }

  return(validation_results)
}

validate_date_column <- function(column, col_config){
  check_required <- ifelse(is.null(col_config$required), FALSE, col_config$required)

  validation_results <- c()

  if (check_required && any(is.na(column) | column == '')) {
    validation_results <- c(validation_results, "Required")
  }

  column_nonmissing <- column[!(is.na(column) | column == "")]
  column_date <- as.Date(column_nonmissing, format = "%Y-%m-%d")

  if (any(is.na(column_date))) {
    validation_results <- c(validation_results, "Invalid date formats")
  }

  return(validation_results)
}

validate_column <- function(column, col_config){
  switch(col_config$type,
         "character" = validate_char_column(column, col_config),
         "text" = validate_char_column(column, col_config),
         "numeric" = validate_num_column(column, col_config),
         "dropdown" = validate_drop_column(column, col_config),
         "autocomplete" = validate_drop_column(column, col_config),
         "checkbox" = validate_cb_column(column, col_config),
         "date" = validate_date_column(column, col_config),
         stop("Unknown column type")
  )
}

collect_validator_results <- function(iv_validated, input_field_names, ns_prefix){
  results <- list()
  for (val_item in names(iv_validated)){
    if (!is.null(iv_validated[[val_item]])){
      item_name <- gsub(paste0(ns_prefix,'-'),'',val_item) # undo the namespace
      results[[item_name]] <- list(
        field = input_field_names[[item_name]],
        type = iv_validated[[val_item]]$type,
        message = iv_validated[[val_item]]$message
      )
    }
  }
  return(results)
}

collect_hot_val_results <- function(df, tbl_config){
  results <- sapply(
    colnames(df),
    function(col_name) {
      val_check <- validate_column(df[[col_name]], tbl_config[[col_name]])
      if (length(val_check) > 0) {
        list(
          field = tbl_config[[col_name]]$col_header,
          type = 'error',
          message = paste(val_check, collapse = ', ')
        )
      } else {NULL}
    }, simplify = FALSE, USE.NAMES = TRUE)
  return(results)
}

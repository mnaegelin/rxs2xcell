# Load necessary libraries
# library(tibble)
# library(dplyr)

# TODO: doesn't quite work right, should a dir be set to processed only once all subdirs are read?

# Define the spider function with resume and interim results saving
spider <- function(path, pattern, state_file = "spider_state.csv", results_file = "spider_results.csv") {
  # Load processed directories if the state file exists
  if (file.exists(state_file)) {
    processed_dirs <- read.csv(state_file, stringsAsFactors = FALSE)$path
  } else {
    processed_dirs <- character()
  }

  # Load interim results if the results file exists
  if (file.exists(results_file)) {
    results <- read.csv(results_file, stringsAsFactors = FALSE)
  } else {
    results <- tibble::tibble(file_name = character(), file_path = character())
  }

  # Define an inner function for recursive crawling
  crawl <- function(path) {
    # Skip directories already processed
    if (path %in% processed_dirs) {
      return(tibble::tibble(file_name = character(), file_path = character()))
    }

    # List all files and directories in the current path
    all_items <- list.files(path, full.names = TRUE)

    # Separate files and directories
    files <- all_items[file.info(all_items)$isdir == FALSE]
    dirs <- all_items[file.info(all_items)$isdir == TRUE]

    # Check for files matching the pattern
    matching_files <- files[grepl(pattern, basename(files))]

    # Save results for matching files
    temp_results <- tibble::tibble(
      file_name = basename(matching_files),
      file_path = dirname(matching_files)
    )

    # Append and save interim results to the results file
    if (nrow(temp_results) > 0) {
      results <<- dplyr::bind_rows(results, temp_results)
      write.csv(results, results_file, row.names = FALSE)
    }

    # Save the current directory to the state file
    write.csv(data.frame(path = c(processed_dirs, path)), state_file, row.names = FALSE)

    # Recursively crawl subdirectories
    for (subdir in dirs) {
      temp_results <- dplyr::bind_rows(temp_results, crawl(subdir))
    }

    return(temp_results)
  }

  # Try to execute the crawl function and catch keyboard interrupts
  tryCatch({
    crawl(path)
  }, interrupt = function(e) {
    message("Spider interrupted. State and results saved.")
  })

  # Return the final results
  return(results)
}

# Usage example
folders_PF <- dir('/Volumes/fe/dendro/Dendrosciences_All',
                  pattern = "PatrickFonti", recursive = FALSE, full.names = TRUE)

starting_path <- folders_PF[1]
file_pattern <- ".*_ROXAS_Settings\\.txt$"

# Call the spider function
found_files <- spider(starting_path, file_pattern)

# Print the results
print(found_files)

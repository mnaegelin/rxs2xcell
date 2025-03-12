

test_that("get_input_files throws error for invalid directory", {
  expect_error(get_input_files('some_nonexistent_dir'))
})

test_that("get_input_files finds correctly matched files", {
  # Create a temporary directory for mock ROXAS files
  test_dir <- tempfile("test_data_dir")
  dir.create(test_dir)
  sub_dir <- file.path(test_dir, "siteA")
  dir.create(sub_dir)
  subsub_dir <- file.path(sub_dir, 'tree1')
  dir.create(subsub_dir)
  base_name <- 'siteA_spB_tA1_1_1'

  # Create test files that should be recognized
  writeLines("", paste0(subsub_dir, "/", base_name, "_Output_Cells.txt"))
  writeLines("", paste0(subsub_dir, "/", base_name, "_Output_Rings.txt"))
  writeLines("", paste0(subsub_dir, "/", base_name, "_ROXAS_Settings.txt"))
  writeBin(raw(100), paste0(subsub_dir, "/", base_name, ".jpg"))

  # Create test files that should be ignored (general and specific)
  writeLines("", paste0(subsub_dir, "/", base_name, "_some_other_file.csv"))
  writeLines("", paste0(subsub_dir, "/", "_another_file.csv"))
  writeLines("", paste0(subsub_dir, "/", base_name, "_annotated.jpg"))
  writeLines("", paste0(subsub_dir, "/", base_name, "_ReferenceSeries.jpg"))
  writeLines("", paste0(subsub_dir, "/", base_name, "_Preview.jpg"))

  expected_result <- list(
    fname_cells = paste0(subsub_dir, "/", base_name, "_Output_Cells.txt"),
    fname_rings = paste0(subsub_dir, "/", base_name, "_Output_Rings.txt"),
    fname_settings = paste0(subsub_dir, "/", base_name, "_ROXAS_Settings.txt"),
    fname_image = paste0(subsub_dir, "/", base_name, ".jpg")
  )

  # Run test
  msgs <- capture_messages({
    result <- get_input_files(test_dir)
  })

  expect_equal(result, expected_result)

  # Clean up
  unlink(test_dir, recursive = TRUE)
})


test_that("get_input_files throws error when files don't match", {
  # Create a temporary directory for mock ROXAS files
  test_dir <- tempfile("test_data_dir")
  dir.create(test_dir)

  # Create test files that are incomplete
  base_name <- 'siteA_spB_tA1_1_1'
  writeLines("", paste0(test_dir, "/", base_name, "_Output_Cells.txt"))
  writeLines("", paste0(test_dir, "/", base_name, "_Output_Rings.txt"))
  #writeLines("", paste0(test_dir, "/", base_name, "_ROXAS_Settings.txt"))
  writeBin(raw(100), paste0(test_dir, "/", base_name, ".jpg"))

  # Run test
  expect_error(get_input_files(test_dir), "ROXAS output files DON'T MATCH for")

  # Clean up
  unlink(test_dir, recursive = TRUE)

})


test_that("get_input_files throws error when no ROXAS files found", {
  # Create a temporary directory without any ROXAS files
  test_dir <- tempfile("test_data_dir")
  dir.create(test_dir)
  writeLines("", paste0(test_dir, "/", "some_other_file.csv"))

  # Run test
  expect_error(get_input_files(test_dir), "NO ROXAS output files found under path")

  # Clean up
  unlink(test_dir, recursive = TRUE)

})

# "get_input_files handles multiple matched files"
# "get_input_files matches JPEG extension variations"

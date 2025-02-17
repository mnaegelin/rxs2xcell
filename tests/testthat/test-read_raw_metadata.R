test_that("nonexistent dir throws error", {
  expect_error(get_input_files('some_nonexistent_dir'))
})


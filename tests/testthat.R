# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(rxs2xcell)
# library(mockery)
#
# # Define mock functions for beepr
# mock_beep <- function(sound, expr) {
#   if (!is.null(expr)) {
#     return(expr)
#   }
# }
#
# mock_beep_on_error <- function(expr, sound) {
#   tryCatch(expr, error = function(e) {
#     stop(e$message)
#   })
# }
#
# # Set up the mocks globally
# withr::with_mock(
#   "beepr::beep" = mock_beep,
#   "beepr::beep_on_error" = mock_beep_on_error,
#   {
#     test_check("yourpackage")  # Replace with your package name
#   }
# )

test_check("rxs2xcell")

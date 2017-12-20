library(testthat)
library(forvol)

# A silly test to get going
test_that("csv_path is a character", {
  expect_is(csv_path, "character")
})

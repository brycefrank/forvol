library(testthat)
library(forvol)

test_check("forvol")


test_that("cvts_equations.csv loads", {
  expect_is(read.csv(file.path(csv_path, "cvts_equations.csv")))
})

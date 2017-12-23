library(testthat)
library(forvol)

# A silly test to get going
test_that("csv_path is a character", {
  expect_is(csv_path, "character")
})

# Until we are confident that all the equations for some volume type
# (i.e. CVTS, CV4 etc) are implemented, I will just choose to test
# only one species / region combination
test_that("Coefficients for an arbitrary region and species are retrieved", {
  expect_equal(get_coefs('OR_W', 202)[[2]], -3.21809)
})

test_that("build_equation returns a function for an arbitrary region
           and species", {
  expect_equal(typeof(build_equation('OR_W', 202)), "closure")
})

# TODO This one does not seem to work...
#test_that("get_equation_id returns a character for an arbitrary
#           region and species", {
#  expect_equal(get_equation_id('OR_W', 202), as.numeric("CU202020"))
#})

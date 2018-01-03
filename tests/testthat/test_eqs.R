library(testthat)
library(forvol)

# A silly test to get going
test_that("csv_path is a character", {
  expect_is(csv_path, "character")
})


### TESTS FOR eq.R ###

# Until we are confident that all the equations for some volume type
# (i.e. CVTS, CV4 etc) are implemented, I will just choose to test
# only one species / region combination
test_that("build_equation returns a function for an arbitrary region
           and species", {
  expect_equal(typeof(build_equation("OR_W", 202, "cvts")), "closure")
})

test_that("get_equation_id returns a character for an arbitrary
           region and species", {
  expect_equal(as.character(get_equation_id("OR_W", 202)), "CU202020")
})

test_that("Coefficients for an arbitrary region and species are retrieved", {
  expect_equal(get_coefs('OR_W', 202)[[2]], -3.21809)
})

### TESTS FOR helper.R ###

#TODO: Write this test
#test_that("find_CSV returns exactly one configuration CSV for each region", {
#  regions <- read.csv(file.path(csv_path, "regions.csv"))
#})

### TESTS FOR calc.R ###
test_that("calc_vol calculates the correct volume for single arbitrary region and species",{
  expect_equal(round(calc_vol(202, 12, 100, "OR_W", "sv632")$sv632), 2879)
})

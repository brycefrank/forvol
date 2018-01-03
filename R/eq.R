# The main script to hold functions for building equations

#' Gets the equation string for a specified region
#' and species code. This is read directly from the 'cvts_equations.csv'
#'
#'
#' @param region The geographic region of interest.
#' @param spcd The FIA species code.
#' @return Returns the equation (as a string)
#' @export
get_equation_id <- function(region, spcd) {
  ## TODO check for multiple returns for FindCSV and species within config
  config <- utils::read.csv(forvol::find_CSV(sprintf("^%s_config", region)))

  ## Find the equation string for the given species
  eq_str <- config$CF_VOL_EQ[which(config$SPECIES_NUM == spcd)]
  return (eq_str)
}

#' Builds the default volume equation as defined by the US
#' Forest Service configuration datasets.
#'
#' @param region Region code
#' @param spcd FIA species code
#' @return An R function that computes CVTS for the input
#' region and species code. If the code cannot be built,
#' @export
build_equation <- function(region, spcd, vol_type) {
  eq_id <- forvol::get_equation_id(region, spcd)

  eq_csv <- utils::read.csv(file.path(system.file("csv", package = "forvol"),
                                      sprintf("%s_equations.csv", vol_type)),
                                      stringsAsFactors = FALSE)

  betas <- forvol::get_coefs(region, spcd)

  ifelse(is.na(betas), return(NA), 1)

  # Check if equation id is in the equations data
  if (!(eq_id %in% eq_csv$CF_VOL_EQ)) {
    return(NA)
  }

  eq_string <- eq_csv$EQ_STRING[which(eq_csv$CF_VOL_EQ == eq_id)]

  # Convert string to expression without coefficients
  func <- parse(text = eq_string)

  # Populate expression with coefficients, convert back to string
  func_betas <- deparse(do.call("substitute", list(func[[1]], betas)))

  # Reparse with coefficients
  func_betas <- eval(parse(text = func_betas))

  return(func_betas)

}

#' Gets the coefficient table for a species in a specific region.
#'
#' @param region The region code string to retrieve from, for instance
#' 'OR_W' is western Oregon.
#' @param spcd The FIA species code
#' @return A 1 row coefficient table containing the values of the coefficients
#' for the specified region and species.
#' @export
get_coefs <- function(region, spcd) {
  config <- forvol::find_CSV(sprintf("^%s_config", region))

  # Catch missing region code
  if (identical(config, character(0))) {
    stop(sprintf("Region code '%s' not found.", region))
  }

  # Load into memory
  config <- utils::read.csv(config)

  # Catch missing species code for the given region
  ifelse(!(spcd %in% config$SPECIES_NUM), return(NA), 1)

  # Get the coefficient table and reset the species
  coef_table <- config$COEF_TABLE[which(config$SPECIES_NUM == spcd)]
  coef_spcd <- config$COEF_TBL_SP[which(config$SPECIES_NUM == spcd)]

  # Get the coefficients csv
  coef_table <- utils::read.csv(file.path(csv_path, paste(coef_table,
                                                   ".csv", sep = "")))
  # Remove NAs
  coef_table <- coef_table[stats::complete.cases(coef_table), ]

  # Check for erroneous coefficient table TODO fix AK_SECN tables
  if (!(names(coef_table)[1] == "Species")) {
    return(NA)
  }

  # If first value of coef_table is "all" return that row,
  # Otherwise return the row of the input species
  if (coef_table$Species[1] == "All") {
    return(coef_table)
  }
  else {
    coef_table <- coef_table[which(coef_table$Species == coef_spcd), ]
    return(coef_table)
  }
}



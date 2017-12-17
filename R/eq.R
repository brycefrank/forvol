#' Gets the equation string for a specified region
#' and species code. This is read directly from the 'cvts_equations.csv'
#'
#'
#' @param region The geographic region of interest.
#' @param spcd The FIA species code.
#' @return Returns the equation (as a string)
get_equation_string <- function(region, spcd) {
  ## TODO check for multiple returns for FindCSV and species within config
  config <- read.csv(find_CSV(sprintf("^%s_config", region)))

  ## Find the equation string for the given species
  eq_str <- config$CF_VOL_EQ[which(config$SPECIES_NUM == spcd)]
  return (eq_str)
}


#' A more straightforward equation builder
#' TODO Decide whether or not to delete the original
#'
build_equation2 <- function(region, spcd) {
  # Using the equation id and given coefficient,
  # attempts to build the volume equation

  # Get the equation string from the configuration file
  eq_id <- get_equation_string(region, spcd)

  # Get equation string using id from the csv
  eq_csv <- read.csv(file.path(system.file("csv", package = "forvol"),
                     "cvts_equations.csv"),
                     stringsAsFactors = FALSE)
  beta <- get_coefs(region, spcd)

  # Check if equation id is in the equations data
  if (!(eq_id %in% eq_csv$CF_VOL_EQ)) {
    # TODO There are probably more robust/formal methods of error handling.
    #print("The equation string '%s' could not be found in cvts_equations.csv's
    #        either an error occurred or the equation is not yet implemented")
    return("EE")
  }

  eq_string <- eq_csv$CVTS_1[which(eq_csv$CF_VOL_EQ == eq_id)]

  # Convert string to expression without coefficients
  func <- parse(text = eq_string)

  # Populate expression with coefficients, convert back to string
  func_betas <- deparse(do.call("substitute", list(func[[1]], beta)))

  # Reparse with coefficients
  func_betas <- eval(parse(text = func_betas))

  return(func_betas)
}

build_equation <- function(eq_id, coefs_table) {
  # Using the equation id and given coefficient,
  # attempts to build the volume equation

  # Get equation string using id from the csv
  eq_csv <- read.csv(file.path(system.file("csv", package = "forvol"),
                     "cvts_equations.csv"),
                     stringsAsFactors = FALSE)
  beta <- coefs_table

  # Check if equation id is in the equations data
  if (!(eq_id %in% eq_csv$CVTS_1)) {
    # TODO There are probably more robust/formal methods of error handling.
    print("The equation string '%s' could not be found in cvts_equations.csv's
            either an error occurred or the equation is not yet implemented")
    return("EE")
  }

  eq_string <- eq_csv$CVTS_1[which(eq_csv$CF_VOL_EQ == eq_id)]

  # Convert string to expression without coefficients
  func <- parse(text = eq_string)

  # Populate expression with coefficients, convert back to string
  func_betas <- deparse(do.call("substitute", list(func[[1]], beta)))

  # Reparse with coefficients
  func_betas <- eval(parse(text = func_betas))

  return(func_betas)
}

#' Serves as the default volume calculation function,
#' retrieves the coefficient table for the specified species,
#' region and volume type
#'
#' @param dbh A column of diameter at breast height values (or a single value)
#' @param ht A column of height values (or a single value)
#' @param spcd FIA species code
#' @param region Geographic region
calc_cvts <- function(dbh, ht, region, spcd) {
  # Get the coefficient table
  coef <- get_coefs(region, spcd)

  # Get the equation string
  eq_string <- get_equation_string(region, spcd)

  # Build the volume equation
  eq <- build_equation(eq_string, coef)

  # Apply the function to dbh and ht and return
  mapply(eq, dbh, ht)
}


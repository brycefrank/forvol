# The main script to hold functions for building equations

library(dplyr)

#' Gets the equation string for a specified region
#' and species code. This is read directly from the 'cvts_equations.csv'
#'
#'
#' @param region The geographic region of interest.
#' @param spcd The FIA species code.
#' @return Returns the equation (as a string)
get_equation_id <- function(region, spcd) {
  ## TODO check for multiple returns for FindCSV and species within config

  config_search <- find_CSV(sprintf("^%s_config", region))

  config <- read.csv(find_CSV(sprintf("^%s_config", region)))

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
#' returns "EE"
#' @export
build_equation <- function(region, spcd) {
  # TODO Handle empty equation 'cells'

  # Get the equation string from the configuration file
  eq_id <- get_equation_id(region, spcd)

  # Get equation string using id from the csv
  eq_csv <- read.csv(file.path(system.file("csv", package = "forvol"),
                     "cvts_equations.csv"),
                     stringsAsFactors = FALSE)
  beta <- get_coefs(region, spcd)

  ifelse(is.na(beta), return(NA), 1)

  # Check if equation id is in the equations data
  if (!(eq_id %in% eq_csv$CF_VOL_EQ)) {
    # TODO There are probably more robust/formal methods of error handling.
    #print("The equation string '%s' could not be found in cvts_equations.csv's
    #        either an error occurred or the equation is not yet implemented")
    return(NA)
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

#' Gets the coefficient table for a species in a specific region.
#'
#' @param region The region code string to retrieve from, for instance
#' 'OR_W' is western Oregon.
#' @param spcd The FIA species code
#' @return A 1 row coefficient table containing the values of the coefficients
#' for the specified region and species.
#' @export
get_coefs <- function(region, spcd) {
  config <- find_CSV(sprintf("^%s_config", region))

  # Catch missing region code
  if (identical(config, character(0))) {
    stop(sprintf("Region code '%s' not found.", region))
  }

  # Load into memory
  config <- read.csv(config)

  # Catch missing species code for the given region
  ifelse(!(spcd %in% config$SPECIES_NUM), return(NA), 1)

  # Get the coefficient table and reset the species
  coef_table <- config$COEF_TABLE[which(config$SPECIES_NUM == spcd)]
  coef_spcd <- config$COEF_TBL_SP[which(config$SPECIES_NUM == spcd)]

  # Get the coefficients csv
  coef_table <- read.csv(file.path(csv_path, paste(coef_table,
                                                   ".csv", sep = "")))
  # Remove NAs
  coef_table <- coef_table[complete.cases(coef_table), ]

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

#' Serves as the default volume calculation function,
#' retrieves the coefficient table for the specified species,
#' region and volume type, generates the function, and applies
#' it to the input data.
#'
#' @param dbh A column of diameter at breast height values (or a single value)
#' @param ht A column of height values (or a single value)
#' @param spcd FIA species code
#' @param region Geographic region
#' @export
calc_cvts <- function(dbh, ht, region, spcd) {
  # Build all of the equations needed from the input spcd
  # and region (should work for multiple regions)


  # Get unique region and scd combinations
  uniques <- unique(data.frame(region, spcd))

  # Build the equations
  uniques$eqs <- mapply(build_equation, uniques$region, uniques$spcd)

  # Split the data into spcd - region groups
  tree_data <- data.frame(dbh, ht, region, spcd)
  tree_split <- split(tree_data, list(tree_data$region, tree_data$spcd))

  new_tree <- data.frame()
  for (group in tree_split) {
    # Get the 'group key'
    region <- group$region[1]
    spcd <- group$spcd[1]

    # Get the equation from eqs
    eq <- uniques$eqs[which((uniques$spcd == spcd & uniques$region == region))]
    eq <- eq[[1]]
    # Apply the equation to each record in the group
    #print(typeof(eq))
    if(typeof(eq) == "closure") {
      group$cvts <- mapply(eq, group$dbh, group$ht)
    } else {
      group$cvts <- NA
    }

    new_tree <- rbind(new_tree ,group)
  }

  return(new_tree)

}














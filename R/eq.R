#' Gets the equation string for a specified region
#' and species code. This is read directly from the 'cvts_equations.csv'
#'
#'
#' @param region The geographic region of interest.
#' @param spcd The FIA species code.
#' @return Returns the equation (as a string)
get_equation_string <- function(region, spcd) {
  ## TODO check for multiple returns for FindCSV and species within config
  config <- read.csv(find_CSV(region))

  ## Find the equation string for the given species
  eq_str <- config$CF_VOL_EQ[which(config$SPECIES_NUM == spcd)]
  return (eq_str)
}

##str_eval <- function(x){
##  return(eval(parse(text=x)))
##}

build_equation <- function(eq_id, coefs_table) {
  ## Using the equation id and given coefficient,
  ## attempts to build the volume equation

  ## Get equation string using id from the csv
  eq_csv <- read.csv(file.path(system.file("csv", package = "forvol"),
                     "cvts_equations.csv"),
                     stringsAsFactors = FALSE)
  beta <- coefs_table

  ## Find correct equation ID
  eq_string <- eq_csv$CVTS_1[which(eq_csv$CF_VOL_EQ == eq_id)]

  ## Convert string to expression without coefficients
  func <- parse(text = eq_string)

  ## Populate expression with coefficients, convert back to string
  func_betas <- deparse(do.call("substitute", list(func[[1]], beta)))

  ## Reparse with coefficients
  func_betas <- eval(parse(text = func_betas))

  return(func_betas)
}

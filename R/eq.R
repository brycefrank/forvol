### These functions handle the building of equation expressions

get_equation_string <- function(region, spcd) {
  ## TODO check for multiple returns for FindCSV and species within config
  config <- read.csv(find_CSV(region))

  ## Find the equation string for the given species
  eq_str <- config$CF_VOL_EQ[which(config$SPECIES_NUM == spcd)]
  return (eq_str)

}

build_equation <- function(eq_id, coefs_table) {
  ## Using the equation id and given coefficient, builds the volume equation

  ## Get equation string using id from the csv
  eq_csv <- read.csv(file.path(system.file("csv", package = "forvol"),
                     "cvts_equations.csv"),
                     stringsAsFactors = FALSE)
  beta <- coefs_table

  ## Find correct equation ID

  eq_string <- eq_csv$CVTS_1[which(eq_csv$CF_VOL_EQ == eq_id)]

  ## Convert string to expression without coefficients
  func <- parse(text = eq_string)

  ## Populate expression with coefficients
  exp <- do.call("substitute", list(func[[1]], beta))

  return(exp)
}

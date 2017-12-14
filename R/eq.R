### A place for volume equations


## Brackett 1973
## Browne 1962
## Pillsbury and Kirley 1984
## Larson and Winterberger 1988
## Campbell 2009
## Embry and Haack 1965
## MacLean and Berger 1976
## Bruce 1984
## TODO: Finish these...could be easily compiled from the existing spreadsheets

GetEquationString <- function(region, spcd) {
  ## TODO check for multiple returns for FindCSV and species within config
  config <- read.csv(FindCSV(region))

  ## Find the equation string for the given species
  
  eq_str <- config$CF_VOL_EQ[which(config$SPECIES_NUM==spcd)]
  return (eq_str)

}

BuildEquation <- function(eq_id, coefs_table) {
  ## Using the equation id and given coefficient, builds the volume equation

  ## Get equation string using id from the csv
  eq_csv <- read.csv(file.path(system.file('csv', package = 'forvol'), 'cvts_equations.csv'),
                     stringsAsFactors=FALSE)
  beta <- coefs_table

  ## Find correct equation ID

  eq_string <- eq_csv$CVTS_1[which(eq_csv$CF_VOL_EQ == eq_id)]

  ## Convert string to expression without coefficients
  func <- parse(text=eq_string)

  ## Populate expression with coefficients
  exp <- do.call("substitute", list(func[[1]], beta))

  return(exp)
}

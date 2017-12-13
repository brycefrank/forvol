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


BuildEquation <- function(eq_id, coefs_table) {
  ## Using the equation id and given coefficient, builds the volume equation
  ## 

  ## Get equation string using id from the csv
  eq_csv <- read.csv(file.path(system.file('csv', package = 'forvol'), 'all_eqs.csv'),
                     stringsAsFactors=FALSE)
  beta <- coefs_table

  ## Fnd correct equation ID

  eq_string <- eq_csv$R_STRING[which(eq_csv$CF_VOL_EQ == eq_id)]

  ## Convert string to expression
  func <- parse(text=eq_string)

  ## Populate expression with coefficients
  exp <- do.call("substitute", list(func[[1]], beta))

  return(exp)

}


Brackett73 <- function(dbh, ht, coefs_table) {
  ## Set the coefficients to a more readable form
  ## TODO Think about moving upstream?
  b <- coefs_table[, -(1)]
  cvts <- 10^(b[1] + b[2]*log10(dbh)*log10(ht)+ b[3] * log10(dbh)**2 + b[4] * log10(dbh)+ b[5]
    * log10(ht) + b[6] * log10(ht)**2)

  return(cvts[1])
}

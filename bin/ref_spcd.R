# Going to make a species list key
# and species lookup functions
read.csv("../csv/ref_species_reduced.csv")


#' Returns the common name of a species
#' given some input FIA species code
#'
#' @param spcd The FIA species code
#' @return A string of the species common name
species_lookup <- function(spcd) {
  # TODO Remove this once it is in the R directory
  # TODO And make csv_path outside the function
  csv_path <- system.file("csv", package = "forvol")

  spcd_data <- read.csv('../csv/ref_species_reduced.csv')

  spcd_data$COMMON_NAME[which(spcd_data$SPCD == spcd)]
}



# Functions that help the end user look up information.

#' Returns the common name of a species
#' given some input FIA species code
#'
#' @param spcd The FIA species code
#' @return A string of the species common name
common_lookup <- function(spcd, latin = FALSE) {
  csv_path <- system.file("csv", package = "forvol")

  spcd_data <- read.csv('../csv/ref_species_reduced.csv')

  spcd_data$COMMON_NAME[which(spcd_data$SPCD == spcd)]
}

spcd_lookup <- function(common_name) {
}

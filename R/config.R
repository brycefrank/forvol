csv_path <- system.file("csv", package = "forvol")

#' Finds the csv with the input search term
#' Mostly for use as a debugging tool
#'
#' @param regexp Regex search term as string
#' @return The absolute path to the csv file
find_CSV <- function(regexp) {
  csv <- list.files(csv_path, regexp, ignore.case = TRUE)

  return(file.path(csv_path, csv))
}

#' Gets the coefficient table for a species in a specific region.:w
#'
#' @param region_code The region code string to retrieve from, for instance
#' 'OR_W' is western Oregon.
#' @param spcd The FIA species code
#' @return A 1 row coefficient table containing the values of the coefficients
#' for the specified region and species.
get_coefs <- function(region_code, spcd) {
  config_regex <- paste(region_code, "_", "config", sep = "")
  config <- list.files(csv_path, config_regex, ignore.case = TRUE)

  # Catch missing region code
  if (identical(config, character(0))) {
    stop(sprintf("Region code '%s' not found.", region_code))
  }

  config <- file.path(csv_path, config)

  # Load into memory
  config <- read.csv(config)

  # Catch missing species code for the given region
  if (!(spcd %in% config$SPCD)) {
    stop(sprintf("Species code coefficients format '%s'
                  not found for this region.", spcd))
  }

  # Get the coefficient table
  coef_table <- config[which(config$COEF_TBL_S == spcd), ]$COEF_TABLE
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
    coef_table <- coef_table[which(coef_table$Species == spcd), ]
    return(coef_table)
  }
}

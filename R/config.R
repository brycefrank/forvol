### Configuration lookup
### Input a region code, find the csv...for now


### List config files in the csv directory


csv_path <- system.file("csv", package = "forvol")

find_CSV <- function(regexp) {
  # Finds the csv with the input string regexp and returns the absolute path
  csv <- list.files(csv_path, regexp, ignore.case = TRUE)

  return(file.path(csv_path, csv))
}

get_coefs <- function(region_code, species_num) {
  # Get region config table file path
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
  if (!(species_num %in% config$SPECIES_NUM)) {
    stop(sprintf("Species code '%s' not found for this region.", species_num))
  }

  # Get the coefficient table
  coef_table <- config[which(config$SPECIES_NUM == species_num), ]$COEF_TABLE

  # Get the coefficients csv
  coef_table <- read.csv(file.path(csv_path, paste(coef_table,
                                                   ".csv", sep = "")))

  # If first value of coef_table is "all" return that row,
  # Otherwise return the row of the input species
  if (coef_table$Species[1] == "All") {
    return(coef_table)
  }
  else {
    coef_table <- coef_table[which(coef_table$Species == species_num), ]
    return(coef_table)
  }
}

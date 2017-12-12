### Configuration lookup
### Input a region code, find the csv...for now


### List config files in the csv directory


csv_path <- system.file('csv', package = 'forvol')

FindCSV <- function() {
  # Finds the csv with the input string
}

GetCoefs <- function(region_code, species_num) {
  # Get region config table file path
  config_regex <- paste(region_code, '_', 'config', sep='')

  config <- list.files(csv_path, config_regex, ignore.case=TRUE)
  print(config)
  config <- file.path(csv_path, config)

  # Load into memory
  config <- read.csv(config)
  print(config)

  # Get the coefficient table
  coef_table <- config[which(config$SPECIES_NUM == species_num),]$COEF_TABLE

  # And the coefficient species
  coef_tbl_sp <- config[which(config$SPECIES_NUM == species_num),]$COEF_TBL_SP

  # Get the coefficients csv
  print(file.path(csv_path, paste(coef_table, '.csv', sep='')))
  coef_table <- read.csv(file.path(csv_path, paste(coef_table, '.csv', sep='')))


  return(coef_table)
}


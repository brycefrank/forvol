#' Testing scripts to verify the existence, accuracy and functionality
#' of cubic foot volume (top and stump) equations. Because these tests
#' do not strictly expect equivalence, they are not considered a formal
#' automated test (for now)
#'
library(devtools)
library(dplyr)
load_all("../../forvol")


# Adjust some column names for consistency
test_data <- readRDS("cvts_test.rds")
test_data <- mutate(test_data, CVTS = VOLTSGRS)

eq <- file.path(csv_path, "cvts_equations.csv")
eq <- read.csv(eq)


#' Tests a specific CVTS function against the FIA dataset
#' ("cvts_test.rds") this is meant to be an internal function
#'
#' \code{test_eq} returns the average difference between
#' the new calculated volumes and the testing dataset.
#'
#' @param spcd The FIA species code
#' @param region_string The configuration ID specifying the region to test
#' @param new_cvts_func The new CVTS equation to test as an R expression
#' @return The average difference (float) mean(test_cvts - new_cvts)
#'
#' @examples test_eq(202, 'W_OR', build_equation('CU202020', get_coefs('W_OR', 202)))
## TODO Fix the above example, which will require fixing the structure of test
## eq anyway...which is bad
test_eq <- function(spcd, region, new_cvts_func) {
  test_df <- filter(test_data, SPCD == spcd, config_code == region) %>%
    select(DO_BH, HT_TOT, CVTS) %>%
    rename(dbh = DO_BH, ht = HT_TOT) %>%
    na.omit()

  if(nrow(test_df) == 0){
    return("No testing data available.")
  }
  
  
  new_vols <- mapply(new_cvts_func, test_df$dbh, test_df$ht)

  ## I need to apply the function instead of calling it on the columns
  return(mean(test_df$CVTS - new_vols))
}


#' Tests for the existence and performance of equation IDs in a given configuration
#'
#' @param config_id The configuration ID specifying the region to test
#'   ex: 'W_OR'
#' @return A dataframe of 'results', either not yet implemented equations or the performance
#' as calculated by \code{test_eq}
#'
#' @examples 
test_config <- function (config_path) {
  ## Tests for the existence and performance of equation IDs in a given
  ## configuration

  ## Read in configuration csv as dataframe
  ##config_csv <- list.files(csv_path, pattern = config_id, full.names = TRUE)
  config <- read.csv(config_path)

  ## Find unique equations that are missing
  missing <- config$CF_VOL_EQ[!(config$CF_VOL_EQ %in% eq$CF_VOL_EQ)]
  missing <- unique(missing)

  ##TODO Implement testing equation string performance
  
  ## Create dataframe
  error_frame <- data.frame(CF_VOL_EQ = missing,
                            Result = 'Missing')

  return(error_frame)
}

test_all_configs <- function() {
  ## Master dataframe
  all_config_errors <- data.frame()

  ## Get all config paths
  configs <- list.files(csv_path, pattern = "config", full.names = TRUE)

  for (config_path in configs) {
    all_config_errors <- rbind(all_config_errors, test_config(config_path))
  }

  return(unique(all_config_errors))
}


nrow(test_all_configs())
a <- test_config('/home/bryce/Programming/forvol/csv/OR_E_config.csv')
b <-test_config('/home/bryce/Programming/forvol/csv/OR_W_config.csv')

sum(a$CF_VOL_EQ %in% b$CF_VOL_EQ)















## Example test:
##df_cvts <- BuildEquation('CU202020', GetCoefs('OR_W', 202))

##test_cvts(202, 'Western Oregon', df_cvts)

##wh_cvts <- BuildEquation('CU000077', GetCoefs('OR_W', 242))
##test_cvts(242, 'Western Oregon', wh_cvts)

##filter(test_data, SPCD == 242)






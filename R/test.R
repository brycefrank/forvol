#' Testing scripts to verify the existence, accuracy and functionality
#' of cubic foot volume (top and stump) equations. Because these tests
#' do not strictly expect equivalence, they are not considered a formal
#' automated test (for now). This is mostly used for debugging and
#' development purposes, with some opportunity for testing volume
#' equation performance.
#'
library(dplyr)

# Adjust some column names for consistency
test_data <- readRDS(system.file("data/cvts_test.rds", package = "forvol"))
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
test_eq <- function(region, spcd, new_cvts_func) {
  # Create a testing data frame for the specified region, spcd and function
  test_df <- filter(test_data, SPCD == spcd, config_code == region) %>%
    select(DO_BH, HT_TOT, CVTS) %>%
    rename(dbh = DO_BH, ht = HT_TOT) %>%
    na.omit()

  # If the data frame is empty, raise an error.
  if (nrow(test_df) == 0){
    return("No testing data available.")
  }

  new_vols <- mapply(new_cvts_func, test_df$dbh, test_df$ht)

  return(mean(test_df$CVTS - new_vols))
}


#' Tests for the existence and performance of equation IDs in a given configuration
#'
#' @param config_id The configuration ID specifying the region to test
#'   ex: 'W_OR'
#' @return A dataframe of 'results', either not yet implemented equations or the performance
#' as calculated by \code{test_eq}
test_config <- function(config_id) {
  ## Read in configuration csv as dataframe
  config_path <- find_CSV(config_id)
  config <- read.csv(config_path)

  ## Find unique equations that are missing
  missing <- config$CF_VOL_EQ[!(config$CF_VOL_EQ %in% eq$CF_VOL_EQ)]
  missing <- unique(missing)

  ##TODO Implement testing equation string performance

  ## Create dataframe
  error_frame <- data.frame(CF_VOL_EQ = missing,
                            Result = "Missing")

  return(error_frame)
}

test_all_configs <- function() {
  ## Master dataframe
  all_config_errors <- data.frame()

  ## Get all config paths
  configs <- list.files(csv_path, pattern = "config", full.names = TRUE)

  for (config_id in configs) {
    all_config_errors <- rbind(all_config_errors, test_config(config_id))
  }

  return(unique(all_config_errors))
}


#' Configuration test for species instead of equations,
#' serves as a function to construct the equations completion
#' status figure in the readme TODO
#'
test_config_sp <- function() {
  # Get region list

  regions <- read.csv(find_CSV('regions'))

  # TODO replace list from 1 to 999 with actual SPCD codes
  spcd_unique <- as.character(read.csv(find_CSV("ref_species_red"))$SPCD)
  vis_frame <- data.frame(spcd = spcd_unique)


  for (region in regions[[1]]) {
    #print(region)
    print(find_CSV(sprintf("^%s_config", region)))
    test_config <- read.csv(find_CSV(sprintf("^%s_config", region)))

    # Attempt to build CVTS equation for all species
    eqs <- mapply(build_equation2, rep(region, nrow(test_config)), test_config$SPECIES_NUM)

    # Boolean, true if not returning a function
    eqs <- 'character' == sapply(eqs, typeof)

    join_frame <- data.frame(spcd = test_config$SPECIES_NUM)
    join_frame[sprintf('%s', region)] <- eqs
    vis_frame <- merge(vis_frame, join_frame, all.x = TRUE)
  }

  library(tidyr)

  # Filter out rows that have no occurrences in any of the regions
  # This is true where not all cells are nas
  some_spcd <- apply(vis_frame[2:ncol(vis_frame)], 1, function(x) any(!is.na((x))))
  vis_frame[some_spcd, ]

}

#' Creates the visualization for completed equations
#'
#'
#' 
test_config_plot <- function() {
  library(ggplot2)

  vis_frame <- test_config_sp()
  rast_frame <- gather(vis_frame, key = "Region", value = "Failing", 2:22)

  ggplot(data = rast_frame, aes(x = Region, y = spcd))+
    geom_tile(aes(fill = Failing)) +
    coord_fixed(ratio = 2)
}

#nrow(test_all_configs())
#a <- test_config('/home/bryce/Programming/forvol/csv/OR_E_config.csv')
#b <-test_config('/home/bryce/Programming/forvol/csv/OR_W_config.csv')

#sum(a$CF_VOL_EQ %in% b$CF_VOL_EQ)

## Example test:
##df_cvts <- BuildEquation('CU202020', GetCoefs('OR_W', 202))

##test_cvts(202, 'Western Oregon', df_cvts)

##wh_cvts <- BuildEquation('CU000077', GetCoefs('OR_W', 242))
##test_cvts(242, 'Western Oregon', wh_cvts)

##filter(test_data, SPCD == 242)

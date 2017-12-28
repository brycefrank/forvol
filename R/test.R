# Testing scripts to verify the existence, accuracy and functionality
# of cubic foot volume (top and stump) equations. Because these tests
# do not strictly expect equivalence, they are not considered a formal
# automated test (for now). This is mostly used for debugging and
# development purposes, with some opportunity for testing volume
# equation performance.
#

# Adjust some column names for consistency
test_data <- readRDS(system.file("data/cvts_test.rds", package = "forvol"))
test_data <- dplyr::mutate(test_data, CVTS = VOLTSGRS)

eq <- file.path(csv_path, "cvts_equations.csv")
eq <- utils::read.csv(eq)

#' Tests a specific CVTS function against the FIA dataset
#' (test_data) this is meant to be an internal function
#'
#' \code{test_eq} returns the average difference between
#' the new calculated volumes and the testing dataset.
#'
#' @param spcd The FIA species code
#' @param region_string The configuration ID specifying the region to test
#' @param new_cvts_func The new CVTS equation to test as an R expression
#' @return The average difference (float) mean(test_cvts - new_cvts)
#'
## TODO Fix the above example, which will require fixing the structure of test
## eq anyway...which is bad
#' @export
test_eq <- function(region, spcd, new_cvts_func) {
  # Create a testing data frame for the specified region, spcd and function
  # TODO Convert this chunk to base R, or at least reduce the tidyr clutter a bit
  test_df <- tidyr::filter(test_data, test_data$SPCD == spcd,
                           test_data$config_code == region)
  test_df <- tidyr::select(test_df$DO_BH, test_df$HT_TOT, test_df$CVTS)
  test_df <- tidyr::rename(dbh = test_df$DO_BH, ht = test_df$HT_TOT)
  test_df <- tidyr::na.omit(test_df)

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
#' @export
test_config <- function(config_id) {
  ## Read in configuration csv as dataframe
  config_path <- forvol::find_CSV(config_id)
  config <- utils::read.csv(config_path)

  ## Find unique equations that are missing
  missing <- config$CF_VOL_EQ[!(config$CF_VOL_EQ %in% eq$CF_VOL_EQ)]
  missing <- unique(missing)

  ## Create dataframe
  error_frame <- data.frame(CF_VOL_EQ = missing,
                            Result = "Missing")

  return(error_frame)
}

#' @export
test_all_configs <- function() {
  ## Master dataframe
  all_config_errors <- data.frame()

  ## Get all config paths
  configs <- list.files(forvol::csv_path, pattern = "config", full.names = TRUE)

  for (config_id in configs) {
    all_config_errors <- rbind(all_config_errors, forvol::test_config(config_id))
  }

  return(unique(all_config_errors))
}

#' Configuration test for species instead of equations,
#' serves as a function to construct the equations completion
#' status figure in the readme TODO
#'
#' @export
test_config_sp <- function(region) {
  test_config <- utils::read.csv(forvol::find_CSV(sprintf("^%s_config", region)))
  eqs <- mapply(forvol::build_equation, rep(region, nrow(test_config)), test_config$SPECIES_NUM)

  # Boolean, true if not returning a function
  eqs <- 'character' == sapply(eqs, typeof)

  join_frame <- data.frame(spcd = test_config$SPECIES_NUM)
  join_frame[sprintf('%s', region)] <- eqs

  return(join_frame)
}

#' @export
test_config_all_sp <- function(gather_frame = TRUE) {
  # Get region list

  regions <- utils::read.csv(forvol::find_CSV('regions'))

  # TODO replace list from 1 to 999 with actual SPCD codes
  spcd_unique <- as.character(utils::read.csv(forvol::find_CSV("ref_species_red"))$SPCD)
  vis_frame <- data.frame(spcd = spcd_unique)


  for (region in regions[[1]]) {
    #print(region)
    test_config <- utils::read.csv(forvol::find_CSV(sprintf("^%s_config", region)))

    # Attempt to build CVTS equation for all species
    eqs <- mapply(forvol::build_equation, rep(region, nrow(test_config)), test_config$SPECIES_NUM)

    # Boolean, true if not returning a function
    eqs <- 'character' == sapply(eqs, typeof)

    join_frame <- data.frame(spcd = test_config$SPECIES_NUM)
    join_frame[sprintf('%s', region)] <- eqs
    vis_frame <- merge(vis_frame, join_frame, all.x = TRUE)
  }

  if (gather_frame == FALSE) {
    return(vis_frame)
  }

  # Filter out rows that have no occurrences in any of the regions
  # This is true where not all cells are nas
  some_spcd <- apply(vis_frame[2:ncol(vis_frame)], 1, function(x) any(!is.na((x))))
  vis_frame[some_spcd, ]
}

#' Creates the visualization for completed equations
#'
#' @export
test_config_plot_all <- function() {
  vis_frame <- forvol::test_config_sp()
  rast_frame <- tidyr::gather(vis_frame, key = "Region", value = "Failing", 2:22)

  ggplot2::ggplot(data = rast_frame, ggplot2::aes(x = rast_frame$Region, y = rast_frame$spcd)) +
    ggplot2::geom_tile(ggplot2::aes(fill = rast_frame$Failing)) +
    ggplot2::coord_fixed(ratio = 2)
}


#' A convenient way to quickly list the failing species
#' for a given region
failing <- function(region) {
  test_config <- utils::read.csv(forvol::find_CSV(sprintf("^%s_config", region)))
  eqs <- mapply(forvol::build_equation, rep(region, nrow(test_config)), test_config$SPECIES_NUM)

  # Boolean, true if not returning a function
  eqs <- "character" == sapply(eqs, typeof)

  join_frame <- data.frame(spcd = test_config$SPECIES_NUM)
  join_frame[sprintf("%s", region)] <- eqs

  join_frame <- tidyr::gather(join_frame, key = "Region", value = "Failing", 2)
  join_frame <- tidyr::filter(join_frame, join_frame$Failing == TRUE)

  return(join_frame)
}

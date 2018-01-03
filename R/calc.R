# A place for calculation functions

#' Serves as the default volume calculation function,
#' retrieves the coefficient table for the specified species,
#' region and volume type, generates the function, and applies
#' it to the input data.
#'
#' @param dbh A column of diameter at breast height values (or a single value)
#' @param ht A column of height values (or a single value)
#' @param spcd FIA species code
#' @param region Geographic region
#' @export
calc_vol <- function(dbh, ht, spcd, region, vol_type) {
  # Get unique region and scd combinations
  uniques <- unique(data.frame(region, spcd))

  # Build the equations
  uniques$eqs <- mapply(forvol::build_equation, uniques$region, uniques$spcd, vol_type)

  # Split the data into spcd - region groups
  tree_data <- data.frame(dbh, ht, region, spcd)
  tree_split <- split(tree_data, list(tree_data$region, tree_data$spcd))

  new_tree <- data.frame()

  for (group in tree_split) {
    # Get the 'group key'
    region <- group$region[1]
    spcd <- group$spcd[1]

    # Get the equation from eqs
    eq <- uniques$eqs[which((uniques$spcd == spcd & uniques$region == region))]
    eq <- eq[[1]]

    # Apply the equation to each record in the group
    if(typeof(eq) == "closure") {
      group[vol_type] <- mapply(eq, group$dbh, group$ht)
    } else {
      group[vol_type] <- NA
    }

    # TODO: probably get rid of this
    new_tree <- rbind(new_tree, group[vol_type])
  }

  # Order based off of original index
  new_tree$index <- as.numeric(row.names(new_tree))
  return(new_tree[order(new_tree$index), ][vol_type])
}

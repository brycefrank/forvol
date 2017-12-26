# Internal functions used for 'helping'

#' @export
csv_path <- system.file("csv", package = "forvol")

#' Finds the csv with the input search term
#' Mostly for use as a debugging tool and internal function
#'
#' @param regexp Regex search term as string
#' @return The absolute path to the csv file
#' @export
find_CSV <- function(regexp) {
  # Add start of string control operator for regexp?
  csv <- list.files(csv_path, regexp, ignore.case = TRUE)

  return(file.path(csv_path, csv))
}

lookup_citation <- function(equation_id) {
  # TODO At some point this will need to incorporate more
  # than just the FIA config csv's as its scope, so it may
  # be worth making our own citation lookup csv. This is at
  # least a good start on that path

  # Find all equation configurations
  csv_list <- find_CSV('_eq.csv')

  csv_combined <- data.frame()
 
  for (csv in csv_list) {
    csv_rd <- read.csv(csv)
    csv_rd <- csv_rd[c(1, 3)]
    csv_combined <- rbind(csv_combined, csv_rd)
  }

  eq_src <- csv_combined$Source[which(csv_combined$CF_VOL_EQ == equation_id)]

  print(as.character(eq_src))
}

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


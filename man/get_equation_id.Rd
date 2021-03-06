% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq.R
\name{get_equation_id}
\alias{get_equation_id}
\title{Gets the equation string for a specified region
and species code. This is read directly from the 'cvts_equations.csv'}
\usage{
get_equation_id(region, spcd)
}
\arguments{
\item{region}{The geographic region of interest.}

\item{spcd}{The FIA species code.}
}
\value{
Returns the equation (as a string)
}
\description{
Gets the equation string for a specified region
and species code. This is read directly from the 'cvts_equations.csv'
}

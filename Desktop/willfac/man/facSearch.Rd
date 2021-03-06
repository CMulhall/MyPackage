% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/facTrac.R
\name{facSearch}
\alias{facSearch}
\title{FacTrac - a way to subset data}
\usage{
facSearch(a = "", b = 0)
}
\arguments{
\item{a}{- String that will be searched for. Defaults to the empty string which will
result in an empty dataset being returned.}

\item{b}{- Number (0, 1, or 2)}
}
\value{
Returns a the requested subset of the faculty data with a newly created age column.
}
\description{
Searches the faculty data set for a string that is either a full name, in
which case the second argument should be 1, or is the title of a department in which
case the second argument should be a 2. If you want the entire dataset, give the
second parameter as 0.
}
\examples{
facSearch()
}


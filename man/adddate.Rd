% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adddate.R
\name{adddate}
\alias{adddate}
\title{Inserts three columns (year,month,day) passing dates  to a matrix or to a dataframe}
\usage{
adddate(data, origin = "1961-1-1")
}
\arguments{
\item{data}{matrix of daily data}

\item{origin}{character string containing the date of the first row of \code{data} as YYYY-MM-DD}
}
\value{
a data frame with dates and \code{data} values
}
\description{
Inserts three columns (year,month,day) passing dates  to a matrix or to a dataframe
}
\seealso{
\code{\link{findDate}}
}

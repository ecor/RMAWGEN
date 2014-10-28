\name{removeNAs}
\alias{removeNAs}
\title{Replaces each entry of the rows containing NA values with NA}
\usage{
removeNAs(data)
}
\arguments{
  \item{data}{a matrix}
}
\value{
the matrix \code{data} with the modified rows of NA values
}
\description{
Replaces each entry of the rows containing NA values with
NA
}
\note{
In \code{\link{getVARmodel}}, when using \code{\link{VAR}}
or \code{\link{VARselect}}, all NAs will be removed
}
\author{
Emanuele Cordano, Emanuele Eccel
}
\seealso{
\code{\link{getVARmodel}}
}


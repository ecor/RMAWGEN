\name{WhereIs}
\alias{WhereIs}
\title{Gets the toponym where a meteorological station is located}
\usage{
WhereIs(name, station_names, location)
}
\arguments{
  \item{name}{character ID of the station}

  \item{station_names}{vector containing the IDs
  (characters) of the considered meteorological stations.
  An example is \code{STATION_NAMES} defined in the
  \code{\link{trentino}} dataset.}

  \item{location}{vector containing the toponyms. An
  example is \code{LOCATION} defined in the
  \code{\link{trentino}} dataset.}
}
\value{
the location toponym given the vectors of station IDs and
the respective location toponyms
}
\description{
Gets the toponym where a meteorological station is located
}
\examples{
data(trentino)
WhereIs("T0099",station_names=STATION_NAMES,location=LOCATION)
}
\author{
Emanuele Cordano, Emanuele Eccel
}


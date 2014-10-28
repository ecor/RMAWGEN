\name{TemperatureStartDay}
\alias{TemperatureStartDay}
\title{Gets the first day in a temperature time series, expressed as decimal julian days since 1970-1-1 00:00 UTC}
\usage{
TemperatureStartDay(name, station_names, start_day)
}
\arguments{
  \item{name}{character ID of the station}

  \item{station_names}{vector containing the IDs
  (characters) of the considered meteorological stations.
  An example is \code{STATION_NAMES} defined in the
  \code{\link{trentino}} dataset.}

  \item{start_day}{vector containing the temperature
  measurement start day. Default is
  \code{TEMPERATURE_MEASUREMENT_START_DAY} defined in the
  \code{\link{trentino}} dataset.}
}
\value{
the temperature measurement start day given the vectors of
station IDs and the respective temperature measurement
start days
}
\description{
Gets the first day in a temperature time series, expressed
as decimal julian days since 1970-1-1 00:00 UTC
}
\examples{
data(trentino)
TemperatureStartDay("T0099",station_names=STATION_NAMES,start_day=TEMPERATURE_MEASUREMENT_START_DAY)
}
\author{
Emanuele Cordano, Emanuele Eccel
}


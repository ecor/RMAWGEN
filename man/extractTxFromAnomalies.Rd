\name{extractTxFromAnomalies}
\alias{extractTxFromAnomalies}
\title{Extracts generated time series of Daily Maximum Temperature from a random multi-realization obtained by \code{\link{generateTemperatureTimeseries}} function}
\usage{
extractTxFromAnomalies(res_multigen, std, SplineAdv)
}
\arguments{
  \item{res_multigen}{matrix containing standardized values
  of daily temperature as returned by
  \code{\link{generateTemperatureTimeseries}} (first item)}

  \item{std}{vector containing standard deviation for each
  maximum temperature anomalies}

  \item{SplineAdv}{matrix containing the averaged values of
  maximum temperature obtained by a spline interpolation of
  monthly climate}
}
\value{
a matrix with generated maximum temperature
}
\description{
Extracts generated time series of Daily Maximum Temperature
from a random multi-realization obtained by
\code{\link{generateTemperatureTimeseries}} function
}
\author{
Emanuele Cordano, Emanuele Eccel
}


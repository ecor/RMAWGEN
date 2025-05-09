% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecastResidual.R
\name{forecastResidual}
\alias{forecastResidual}
\title{Forecasts the residual value of a VAR realization given the white noise covariance matrix}
\usage{
forecastResidual(var, xprev = NULL, B = NULL)
}
\arguments{
\item{var}{A VAR model represented by a \code{varest} object as returned by \code{\link{getVARmodel}} or \code{\link[vars]{VAR}}}

\item{xprev}{previous status of the random variable, in this case the "current instant"white-noise". Default is \code{NULL} and then randomly generated.}

\item{B}{matrix of coefficients for the vectorial white-noise component}
}
\value{
a vector of values
}
\description{
Forecasts the residual value of a VAR realization given the white noise covariance matrix
}
\seealso{
\code{\link{forecastEV}},\code{\link{NewVAReventRealization}}
}
\author{
Emanuele Cordano, Emanuele Eccel
}

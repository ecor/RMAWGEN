% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalizeGaussian.R
\name{normalizeGaussian}
\alias{normalizeGaussian}
\title{Converts a random variable \code{x} extracted by a population represented by the sample \code{data} or \code{sample}
 to a normally-distributed variable with assigned mean and standard deviation or vice versa in case \code{inverse} is \code{TRUE}}
\usage{
normalizeGaussian(
  x = 0,
  data = x,
  cpf = NULL,
  mean = 0,
  sd = 1,
  inverse = FALSE,
  step = NULL,
  prec = 10^-4,
  type = 3,
  extremes = TRUE,
  sample = NULL
)
}
\arguments{
\item{x}{value or vector of values to be converted}

\item{data}{a sample of data on which a non-parametric probability distribution is estimated}

\item{cpf}{cumulative probability distribution. If \code{NULL} (default) is calculated as \code{\link{ecdf}(data)}}

\item{mean}{mean (expected value) of the normalized random variable. Default is 0.}

\item{sd}{standard deviation of the normalized random variable. Default is 1.}

\item{inverse}{logical value. If \code{TRUE} the function works inversely (the opposite way). Default is \code{FALSE}.}

\item{step}{vector of values in which step discontinuities of the cumulative probability function occur. Default is \code{NULL}}

\item{prec}{amplitude of the neighbourhood of the step discontinuities where cumulative probability function is treated as non-continuous.}

\item{type}{see \code{\link{quantile}}}

\item{extremes}{logical variable.
If \code{TRUE} (default) the probability or frequency is multiplied by \deqn{\frac{N}{N+1}} where \eqn{N} is the length of \code{data}}

\item{sample}{a character string or \code{NULL} containing sample or probability distribution information.
Default is \code{NULL}}
}
\value{
the normalized variable or its inverse


 @note This function makes a Marginal Gaussianization. See the R code for further details
}
\description{
Converts a random variable \code{x} extracted by a population represented by the sample \code{data} or \code{sample}
 to a normally-distributed variable with assigned mean and standard deviation or vice versa in case \code{inverse} is \code{TRUE}
}
\author{
Emanuele Cordano, Emanuele Eccel
}

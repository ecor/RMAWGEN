\name{normalizeGaussian_severalstations_prec}
\alias{normalizeGaussian_severalstations_prec}
\title{DEPRECATED Converts several samples \code{x} random variable (daily precipitation values) extracted by populations represented by the columns of \code{data} respectively or \code{sample}
 to a normally-distributed samples with assinged mean and standard deviation or vice versa in case \code{inverse} is \code{TRUE} using the function \code{\link{normalizeGaussian_prec}}}
\usage{
normalizeGaussian_severalstations_prec(x, data = x, cpf = NULL, mean = 0,
  sd = 1, inverse = FALSE, qnull = NULL, valmin = 0.5, type = 3,
  extremes = TRUE, sample = NULL, origin_x = NULL, origin_data = NULL)
}
\arguments{
  \item{x}{value to be converted}

  \item{data}{a sample of data on which a non-parametric
  probability distribution is estimated}

  \item{cpf}{cumulative probability distribution. If
  \code{NULL} (default) is calculated as
  \code{\link{ecdf}(data)}}

  \item{mean}{mean (expected value) of the normalized
  random variable. Default is 0.}

  \item{sd}{standard deviation of the normalized random
  variable. Default is 1.}

  \item{inverse}{logical value. If \code{TRUE} the function
  works inversely (the opposite way). Default is
  \code{FALSE}.}

  \item{qnull}{probability of no precipitation occurence.
  (It can be a matrix in case \code{sample="monthly"}}

  \item{valmin}{minimum value of precipitation to consider
  a wet day}

  \item{type}{see \code{\link{quantile}}}

  \item{extremes}{logical variable.  If \code{TRUE}
  (default) the probability or frequency is multiplied by
  \deqn{\frac{N}{N+1}} where \eqn{N} is the length of
  \code{data}}

  \item{sample}{information about sample or probability
  distribution. Default is \code{NULL}}

  \item{origin_x}{date corresponding to the first row of
  \code{x}}

  \item{origin_data}{date corresponding to the first row of
  \code{data}}
}
\value{
a matrix or a data.frame with the normalized variable or
its inverse
}
\description{
DEPRECATED Converts several samples \code{x} random
variable (daily precipitation values) extracted by
populations represented by the columns of \code{data}
respectively or \code{sample} to a normally-distributed
samples with assinged mean and standard deviation or vice
versa in case \code{inverse} is \code{TRUE} using the
function \code{\link{normalizeGaussian_prec}}
}
\note{
In the version 1.2.5 of \pkg{RMAWGEN} This function is
deprecated and not used.
}
\author{
Emanuele Cordano, Emanuele Eccel
}
\seealso{
\code{\link{normalizeGaussian_prec}}
}


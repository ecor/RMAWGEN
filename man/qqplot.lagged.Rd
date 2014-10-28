\name{qqplot.lagged}
\alias{qqplot.lagged}
\title{This function creates a Q-Q plot of  the  \code{lag}-lag moving cumulative addition of the values in the samples \code{x,y,z}}
\usage{
qqplot.lagged(x = rnorm(1000), y = rnorm(1000), z = NULL,
  when = 1:length(x), lag = 1, pch = 1, ...)
}
\arguments{
  \item{x,y}{samples. If \code{x} is a data frame, \code{y}
  and \code{z} can be omitted.}

  \item{z}{further samples organized as a list}

  \item{when}{(integer) inidices of \code{x} and \code{y}
  on which the Q-Q plot is made.}

  \item{lag}{lag (current index included) on whose value
  the addition is made.}

  \item{pch}{a vector of plotting characters or symbols:
  see \code{\link{points}}}

  \item{...}{further arguments for \code{\link{qqplot}}}
}
\value{
the Q-Q plot
}
\description{
This function creates a Q-Q plot of the \code{lag}-lag
moving cumulative addition of the values in the samples
\code{x,y,z}
}
\seealso{
\code{\link{qqplot}}
}


\name{continuity_ratio}
\alias{continuity_ratio}
\title{Calculates the continuity ratio of a set of precipitation measured or generated data in several sites as defined by Wilks, 1998 (see reference link)}
\usage{
continuity_ratio(data, lag = 0, valmin = 0.5)
}
\arguments{
  \item{data}{containing daily precipitation time series
  for several gauges (one gauge time series per column)}

  \item{lag}{numeric lag (expressed as number of days) used
  for computation for "cross" continuity ratio and joint
  probability of prercipitation (no)occurence.}

  \item{valmin}{threshold precipitation value [mm] for
  wet/dry day indicator. If precipitation is lower than
  \code{valmin}, day is considered dry. Default is 0.5 mm.}
}
\value{
A list containing the following matrices:

\code{continuity_ratio} : \code{lag}-day lagged continuity
ratio ,

\code{occurence} : joint probability of \code{lag}-day
lagged precipitation occurence

\code{nooccurence} : joint probability of \code{lag}-day
lagged no precipitation occurence.

\code{noccurence_occurence} : joint probability of
\code{lag}-day lagged no precipitation and precipitation
occurence respectively.

\code{ccurence_nooccurence} : joint probability of
\code{lag}-day lagged precipitation and no precipitation
occurence respectively.
}
\description{
Calculates the continuity ratio of a set of precipitation
measured or generated data in several sites as defined by
Wilks, 1998 (see reference link)
}
\note{
If \code{lag==0} the function returns the continuity ratio
and joint probability as described by Wilks, 1998.
Otherwise the precipitation values for each couple of rain
gauges are taken with \code{lag}-day lag.
}
\author{
Emanuele Cordano, Emanuele Eccel
}
\references{
see the following URL references:
\url{http://onlinelibrary.wiley.com/doi/10.1002/joc.2305/abstract}
and
\url{http://www.sciencedirect.com/science/article/pii/S0022169498001863}
}


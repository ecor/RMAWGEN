\name{ComprehensivePrecipitationGenerator}
\alias{ComprehensivePrecipitationGenerator}
\title{The comprehensive Precipitation Generator}
\usage{
ComprehensivePrecipitationGenerator(station = c("T0001", "T0010", "T0099"),
  prec_all, mean_climate_prec = NULL, year_max = 1990, year_min = 1961,
  leap = TRUE, nmonth = 12, cpf = NULL, verbose = TRUE, p = 1,
  type = "none", lag.max = NULL, ic = "AIC", activateVARselect = FALSE,
  exogen = NULL, exogen_sim = NULL, is_exogen_gaussian = FALSE,
  year_max_sim = year_max, year_min_sim = year_min,
  mean_climate_prec_sim = NULL, onlygeneration = FALSE, varmodel = NULL,
  type_quantile = 3, qnull = NULL, valmin = 0.5, step = 0,
  n_GPCA_iteration = 0, n_GPCA_iteration_residuals = n_GPCA_iteration,
  sample = NULL, extremes = TRUE, exogen_all = NULL,
  exogen_all_col = station, no_spline = FALSE, nscenario = 1,
  seed = NULL, noise = NULL)
}
\arguments{
  \item{station}{character vector of the IDs of the
  considered meteorological stations}

  \item{prec_all}{data frame containing daily precipitation
  of all meteorological stations. See
  \code{\link{PRECIPITATION}} defined in the
  \code{\link{trentino}} dataset for formatting.}

  \item{mean_climate_prec}{a matrix containing monthly mean
  daily precipitation for the considered station. If it is
  \code{NULL}, it is calculated. See input of
  \code{\link{is.monthly.climate}}}

  \item{year_max}{start year of the recorded (calibration)
  period}

  \item{year_min}{end year of the recorded (calibration)
  period}

  \item{leap}{logical variables. If it is \code{TRUE}
  (default)(recommended), leap years are considered,
  otherwise all years have 365 days}

  \item{nmonth}{number of months in one year (default is
  12)}

  \item{verbose}{logical variable}

  \item{cpf}{see
  \code{\link{normalizeGaussian_severalstations}}}

  \item{sample,extremes,qnull,valmin}{see
  \code{\link{normalizeGaussian_severalstations}}}

  \item{step}{see
  \code{\link{normalizeGaussian_severalstations}}. Default
  is 0.}

  \item{p,type,lag.max,ic,activateVARselect}{see respective
  input parameter on \code{\link{getVARmodel}}}

  \item{year_max_sim}{last year of the simulation period.
  Default is equal to \code{year_max}}

  \item{year_min_sim}{first year of the simulation period.
  Default is equal to \code{year_min}}

  \item{mean_climate_prec_sim}{a matrix containing monthly
  mean daily precipitation for the simulation period. If is
  \code{NULL} (Default), it is set equal to
  \code{mean_climate_prec}.}

  \item{n_GPCA_iteration}{number of iterations of
  Gaussianization process for data. Default is 0 (no
  Gaussianization)}

  \item{n_GPCA_iteration_residuals}{number of iterations of
  Gaussianization process for VAR residuals. Default is 0
  (no Gaussianization)}

  \item{exogen}{data frame or matrix containing the
  (normalized or not) exogenous variables (predictors) for
  the recorded (calibration) period.}

  \item{exogen_sim}{data frame or matrix containing the
  (normalized or not) exogenous variables (predictors) for
  the simulation period. Default is \code{NULL}. If it is
  \code{NULL}, it is replaced with \code{exogen} within the
  function.}

  \item{is_exogen_gaussian}{logical value. If \code{TRUE},
  \code{exogen_sim} and \code{exogen} are given as already
  normalized variables, otherwhise they are not normalized.
  Default is \code{FALSE}}

  \item{onlygeneration}{logical value. If \code{TRUE} the
  VAR model \code{varmodel} is given as input and only
  random generation is done, otherwise (default) is
  calculated from measured data}

  \item{varmodel}{the comprehensinve VAR model as a
  \code{\link{varest2}} S4 object or a \code{NULL} object.
  If \code{NULL} (default), the comprehensinve VAR is
  estimated from measured data within the function,
  otherwise it is given as input and only random generation
  is done.}

  \item{type_quantile}{see \code{type} on
  \code{\link{quantile}}}

  \item{exogen_all}{data frame containing exogenous
  variable formatted like \code{prec_all}. Default is
  \code{NULL}. It is alternative to \code{exogen} and if it
  not \code{NULL},\code{is_exogen_gaussian} is
  automatically set \code{FALSE}}

  \item{exogen_all_col}{vector of considered columns of
  \code{exogen_all}. Default is \code{station}.}

  \item{no_spline}{logical value. See
  \code{\link{splineInterpolateMonthlytoDailyforSeveralYears}}.
  Default is \code{TRUE}.}

  \item{nscenario}{number of generated scenarios for daily
  maximum and minimum temperature}

  \item{seed}{seed for stochastic random generation see
  \code{\link{set.seed}}.}

  \item{noise}{stochastic noise to add for variabile
  generation. Default is \code{NULL}. See
  \code{\link{newVARmultieventRealization}}. Not used in
  case that \code{nscenario>1}.}
}
\value{
A list of the following variables:

\code{prec_mes} matrix containing measured daily
precipitation (the data is copied by the measured data
given as input for the period and the station considered
for \code{varmodel} estimation)

\code{prec_spline} matrix containing climatic
"spline-interpolated" daily preciptation from
\code{mean_climate_prec}

\code{data_prec} matrix containing normalized measured
precipitation variable

\code{prec_gen} matrix containing generated daily
precipitation [mm]

\code{prec_spline_sim} matrix containing climatic
"spline-interpolated" daily preciptation from
\code{mean_climate_prec_sim}

\code{data_prec_gen} matrix containing normalized generated
precipitation variable

\code{mean_climate_prec} matrix containing monthly means of
daily precipitation (historical scenario)

\code{mean_climate_prec_sim} matrix containing monthly
means of daily precipitation (predicted/simulated scenario)

\code{var} a varest object containing the used VAR model
}
\description{
The comprehensive Precipitation Generator
}
\note{
It pre-processes and generates a multi-site precipitation
fields. It uses \code{\link{getVARmodel}}. Detailed
examples can be viewed of this function in
\href{https://docs.google.com/file/d/0B8xDtMCnW3dJU2JIemVqMnpKTHc/edit}{this
presentation}. Unfortunately, using this approach, the
spatial correlations are underestimated. This is due to the
persinstence of zeros in the precipitation records. This
problem is known in literature and can be solved in the
future versions of RMAWGEN. See the R code for further
details
}
\examples{
data(trentino)
set.seed(1222) # set the seed for random generations!
year_max <- 1990
year_min <- 1961
year_max_sim <- 1982
year_min_sim <- 1981

n_GPCA_iter <- 2
p <- 1
nscenario=1
station <- c("T0090","T0083")
## Not Run: the call to ComprehensivePrecipitationGenerator may elapse too
## long time (more than 5 eseconds) and is not executed  by CRAN check.
## Please uncomment the following line to run the example on your own PC.
# generation00 <- ComprehensivePrecipitationGenerator(station=station,
# prec_all=PRECIPITATION,year_min=year_min,year_max=year_max,
# year_min_sim=year_min_sim,year_max_sim=year_max_sim,p=p,
# n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=0,
# sample="monthly",nscenario=nscenario,no_spline=TRUE)

#
}
\author{
Emanuele Cordano, Emanuele Eccel
}
\seealso{
\code{\link{splineInterpolateMonthlytoDailyforSeveralYears}}
}


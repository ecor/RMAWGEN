
# Author: ecor
###############################################################################


#'
#' Multi-site autoregressive Models for Daily Weather Generation. The modeling in climate change applications for agricultural or hydrological purposes often requires daily time-series of precipitation and temperature.
#' This is the case of downscaled series from monthly or seasonal predictions of Global Climate Models (GCMs).  The R package RMAWGEN (R Multi-Sites Auto
#' regressive Weather GENerator) is built to generate daily temperature and precipitation time series in several sites by using the theory of vectorial
#' autoregressive models (VAR). The VAR model is used because it is able to maintain the temporal and spatial correlations among  the several series. In
#' particular, observed time series of daily maximum and minimum temperature and precipitation are used to calibrate the parameters of a VAR model (saved
#' as ''GPCAvarest2'' or ''varest2'' classes, which inherit the "varest" S3 class defined in the package vars [Pfaff, 2008]). Therefore the VAR model, coupled
#' with monthly mean weather variables downscaled by GCM predictions, allows to generate several stochastic daily scenarios. The structure of the package
#' consists in functions that transform precipitation and temperature time series into Gaussian-distributed random variables through deseasonalization and
#' Principal Component Analysis. Then a VAR model is calibrated on transformed time series. The time series generated by VAR are then inversely re
#' transformed into precipitation and/or temperature series. An application dateset is included in the RMAWGEN package as an example; it is presented by
#' using a dataset with daily weather time series recorded in 59 different sites of Trentino (Italy) and its neighborhoods for the period 1958-2007. The
#' software is distributed as a Free Software with General Public License (GPL) and is available on CRAN and Github.
#' A presentation of the package is available on \url{https://docs.google.com/file/d/0B66otCUk3Bv6V3RPbm1mUG4zVHc/edit}.
#' Example script files about package usage are available on \url{https://github.com/ecor/RMAWGENCodeCorner}.

#'
#' \tabular{ll}{
#' Package: \tab RMAWGEN\cr
#' Type: \tab Package\cr
#' Version: \tab 1.3.6\cr
#' Date: \tab   2019-11-13\cr
#' License: \tab GPL (>= 2)
#'

#'
#' \cr
#' LazyLoad: \tab yes\cr
#' Depends: R(>=2.12),time,chron,vars \cr
#' }
#'
#'
#'
#'
#'
#'
#'
#' @name RMAWGEN-package
#' @aliases RMAWGEN

#' @title R - Multi-site Autoregressive WEather Generator
#' @author Emanuele Cordano \email{emanuele.cordano@@gmail.org}, Emanuele Eccel \email{emanuele.eccel@@fmach.it}
#' @references
#' Cordano E. and Eccel E. (2016), Tools for stochastic weather series generation in R environment, Italian Journal of Agrometeorology \doi{10.19199/2016.3.2038-5625.031}
#'
#' Pfaff B. (2008). VAR, SVAR and SVEC Models: Implementation Within R Package vars. Journal of Statistical Software 27(4). \url{https://www.jstatsoft.org/v27/i04/}(doi:10.18637/jss.v027.i04)
#'
#' @note First release of RMAWGEN was created in the frame of ACE-SAP and ENVIROCHANGE  projects
#' funded by Provincia Autonoma di Trento, Italy.
#'
#' RMAWGEN is free software: you can redistribute it and/or modify
#'    it under the terms of the GNU General Public License as published by
#'    the Free Software Foundation, either version 3 of the License, or
#'    (at your option) any later version.
#'
#'    RMAWGEN is distributed in the hope that it will be useful,
#'    but WITHOUT ANY WARRANTY; without even the implied warranty of
#'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#'    GNU General Public License for more details.
#'
 #'    You should have received a copy of the GNU General Public License
 #'    along with this program.  If not, see \url{http://www.gnu.org/licenses/}.
#'
#' @keywords package vector auto-regression models  temperature precipitation time-series
# @seealso \code{\link{is.pseudoprime}}
# @examples data(trentino)
#
NULL


###### @docType package https://doi.org/10.19199/2016.3.2038-5625.031

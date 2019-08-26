\name{trentino}
\docType{data}
\alias{trentino}
\alias{ELEVATION}
\alias{LOCATION}
\alias{PRECIPITATION} 
\alias{PRECIPITATION_MEASUREMENT_END_DAY}
\alias{PRECIPITATION_MEASUREMENT_START_DAY} 
\alias{STATION_LATLON}
\alias{STATION_NAMES}
\alias{TEMPERATURE_MAX} 
\alias{TEMPERATURE_MEASUREMENT_END_DAY}
\alias{TEMPERATURE_MEASUREMENT_START_DAY} 
\alias{TEMPERATURE_MIN} 

\title{
Trentino Dataset 
}
\usage{
data(trentino)
}

\description{
It contains the following variables: 
\describe{
\item{\code{TEMPERATURE_MIN}}{Data frame containing \code{year},\code{month} , \code{day} and daily minimum temperature in 59 stations in Trentino region}
\cr
\item{\code{TEMPERATURE_MAX}}{Data frame containing \code{year},\code{month} , \code{day} and daily maximum temperature in 59 stations in Trentino region}                 
\cr
\item{\code{PRECIPITATION}}{Data frame containing \code{year},\code{month} , \code{day} and daily precipitation in 59 stations in Trentino region}                 
\cr
\item{\code{STATION_NAMES}}{Vector containing the names of the meteorological stations}                      
\cr
\item{\code{ELEVATION}}{Vector containing the elevations of the meteorological stations respectively}      
\cr
\item{\code{STATION_LATLON}}{Matrix containing the latitude and longitude coordinates, respectively, of the meteorological stations}                    
\cr
\item{\code{LOCATION}}{Vector containing the names of the location of each meteorological station}    
\cr
\item{\code{TEMPERATURE_MEASUREMENT_START_DAY}}{Vector containing the first days referred to midday (expressed as decimal julian day since 1970-1-1 00:00 UTC) of temperature measurement of each meteorological station}
\cr
\item{\code{TEMPERATURE_MEASUREMENT_END_DAY}}{Vector containing the last days referred to midday (expressed as decimal julian day since 1-1-1970 00:00 UTC) of temperature measurement of each meteorological station}   
\cr
\item{\code{PRECIPITATION_MEASUREMENT_START_DAY}}{Vector containing the first days referred to midday (expressed as decimal julian day since 1-1-1970 00:00 UTC) of precipitation measurement of each meteorological station}
\cr
\item{\code{PRECIPITATION_MEASUREMENT_END_DAY}}{Vector containing the last days referred to midday (expressed as decimal julian day since 1-1-1970) of precipitation measurement of each meteorological station}  
}
}
\details{
This dataset stores all information about meteorological stations and instrumental timeseries.
The user can easily use the package with his/her own data after replacing the values of such variables.
}
\format{
Data frames and vectors
}
\source{
Original data are provided by Provincia Autonoma di Trento (\url{http://www.meteotrentino.it/}), Fondazione Edmund Mach (\url{www.fmach.it}),
Provincia Autonama di Bolzano/Autome Provinz Bozen (\url{http://www.provincia.bz.it/meteo}),
ARPA Lombardia (\url{www.arpalombardia.it/}), ARPA Veneto (\url{www.arpa.veneto.it/meteo.htm}).

This dataset is intended for research purposes only, being distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY.
}

%%\references{
%% ~put references to the literature/web site here ~
%%}
\keyword{dataset}

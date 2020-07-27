## 
## Multi-scenario generation of pracipitation
##
## Author: Emanuele Cordano
## Date: 2020 07 27
##

rm(list=ls())
library(RMAWGEN)
data(trentino)
set.seed(1222) # set the seed for random generations!
year_max <- 1990
year_min <- 1961
year_max_sim <- 1982
year_min_sim <- 1981
n_GPCA_iter <- 5 
n_GPCA_iteration_residuals <- 5
p <- 3
nscenario=3
station <- c("T0090","T0083")

generation00 <-ComprehensiveTemperatureGenerator(station=station,
Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,
 p=p,n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,
 sample="monthly",year_min_sim=year_min_sim,year_max_sim=year_max_sim,nscenario=nscenario)



summary(generation00)
#Length Class       Mode
#input     19     -none-      list
#var        1     GPCAvarest2 S4  
#output     6     -none-      list
#temporary  4     data.frame  list




nrow(generation00$output$Tx_gen)
#[1] 10957



nrow(generation00$output$Tx_gen00001)
#[1] 730

nrow(generation00$output$Tx_gen00002)
#[1] 730

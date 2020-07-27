## 
## Multi-scenario generation of pracipitation
##
## Authors: Douglas M. Hultstrand (hultstrand@appliedweatherassociates.com),Emanuele Cordano
##

library(RMAWGEN)
data(trentino)
set.seed(1222) # set the seed for random generations!
year_max <- 1990
year_min <- 1961
year_max_sim <- 1982
year_min_sim <- 1981
n_GPCA_iter <- 2
p <- 1
nscenario=2
station <- c("T0090","T0083")

generation00 <- ComprehensivePrecipitationGenerator(station=station,
 prec_all=PRECIPITATION,year_min=year_min,year_max=year_max,
 year_min_sim=year_min_sim,year_max_sim=year_max_sim,p=p,
 n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=0,
 sample="monthly",nscenario=nscenario,no_spline=TRUE)


summary(generation00)
#                      Length Class       Mode   
#prec_mes                 2   data.frame  list   
#prec_spline              2   data.frame  list   
#data_prec                2   data.frame  list   
#prec_gen                 2   data.frame  list   
#prec_spline_sim          2   data.frame  list   
#data_prec_gen         1460   -none-      numeric
#mean_climate_prec        2   data.frame  list   
#mean_climate_prec_sim    2   data.frame  list   
#var                      1   GPCAvarest2 S4     
#prec_gen00001            2   data.frame  list   
#prec_gen00002            2   data.frame  list 


nrow(generation00$prec_mes)
#[1] 10957

nrow(generation00$prec_gen)
#[1] 730

nrow(generation00$prec_gen00001)
#[1] 730

nrow(generation00$prec_gen00002)
#[1] 730

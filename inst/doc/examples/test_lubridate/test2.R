rm(list=ls())

library(RMAWGEN)
library(lubridate)
source('/STORAGE/projects/R-Packages/RMAWGEN/R/plot.sample.R')
data(trentino)
plot_sample(x=TEMPERATURE_MIN$T0090,sample="monthly",
  origin="1958-1-1",axes=FALSE,xlab="Tn [ degC]",
  ylab="x")
 
set.seed(123456) 
z <- rexp(10000,rate=0.5) 
x <- normalizeGaussian(x=z,data=z) 
plot_sample(x=z,xlab="z",ylab="x")

rm(list=ls())

library(RMAWGEN) 

##source('/STORAGE/projects/R-Packages/RMAWGEN/R/mmonths.R') 

library(lubridate)
set.seed(1234)

 N <- 30
x <- rexp(N)
y <- x+rnorm(N)
df <- data.frame(x=x,y=y)
 
dfg <- normalizeGaussian_severalstations(df,data=df,extremes=TRUE,inverse=FALSE)
 
dfi <- normalizeGaussian_severalstations(dfg,data=df,extremes=TRUE,inverse=TRUE)
 
N <- 365*2
origin <- "1981-01-01"
x <- rexp(N)
y <- x+rnorm(N)
df <- data.frame(x=x,y=y)
 
dfgm <- normalizeGaussian_severalstations(df,data=df,extremes=TRUE,inverse=FALSE,origin_x=origin,origin_data=origin,sample="monthly")
 
dfim <- normalizeGaussian_severalstations(dfg,data=df,extremes=TRUE,inverse=TRUE,origin_x=origin,origin_data=origin,sample="monthly")
  
  
 
rm(list=ls())
## TEMPERATURE

##https://en.wikipedia.org/wiki/Power_transform
library(RMAWGEN)
library(lubridate)
library(magrittr)
library(dplyr)
library(reshape2)
library(xts)
####
data(trentino)

###
df <- TEMPERATURE_MAX
time0 <- paste(df$year,df$month,df$day,sep="_") %>% as.Date(format="%Y_%m_%d")

## DAILY TIME SERIES (ZOO : TIME-DOMAIN)
dfzoo <- df %>% dplyr::select(-year,-month,-day) %>% as.zoo()
index(dfzoo) <- time0
dfzoo <- dfzoo %>% xts(frequency = 12)
####
plot(dfzoo$T0179)


####
#MONTHLY AGGREGATION
dfzoo_m <- df %>% mutate(time=time0) %>% select(-day) %>% melt(id=c("month","year","time")) %>% group_by(month,year,variable) %>% summarize(time=time[1],value=mean(value,na.rm=TRUE)) %>% ungroup()##select(-day) %>% group_by(month,year) %>%
dfzoo_m2 <- dfzoo_m %>% dcast(time ~ variable)
time1 <- dfzoo_m2$time
dfzoo_mon <- dfzoo_m2 %>% select(-time) %>% as.zoo()
index(dfzoo_mon) <- time1
###
temp00x <- dfzoo_mon$T0179 %>% ts(frequency=12)## paganalla?
## celsius to kelvin
#temp00x <- temp00x+273.15

plot(temp00x)
temp00x.decomp <- decompose(temp00x, type = "additive")
plot(temp00x.decomp)
###
###https://www.codingprof.com/3-easy-ways-to-test-for-heteroscedasticity-in-r-examples/
####https://www.scirp.org/(S(lz5mqp453edsnp55rrgjct55.))/journal/paperinformation.aspx?paperid=28972

### https://www.ebay.it/itm/165311730327?chn=ps&norover=1&mkevt=1&mkrid=724-128315-5854-1&mkcid=2&itemid=165311730327&targetid=1275486831783&device=c&mktype=pla&googleloc=20608&poi=&campaignid=9556814099&mkgroupid=124191510602&rlsatarget=pla-1275486831783&abcId=1145978&merchantid=116436416&gclid=Cj0KCQjw2_OWBhDqARIsAAUNTTFamrud9tfKKe2Xghtq7Nw-X4EOAn6TS-1mAhFAXzIv_UXONNUacxcaAk9XEALw_wcB#shpCntId

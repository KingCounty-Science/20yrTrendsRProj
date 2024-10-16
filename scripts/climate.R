################### OPeNDAP EXAMPLE SCRIPT: SIMPLE ########################################
##### FILENAME: OPeNDAPExample_TimeSeries_macav2livneh_SimpleExample.r   ##################
##### PURPOSE: THIS SCRIPT PULLS ONE POINT AND ALL TIME STEPS FROM A MACA DATA FILE #######
##### AUTHOR: HEATHER DINON ALDRIDGE (hadinon@ncsu.edu) ###################################
##### UPDATED: FEBRUARY 5, 2015                      ######################################
##### THIS SCRIPT IS RUN USING R version 3.0.1 (2013-05-16) ###############################
##### FOR MORE INFORMATION ON THE ncdf4 R PACKAGE, SEE: ###################################
##### http://cran.r-project.org/web/packages/ncdf4/ncdf4.pdf ##############################
##### ANOTHER WAY TO ACCESS DATA USING OPeNDAP CAN BE FOUND HERE: #########################
##### http://lukemiller.org/index.php/2011/02/accessing-noaa-tide-data-with-r/ ############
###########################################################################################

####### modified by Beth Sosik to suit analysis of climate variables against B-IBI data
setwd(here::here())
setwd("./scripts")
library(sf)
library(lubridate)
library(data.table)
library(stringr)
library(TITAN2)
gdb.path<-'//kc.kingcounty.lcl/dnrp/WLRD/STS/GreenWQA/Biota/GIS/KC_basins'
source('biostats.R', encoding = 'UTF-8')
setwd(here::here())
setwd("./outputs")
sites <- st_read(dsn=paste0(gdb.path, '/KC_basin_pourpoints.shp'))
st_crs(sites)

sites<-st_transform(sites, 4326)##project the sites to the same CRS as the gridMET data
sites$lat<-st_coordinates(sites)[,2]
sites$long<-st_coordinates(sites)[,1]
site_lat<-data.table(Value=c(sites$lat), site=c(sites$Name))
site_long<-data.table(Value=c(sites$long), site=c(sites$Name))
site_lat[,merge:=Value]
site_long[,merge:=Value]
setkeyv(site_lat,c('merge'))
setkeyv(site_long,c('merge'))

##Tmean is just the average of daily tmin and tmax. Use this to calculate growing degree days above 32F for each sampling event.

## LOAD THE REQUIRED LIBRARY
library(ncdf4)

### DEFINE THE URL

##http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html##
# urltotal<-"http://convection.meas.ncsu.edu:8080/thredds/dodsC/pinemap/maca/past/macav2livneh_pr_bcc-csm1-1-m_historical_1970_1989_CONUS.nc"
# urltotal<-"http://thredds.northwestknowledge.net:8080/thredds/dodsC/macav2livneh_huss_BNU-ESM_r1i1p1_historical_1950_2005_CONUS_daily_aggregated.nc"
##http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html
urltotalmax<-"http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_tmmx_1979_CurrentYear_CONUS.nc"
urltotalmiin<-"http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_tmmn_1979_CurrentYear_CONUS.nc"
urlprecip<-"http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc"
# urltotal<-"http://convection.meas.ncsu.edu:8080/thredds/dodsC/prism/daily/combo/2006/PRISM_combo_20061208.nc"

#/thredds/dodsC/agg_met_tmmx_1979_CurrentYear_CONUS.nc
## OPEN THE FILE
# nc <- nc_open(urltotal)
ncmax <- nc_open(urltotalmax)
ncmin <- nc_open(urltotalmiin)
ncpr <- nc_open(urlprecip)
## SHOW SOME METADATA 
# nc
ncmax
ncmin

## DISPLAY INFORMATION ABOUT AN ATTRIBUTE
# ncatt_get(nc,"tmean")
ncatt_get(ncmax,"daily_maximum_temperature")
ncatt_get(ncmin,"daily_minimum_temperature")
ncatt_get(ncpr,"precipitation_amount")

## GET DATA SIZES: http://www.inside-r.org/packages/cran/ncdf4/docs/ncvar_get
## NOTE: FILE DIMENSIONS ARE lon,lat,time
# v3 <- nc$var[[1]]
# lonsize <- v3$varsize[1]
# latsize <- v3$varsize[2] 
# endcount <- v3$varsize[3] 

v4 <- ncmax$var[[1]]
lonsize4 <- v4$varsize[1]
latsize4 <- v4$varsize[2]
endcount4 <- v4$varsize[3]

v4 <- ncpr$var[[1]]
lonsize4 <- v4$varsize[1]
latsize4 <- v4$varsize[2]
endcount4 <- v4$varsize[3]

# v5 <- ncmin$var[[1]]
# lonsize5 <- v5$varsize[1]
# latsize5 <- v5$varsize[2] 
# endcount5 <- v5$varsize[3]

### DEFINE OUR POINT OF INTEREST 
## NOTE: MAKE SURE TO CHECK WHETHER YOUR SOURCE STARTS COUNTING AT 0 OR 1
## e.g. ncdf4 PACKAGE STARTS COUNTING AT 1 BUT OPeNDAP DATASET ACCESS FORM STARTS AT 0:
##these numbers correspond to the index numbers of the array that stores the coordinates. To match to sample points, need to find array position of nearest given coordinate.
##Find grid points nearest to sampling sites

gridmet_lat<-data.table(Value=c(v4$dim[[2]]$vals), index=c(1:length(v4$dim[[2]]$vals)))
gridmet_long<-data.table(Value=c(v4$dim[[1]]$vals), index=c(1:length(v4$dim[[1]]$vals)))
gridmet_lat[,merge:=Value]
gridmet_long[,merge:=Value]
setkeyv(gridmet_lat,c('merge'))
setkeyv(gridmet_long,c('merge'))
merge_lat<-gridmet_lat[site_lat, roll='nearest']
merge_long<-gridmet_long[site_long, roll='nearest']

latlongmatch<-merge(merge_long, merge_lat,  by="site")
lon=c(latlongmatch$index.x)
lat=c(latlongmatch$index.y)

# lon=70
# lat=41

## DEFINE OUR VARIABLE NAME 
# var="tmin"

wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

DD1<-data.frame(time=Date(), data.x=numeric(), data.y=numeric(), tmean=numeric(), x32=numeric(), wtr_year=character(), Year=character(), DD=numeric(), Site=character())

i<-1
library(plyr)
for (i in 1:nrow(latlongmatch)){
  
var="daily_maximum_temperature"

## READ THE DATA VARIABLE (e.g. precipitation IN THIS CASE): http://www.inside-r.org/packages/cran/ncdf4/docs/ncvar_get 
## AND http://stackoverflow.com/questions/19936432/faster-reading-of-time-series-from-netcdf
## ORDER OF start= AND count= IS BASED ON ORDER IN BRACKETS AFTER VARIABLE NAME (SHOWN WHEN DISPLAYING YOUR METADATA)
## FROM THE DOCUMENTATION... "If [start] not specified, reading starts at the beginning of the file (1,1,1,...)."
## AND "If [count] not specified and the variable does NOT have an unlimited dimension, the entire variable is read. 
## As a special case, the value "-1" indicates that all entries along that dimension should be read."
data <- ncvar_get(ncmax, var, start=c(lon[i],lat[i],1),count=c(1,1,-1))
## READ THE TIME VARIABLE
time <- ncvar_get(ncmax, "day", start=c(1),count=c(-1))
## CONVERT TIME FROM "days since 1900-01-01" TO YYYY-MM-DD
time=as.Date(time, origin="1900-01-01") ##note: assumes leap years! http://stat.ethz.ch/R-manual/R-patched/library/base/html/as.Date.html
# PUT EVERYTHING INTO A DATA FRAME
cmax <- data.frame(time,data)

var="daily_minimum_temperature"
data <- ncvar_get(ncmin, var, start=c(lon[i],lat[i],1),count=c(1,1,-1))
## READ THE TIME VARIABLE
time <- ncvar_get(ncmin, "day", start=c(1),count=c(-1))
## CONVERT TIME FROM "days since 1900-01-01" TO YYYY-MM-DD
time=as.Date(time, origin="1900-01-01") ##note: assumes leap years! http://stat.ethz.ch/R-manual/R-patched/library/base/html/as.Date.html
# PUT EVERYTHING INTO A DATA FRAME
cmin <- data.frame(time,data)

c<-merge(cmax, cmin, by="time")
c<-subset(c, time>"2001-01-01")
c$tmean<-(c$data.x+c$data.y)/2
c$x32<-c$tmean-273.15
c$wtr_year<-wtr_yr(c$time, start_month=10)
c$Year<-format(as.Date(c$time, '%Y/%m/%d'), "%Y")
c[c$x32>0,]$x32<-0 #<= for DD above 32, > for DD below 32
DD<-ddply(c, .(Year), mutate, DD=cumsum(x32)) ##use this for heat days abov 32 since Jan 1st
# DD<-ddply(c, .(wtr_year), mutate, DD=cumsum(x32)) ## use this for freeze days below 32 since start of water year
DD$Site<-latlongmatch$site[i]
DD1<-rbind(DD, DD1)
}
saveRDS(DD1, "DegreeDays_below32F.rds")

 DD1<-data.frame(time=Date(), data.x=numeric(), data.y=numeric(), tmean=numeric(), x32=numeric(), wtr_year=character(), Year=character(), DD=numeric(), Site=character())
 for (i in 1:nrow(latlongmatch)){
     
     var="daily_maximum_temperature"
     
       ## READ THE DATA VARIABLE (e.g. precipitation IN THIS CASE): http://www.inside-r.org/packages/cran/ncdf4/docs/ncvar_get 
       ## AND http://stackoverflow.com/questions/19936432/faster-reading-of-time-series-from-netcdf
       ## ORDER OF start= AND count= IS BASED ON ORDER IN BRACKETS AFTER VARIABLE NAME (SHOWN WHEN DISPLAYING YOUR METADATA)
       ## FROM THE DOCUMENTATION... "If [start] not specified, reading starts at the beginning of the file (1,1,1,...)."
       ## AND "If [count] not specified and the variable does NOT have an unlimited dimension, the entire variable is read. 
       ## As a special case, the value "-1" indicates that all entries along that dimension should be read."
       data <- ncvar_get(ncmax, var, start=c(lon[i],lat[i],1),count=c(1,1,-1))
       ## READ THE TIME VARIABLE
         time <- ncvar_get(ncmax, "day", start=c(1),count=c(-1))
         ## CONVERT TIME FROM "days since 1900-01-01" TO YYYY-MM-DD
           time=as.Date(time, origin="1900-01-01") ##note: assumes leap years! http://stat.ethz.ch/R-manual/R-patched/library/base/html/as.Date.html
           # PUT EVERYTHING INTO A DATA FRAME
             cmax <- data.frame(time,data)
             
               var="daily_minimum_temperature"
               data <- ncvar_get(ncmin, var, start=c(lon[i],lat[i],1),count=c(1,1,-1))
               ## READ THE TIME VARIABLE
                 time <- ncvar_get(ncmin, "day", start=c(1),count=c(-1))
                 ## CONVERT TIME FROM "days since 1900-01-01" TO YYYY-MM-DD
                   time=as.Date(time, origin="1900-01-01") ##note: assumes leap years! http://stat.ethz.ch/R-manual/R-patched/library/base/html/as.Date.html
                   # PUT EVERYTHING INTO A DATA FRAME
                     cmin <- data.frame(time,data)
                     
                       c<-merge(cmax, cmin, by="time")
                       c<-subset(c, time>"2001-01-01")
                       c$tmean<-(c$data.x+c$data.y)/2
                       c$x32<-c$tmean-273.15
                       c$wtr_year<-wtr_yr(c$time, start_month=10)
                       c$Year<-format(as.Date(c$time, '%Y/%m/%d'), "%Y")
                       c[c$x32<=0,]$x32<-0 #<= for DD above 32, > for DD below 32
                       DD<-ddply(c, .(Year), mutate, DD=cumsum(x32)) ##use this for heat days abov 32 since Jan 1st
                       # DD<-ddply(c, .(wtr_year), mutate, DD=cumsum(x32)) ## use this for freeze days below 32 since start of water year
                         DD$Site<-latlongmatch$site[i]
                         DD1<-rbind(DD, DD1)
                         }
saveRDS(DD1, "DegreeDays_above32F.rds")

DD1<-data.frame(time=Date(), data.x=numeric(), data.y=numeric(), tmean=numeric(), x32=numeric(), wtr_year=character(), Year=character(), DD=numeric(), Site=character())

for (i in 1:nrow(latlongmatch)){
    
     var="daily_maximum_temperature"
     
       ## READ THE DATA VARIABLE (e.g. precipitation IN THIS CASE): http://www.inside-r.org/packages/cran/ncdf4/docs/ncvar_get 
       ## AND http://stackoverflow.com/questions/19936432/faster-reading-of-time-series-from-netcdf
       ## ORDER OF start= AND count= IS BASED ON ORDER IN BRACKETS AFTER VARIABLE NAME (SHOWN WHEN DISPLAYING YOUR METADATA)
       ## FROM THE DOCUMENTATION... "If [start] not specified, reading starts at the beginning of the file (1,1,1,...)."
       ## AND "If [count] not specified and the variable does NOT have an unlimited dimension, the entire variable is read. 
       ## As a special case, the value "-1" indicates that all entries along that dimension should be read."
       data <- ncvar_get(ncmax, var, start=c(lon[i],lat[i],1),count=c(1,1,-1))
       ## READ THE TIME VARIABLE
         time <- ncvar_get(ncmax, "day", start=c(1),count=c(-1))
         ## CONVERT TIME FROM "days since 1900-01-01" TO YYYY-MM-DD
          time=as.Date(time, origin="1900-01-01") ##note: assumes leap years! http://stat.ethz.ch/R-manual/R-patched/library/base/html/as.Date.html
           # PUT EVERYTHING INTO A DATA FRAME
             cmax <- data.frame(time,data)
             
               var="daily_minimum_temperature"
               data <- ncvar_get(ncmin, var, start=c(lon[i],lat[i],1),count=c(1,1,-1))
               ## READ THE TIME VARIABLE
                 time <- ncvar_get(ncmin, "day", start=c(1),count=c(-1))
                 ## CONVERT TIME FROM "days since 1900-01-01" TO YYYY-MM-DD
                   time=as.Date(time, origin="1900-01-01") ##note: assumes leap years! http://stat.ethz.ch/R-manual/R-patched/library/base/html/as.Date.html
                   # PUT EVERYTHING INTO A DATA FRAME
                     cmin <- data.frame(time,data)
                     
                       c<-merge(cmax, cmin, by="time")
                       c<-subset(c, time>"2001-01-01")
                       c$tmean<-(c$data.x+c$data.y)/2
                       c$x50<-c$tmean-283.15
                       c$wtr_year<-wtr_yr(c$time, start_month=10)
                       c$Year<-format(as.Date(c$time, '%Y/%m/%d'), "%Y")
                       c[c$x50<=0,]$x50<-0 #<= for DD above 32, > for DD below 32
                       DD<-ddply(c, .(Year), mutate, DD=cumsum(x50)) ##use this for heat days abov 32 since Jan 1st
                       # DD<-ddply(c, .(wtr_year), mutate, DD=cumsum(x32)) ## use this for freeze days below 32 since start of water year
                         DD$Site<-latlongmatch$site[i]
                         DD1<-rbind(DD, DD1)
                         }
saveRDS(DD1, "DegreeDays_above50F.rds")

## CLOSE THE FILE
nc_close(nc)

DDpr<-data.frame(time=Date(), data.x=numeric(), data.y=numeric(), wtr_year=character(), Year=character(), precip=numeric(),precipdays=numeric(), Site=character())
DDprsum<-data.frame(wtr_year=character(), precip=numeric(),precipdays=numeric(), Site=character())

i<-1
for (i in 1:nrow(latlongmatch)){
  
  var="precipitation_amount"
  
  ## READ THE DATA VARIABLE (e.g. precipitation IN THIS CASE): http://www.inside-r.org/packages/cran/ncdf4/docs/ncvar_get 
  ## AND http://stackoverflow.com/questions/19936432/faster-reading-of-time-series-from-netcdf
  ## ORDER OF start= AND count= IS BASED ON ORDER IN BRACKETS AFTER VARIABLE NAME (SHOWN WHEN DISPLAYING YOUR METADATA)
  ## FROM THE DOCUMENTATION... "If [start] not specified, reading starts at the beginning of the file (1,1,1,...)."
  ## AND "If [count] not specified and the variable does NOT have an unlimited dimension, the entire variable is read. 
  ## As a special case, the value "-1" indicates that all entries along that dimension should be read."
  data <- ncvar_get(ncpr, var, start=c(lon[i],lat[i],1),count=c(1,1,-1))
  ## READ THE TIME VARIABLE
  time <- ncvar_get(ncpr, "day", start=c(1),count=c(-1))
  ## CONVERT TIME FROM "days since 1900-01-01" TO YYYY-MM-DD
  time=as.Date(time, origin="1900-01-01") ##note: assumes leap years! http://stat.ethz.ch/R-manual/R-patched/library/base/html/as.Date.html
  # PUT EVERYTHING INTO A DATA FRAME
  precip <- data.frame(time,data)
  c<-subset(precip, time>"2001-01-01")
  c$wtr_year<-wtr_yr(c$time, start_month=10)
  c$Year<-format(as.Date(c$time, '%Y/%m/%d'), "%Y")
  DD<-ddply(c, .(wtr_year), mutate, precip=cumsum(data))
  DD<-ddply(DD, .(wtr_year), mutate, precipdays=cumsum(data>0))
  DD$Site<-latlongmatch$site[i]
  DDpr<-rbind(DD, DDpr)
  # DDsum<-ddply(c, .(wtr_year), summarize, precip=sum(data),precipdays=sum(data>0)) 

  # DDsum$Site<-latlongmatch$site[i]
  # DDprsum<-rbind(DDsum, DDprsum)
}

saveRDS(DDpr, "Precip.rds")

# DDprsum<-subset(DDprsum, wtr_year!=2001&wtr_year!=2021)
# write.csv(DDprsum, "Annual_precip_basin.csv")
# DDprsum<-read.csv("Annual_precip_basin.csv")
# ggplot(DDprsum, aes(x=wtr_year, y=precip, group=wtr_year))+geom_point()+geom_violin(draw_quantiles = .5)
# ggplot(DDprsum, aes(x=wtr_year, y=precipdays, group=wtr_year))+geom_point()+geom_violin(draw_quantiles = .5)
# DDprsumall<-ddply(DDprsum, .(Site), summarize, meanprecip=mean(precip), meanprecipdays=mean(precipdays))
# write.csv(DDprsumall, "mean_precip_basin.csv")

## PLOT THE DATA 
library(dplyr)
setwd(here::here())
setwd("./outputs")
DD50 <- readRDS("DegreeDays_above50F.rds")
# DD50_test<-readRDS("G:/GreenWQA/Biota/BenthosProjects_KC/trends analysis/2021 trends analysis/DegreeDays_above50F.rds")

test<-merge(subset(DD50, wtr_year=="2021"), subset(DD50_test, wtr_year=="2021"), by.x=c("Site", "time2", "wtr_year", "Year"), by.y=c("Site", "time", "wtr_year", "Year"))
test$test<-test$x50.x==test$x50.y

DD50$time<-format(as.Date(DD50$time, '%Y/%m/%d'), "%m/%d/%Y")
DD50$time_chr<-as.character(paste0(DD50$time))
DD50$time2<-as.Date(DD50$time, '%m/%d/%Y')
DD50$time_chr<-gsub("(\\D)0|^0", "\\1", DD50$time_chr)
KC_scores<-read.csv("B-IBI_results_03082023_2002-2021_NoUnique_subsamp_450_LR_exclude.csv")
KC_scores<-subset(KC_scores, Tot_Abund>=450)
KC_scores$Visit.Date<-as.Date(KC_scores$Visit.Date, "%Y-%m-%d")
samples<-subset(KC_scores, select=c(Sample.Code,Site.Code, Visit.Date)) ### make sure to fix 09DUW0277/05, incorrectly given site as 08BEA3312 in PSSB and B-IBI results
samples<-inner_join(samples, DD50, by=c("Site.Code"="Site", "Visit.Date"="time2"))
##added this next section to get change in annual daily mean air temps for FWS publication##
annualdailymean_site<-ddply(samples, .(Site.Code, Year), summarize, annualdailymean=mean(tmean))
annualdailymean_site$annualdailymean<-annualdailymean_site$annualdailymean-273.15
ggplot(annualdailymean_site, aes(x=Year, y=annualdailymean, group=Year))+geom_boxplot()
l=unique(c(as.character(annualdailymean_site$Site.Code)))
annualdailymean_site$Site.Number<-as.numeric(factor(annualdailymean_site$Site.Code, levels=l))
annualdailymean_site$Year<-as.numeric(paste0(annualdailymean_site$Year))
with(annualdailymean_site, rkt(date = Year, y = annualdailymean, block = Site.Number, correct = FALSE, rep="a"))
##
setdiff(KC_scores$Site.Code, samples$Site.Code)
samples<-subset(samples, select=c(Sample.Code, Site.Code, Visit.Date, Year, x50,DD, wtr_year))
write.csv(samples, "DegreeDaysabove10C.csv")

DD32 <- readRDS("DegreeDays_above32F.rds")
DD32$time<-format(as.Date(DD32$time, '%Y/%m/%d'), "%m/%d/%Y")
DD32$time_chr<-as.character(paste0(DD32$time))
DD32$time2<-as.Date(DD32$time, '%m/%d/%Y')
DD32$time_chr<-gsub("(\\D)0|^0", "\\1", DD32$time_chr)
KC_scores<-read.csv("B-IBI_results_03082023_2002-2021_NoUnique_subsamp_450_LR_exclude.csv")
KC_scores<-subset(KC_scores, Tot_Abund>=450)
KC_scores$Visit.Date<-as.Date(KC_scores$Visit.Date, "%Y-%m-%d")
samples<-subset(KC_scores, select=c(Sample.Code,Site.Code, Visit.Date)) ### make sure to fix 09DUW0277/05, incorrectly given site as 08BEA3312 in PSSB and B-IBI results
samples<-inner_join(samples, DD32, by=c("Site.Code"="Site", "Visit.Date"="time2"))
setdiff(KC_scores$Site.Code, samples$Site.Code)
samples<-subset(samples, select=c(Sample.Code, Site.Code, Visit.Date, Year, x32,DD, wtr_year))
write.csv(samples, "DegreeDaysabove0C.csv")

DD32 <- readRDS("DegreeDays_below32F.rds")
DD32$time<-format(as.Date(DD32$time, '%Y/%m/%d'), "%m/%d/%Y")
DD32$time_chr<-as.character(paste0(DD32$time))
DD32$time2<-as.Date(DD32$time, '%m/%d/%Y')
DD32$time_chr<-gsub("(\\D)0|^0", "\\1", DD32$time_chr)
KC_scores<-read.csv("B-IBI_results_03082023_2002-2021_NoUnique_subsamp_450_LR_exclude.csv")
KC_scores<-subset(KC_scores, Tot_Abund>=450)
KC_scores$Visit.Date<-as.Date(KC_scores$Visit.Date, "%Y-%m-%d")
samples<-subset(KC_scores, select=c(Sample.Code,Site.Code, Visit.Date)) ### make sure to fix 09DUW0277/05, incorrectly given site as 08BEA3312 in PSSB and B-IBI results
samples<-inner_join(samples, DD32, by=c("Site.Code"="Site", "Visit.Date"="time2"))
setdiff(KC_scores$Site.Code, samples$Site.Code)
samples<-subset(samples, select=c(Sample.Code, Site.Code, Visit.Date, Year, x32,DD, wtr_year))
write.csv(samples, "DegreeDaysbelow0C.csv")

DDPr <- readRDS("Precip.rds")
DDPr$time<-format(as.Date(DDPr$time, '%Y/%m/%d'), "%m/%d/%Y")
DDPr$time_chr<-as.character(paste0(DDPr$time))
DDPr$time2<-as.Date(DDPr$time, '%m/%d/%Y')
DDPr$time_chr<-gsub("(\\D)0|^0", "\\1", DDPr$time_chr)
KC_scores<-read.csv("B-IBI_results_03082023_2002-2021_NoUnique_subsamp_450_LR_exclude.csv")
KC_scores<-subset(KC_scores, Tot_Abund>=450)
KC_scores$Visit.Date<-as.Date(KC_scores$Visit.Date, "%Y-%m-%d")
samples<-subset(KC_scores, select=c(Sample.Code,Site.Code, Visit.Date)) ### make sure to fix 09DUW0277/05, incorrectly given site as 08BEA3312 in PSSB and B-IBI results
samples<-inner_join(samples, DDPr, by=c("Site.Code"="Site", "Visit.Date"="time2"))
setdiff(KC_scores$Site.Code, samples$Site.Code)
samples<-subset(samples, select=c(Sample.Code, Site.Code, Visit.Date, Year, precip, precipdays, wtr_year))
write.csv(samples, "Precipitation.csv")

degreedays10<-read.csv("DegreeDaysabove10C.csv", header=T)
degreedays0<-read.csv("DegreeDaysabove0C.csv", header=T)
# degreedays$Visit.Date<-as.Date(degreedays$Visit.Date, "%m/%d/%Y")
# degreedays$wtr_year<-wtr_yr(degreedays$Visit.Date, start_month=10)
degreedaysfreeze<-read.csv("DegreeDaysbelow0C.csv", header=T)
precip<-read.csv("Precipitation.csv")


library(ggplot2)
library(plyr)
ggplot(degreedays10, aes(y=DD, x=Year, group=Year, color=Year, label=Year))+geom_boxplot()
ggplot(degreedays0, aes(y=DD, x=Year, group=Year, color=Year, label=Year))+geom_boxplot()
ggplot(degreedaysfreeze, aes(y=DD, x=Year, group=Year, color=Year, label=Year))+geom_boxplot()
ggplot(precip, aes(y=precip, x=Year, group=Year, color=Year, label=Year))+geom_boxplot()
ggplot(precip, aes(y=precipdays, x=Year, group=Year, color=Year, label=Year))+geom_boxplot()


KC_scores<-read.csv("B-IBI_results_03082023_2002-2021_NoUnique_subsamp_450_LR_exclude.csv")
KC_scores<-subset(KC_scores, Tot_Abund>=450)
KC_trends<-read.csv("Site_Trends_coarse_LR_03092023_2002-2021_LRexclude.csv")
KC_trends<-subset(KC_trends, !(Site.Code=="09DUW0277"&Stream.or.River=="Riverton Creek (003D)"))
KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))
KC_scores<-subset(KC_scores, Site.Code %in% KC_trends$Site.Code)


####DD50F#######

# BIBI_scores<-read.csv("B-IBI_results_03082023_2002-2021_NoUnique_subsamp_450_LR_exclude.csv")
# BIBI_scores<-subset(BIBI_scores, Tot_Abund>=450)
degreedays10_scores<-merge(KC_scores, degreedays10, by.x=c("Site.Code", "Visit.Date", "Sample.Code"), by.y=c("Site.Code", "Visit.Date", "Sample.Code"))
degreedays10_scores<-degreedays10_scores[!str_detect(degreedays10_scores$Sample.Code, "_R"),]
degreedays10_scores<-degreedays10_scores[!str_detect(degreedays10_scores$Sample.Code, "[0-9]R[0-9]"),]
degreedays10_scores<-degreedays10_scores[!str_detect(degreedays10_scores$Sample.Code, "[0-9]R$"),]
degreedays10_scores<-degreedays10_scores[!str_detect(degreedays10_scores$Sample.Code, "QC"),]
degreedays10_scores<-degreedays10_scores[!str_detect(degreedays10_scores$Sample.Code, "_r"),]
degreedays10_scores<-degreedays10_scores[!str_detect(degreedays10_scores$Sample.Code, "Duplicate"),]

library(rkt)
l=unique(c(as.character(degreedays10_scores$Site.Code)))
degreedays10_scores$Site.Number<-as.numeric(factor(degreedays10_scores$Site.Code, levels=l))
with(degreedays10_scores, rkt(date = Year.x, y = DD, block = Site.Number, correct = FALSE, rep="a"))


library(EnvStats)
cor.test(degreedays10_scores$Overall.Score, degreedays10_scores$DD, method="spearman")
  # with(degreedays10_scores, kendallTrendTest(DD ~ as.numeric(Year.x)))$estimate[1]
# degreedays10_scores<- ddply(degreedays10_scores, .(Site.Code), mutate, slope =lm(DD ~ as.numeric(Year.x))[[1]][2])
degreedays10_scores<-ddply(degreedays10_scores,.(Site.Code),mutate, mk.tau=kendallTrendTest( DD~ as.numeric(Year.x))$estimate[1])
degreedays10_scores<-ddply(degreedays10_scores,.(Site.Code),mutate, mk.Slope=kendallTrendTest(DD ~ as.numeric(Year.x))$estimate[2])
degreedays10_trends<-unique(subset(degreedays10_scores, select=c(Site.Code, mk.tau, mk.Slope)))

# KC_trends<-read.csv("Site_Trends_coarse_LR_03092023_2002-2021_LRexclude.csv")
# KC_trends<-subset(KC_trends, !(Site.Code=="09DUW0277"&Stream.or.River=="Riverton Creek (003D)"))
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))

degreedays10_trends<-merge(KC_trends, degreedays10_trends,  by.x="Site.Code", by.y="Site.Code")
summary(degreedays10_trends$mk.Slope)
# cor.test(degreedays10_trends$MKtau_Overall.Score, degreedays10_trends$mk.tau)
cor.test(degreedays10_trends$MKSlope_Overall.Score, degreedays10_trends$mk.Slope, method="spearman")

#########DD>0C#######

degreedays0_scores<-merge(KC_scores, degreedays0, by.x=c("Site.Code", "Visit.Date", "Sample.Code"), by.y=c("Site.Code", "Visit.Date", "Sample.Code"))
degreedays0_scores<-degreedays0_scores[!str_detect(degreedays0_scores$Sample.Code, "_R"),]
degreedays0_scores<-degreedays0_scores[!str_detect(degreedays0_scores$Sample.Code, "[0-9]R[0-9]"),]
degreedays0_scores<-degreedays0_scores[!str_detect(degreedays0_scores$Sample.Code, "[0-9]R$"),]
degreedays0_scores<-degreedays0_scores[!str_detect(degreedays0_scores$Sample.Code, "QC"),]
degreedays0_scores<-degreedays0_scores[!str_detect(degreedays0_scores$Sample.Code, "_r"),]
degreedays0_scores<-degreedays0_scores[!str_detect(degreedays0_scores$Sample.Code, "Duplicate"),]

library(rkt)
l=unique(c(as.character(degreedays0_scores$Site.Code)))
degreedays0_scores$Site.Number<-as.numeric(factor(degreedays0_scores$Site.Code, levels=l))
with(degreedays0_scores, rkt(date = Year.x, y = DD, block = Site.Number, correct = FALSE, rep="a"))


library(EnvStats)
cor.test(degreedays0_scores$Overall.Score, degreedays0_scores$DD, method="spearman")
# with(degreedays0_scores, kendallTrendTest(DD ~ as.numeric(Year.x)))$estimate[1]
# degreedays0_scores<- ddply(degreedays0_scores, .(Site.Code), mutate, slope =lm(DD ~ as.numeric(Year.x))[[1]][2])
degreedays0_scores<-ddply(degreedays0_scores,.(Site.Code),mutate, mk.tau=kendallTrendTest( DD~ as.numeric(Year.x))$estimate[1])
degreedays0_scores<-ddply(degreedays0_scores,.(Site.Code),mutate, mk.Slope=kendallTrendTest(DD ~ as.numeric(Year.x))$estimate[2])
degreedays0_trends<-unique(subset(degreedays0_scores, select=c(Site.Code, mk.tau, mk.Slope))) 

# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))

degreedays0_trends<-merge(KC_trends, degreedays0_trends,  by.x="Site.Code", by.y="Site.Code")
summary(degreedays0_trends$mk.Slope)
# cor.test(degreedays0_trends$MKtau_Overall.Score, degreedays0_trends$mk.tau)
cor.test(degreedays0_trends$MKSlope_Overall.Score, degreedays0_trends$mk.Slope, method="spearman")

############DDFreezing###############

degreedaysfreeze_scores<-merge(KC_scores, degreedaysfreeze, by.x=c("Site.Code", "Visit.Date", "Sample.Code"), by.y=c("Site.Code", "Visit.Date", "Sample.Code"))
degreedaysfreeze_scores<-degreedaysfreeze_scores[!str_detect(degreedaysfreeze_scores$Sample.Code, "_R"),]
degreedaysfreeze_scores<-degreedaysfreeze_scores[!str_detect(degreedaysfreeze_scores$Sample.Code, "[0-9]R[0-9]"),]
degreedaysfreeze_scores<-degreedaysfreeze_scores[!str_detect(degreedaysfreeze_scores$Sample.Code, "[0-9]R$"),]
degreedaysfreeze_scores<-degreedaysfreeze_scores[!str_detect(degreedaysfreeze_scores$Sample.Code, "QC"),]
degreedaysfreeze_scores<-degreedaysfreeze_scores[!str_detect(degreedaysfreeze_scores$Sample.Code, "_r"),]
degreedaysfreeze_scores<-degreedaysfreeze_scores[!str_detect(degreedaysfreeze_scores$Sample.Code, "Duplicate"),]

library(rkt)
l=unique(c(as.character(degreedaysfreeze_scores$Site.Code)))
degreedaysfreeze_scores$Site.Number<-as.numeric(factor(degreedaysfreeze_scores$Site.Code, levels=l))
with(degreedaysfreeze_scores, rkt(date = Year.x, y = abs(DD), block = Site.Number, correct = FALSE, rep="a"))


library(EnvStats)
cor.test(degreedaysfreeze_scores$Overall.Score, abs(degreedaysfreeze_scores$DD), method="spearman")
# with(degreedaysfreeze_scores, kendallTrendTest(DD ~ as.numeric(Year.x)))$estimate[1]
# degreedaysfreeze_scores<- ddply(degreedaysfreeze_scores, .(Site.Code), mutate, slope =lm(DD ~ as.numeric(Year.x))[[1]][2])
degreedaysfreeze_scores<-ddply(degreedaysfreeze_scores,.(Site.Code),mutate, mk.tau=kendallTrendTest( abs(DD)~ as.numeric(Year.x))$estimate[1])
degreedaysfreeze_scores<-ddply(degreedaysfreeze_scores,.(Site.Code),mutate, mk.Slope=kendallTrendTest(abs(DD) ~ as.numeric(Year.x))$estimate[2])
degreedaysfreeze_trends<-unique(subset(degreedaysfreeze_scores, select=c(Site.Code, mk.tau, mk.Slope))) 

# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))

degreedaysfreeze_trends<-merge(KC_trends, degreedaysfreeze_trends,  by.x="Site.Code", by.y="Site.Code")
summary(degreedaysfreeze_trends$mk.Slope)
# cor.test(degreedaysfreeze_trends$MKtau_Overall.Score, degreedaysfreeze_trends$mk.tau)
cor.test(degreedaysfreeze_trends$MKSlope_Overall.Score, degreedaysfreeze_trends$mk.Slope, method="spearman")

###########Total accumulated precipitation#####

precip_scores<-merge(KC_scores, precip, by.x=c("Site.Code", "Visit.Date", "Sample.Code"), by.y=c("Site.Code", "Visit.Date", "Sample.Code"))
precip_scores<-precip_scores[!str_detect(precip_scores$Sample.Code, "_R"),]
precip_scores<-precip_scores[!str_detect(precip_scores$Sample.Code, "[0-9]R[0-9]"),]
precip_scores<-precip_scores[!str_detect(precip_scores$Sample.Code, "[0-9]R$"),]
precip_scores<-precip_scores[!str_detect(precip_scores$Sample.Code, "QC"),]
precip_scores<-precip_scores[!str_detect(precip_scores$Sample.Code, "_r"),]
precip_scores<-precip_scores[!str_detect(precip_scores$Sample.Code, "Duplicate"),]

mean(precip_scores$precip)
mean(ddply(precip_scores, .(Site.Code), summarize, meanprecip=mean(precip))$meanprecip)

library(rkt)
l=unique(c(as.character(precip_scores$Site.Code)))
precip_scores$Site.Number<-as.numeric(factor(precip_scores$Site.Code, levels=l))
with(precip_scores, rkt(date = Year.x, y = precip, block = Site.Number, correct = FALSE, rep="a"))


library(EnvStats)
cor.test(precip_scores$Overall.Score, precip_scores$precip, method="spearman")
precip_scores<-ddply(precip_scores,.(Site.Code),mutate, mk.tau=kendallTrendTest( precip~ as.numeric(Year.x))$estimate[1])
precip_scores<-ddply(precip_scores,.(Site.Code),mutate, mk.Slope=kendallTrendTest(precip ~ as.numeric(Year.x))$estimate[2])
precip_trends<-unique(subset(precip_scores, select=c(Site.Code, mk.tau, mk.Slope))) 

# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))

precip_trends<-merge(KC_trends, precip_trends,  by.x="Site.Code", by.y="Site.Code")
summary(precip_trends$mk.Slope)
# cor.test(precip_trends$MKtau_Overall.Score, precip_trends$mk.tau)
cor.test(precip_trends$MKSlope_Overall.Score, precip_trends$mk.Slope, method="spearman")

###########Total precipitation days#####


with(precip_scores, rkt(date = Year.x, y = precipdays, block = Site.Number, correct = FALSE, rep="a"))


library(EnvStats)
cor.test(precip_scores$Overall.Score, precip_scores$precipdays, method="spearman")
precip_scores<-ddply(precip_scores,.(Site.Code),mutate, mk.tau=kendallTrendTest( precipdays~ as.numeric(Year.x))$estimate[1])
precip_scores<-ddply(precip_scores,.(Site.Code),mutate, mk.Slope=kendallTrendTest(precipdays ~ as.numeric(Year.x))$estimate[2])
precip_trends<-unique(subset(precip_scores, select=c(Site.Code, mk.tau, mk.Slope))) 

mean(precip_scores$precipdays)
mean(ddply(precip_scores, .(Site.Code), summarize, meanprecip=mean(precipdays))$meanprecip)
# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))

precip_trends<-merge(KC_trends, precip_trends,  by.x="Site.Code", by.y="Site.Code")
summary(precip_trends$mk.Slope)
# cor.test(precip_trends$MKtau_Overall.Score, precip_trends$mk.tau)
cor.test(precip_trends$MKSlope_Overall.Score, precip_trends$mk.Slope, method="spearman")

###############LastFreeze###############



DegreeDays_Below32_2 <- readRDS("DegreeDays_below32F.rds")
DD1<-DegreeDays_Below32_2
DD1$time<-format(as.Date(DD1$time, '%Y/%m/%d'), "%m/%d/%Y")
DD1$time_chr<-as.character(paste0(DD1$time))
DD1$time2<-as.Date(DD1$time, '%m/%d/%Y')
DD1$time_chr<-gsub("(\\D)0|^0", "\\1", DD1$time_chr)

DD1$julian<-as.numeric(format(as.Date(DD1$time, '%m/%d/%Y'), "%j"))
366-as.numeric(format(as.Date("10/1/2002", '%m/%d/%Y'), "%j"))
DD1$WYjulian<-(DD1$julian+92) %% 366
lastfrz<-ddply(subset(DD1, x32<0), .(wtr_year, Site), summarize,lastFrz=max(WYjulian))

# KC_scores<-read.csv("B-IBI_results_1182023_2002-2021.csv")
# KC_scores$Visit.Date<-as.Date(KC_scores$Visit.Date, "%Y-%m-%d")
samples<-subset(KC_scores, select=c(Sample.Code,Site.Code, Visit.Date, Overall.Score)) ### make sure to fix 09DUW0277/05, incorrectly given site as 08BEA3312 in PSSB and B-IBI results


samples$Year<-as.numeric(format(as.Date(samples$Visit.Date, '%Y-%m-%d'), "%Y"))
lastFrz<-inner_join(samples, lastfrz, by=c("Site.Code"="Site", "Year"="wtr_year"))
# subset(samples, !Sample.Code %in% lastFrz$Sample.Code)
library(ggpmisc)

lastFrz<-lastFrz[!str_detect(lastFrz$Sample.Code, "_R"),]
lastFrz<-lastFrz[!str_detect(lastFrz$Sample.Code, "[0-9]R[0-9]"),]
lastFrz<-lastFrz[!str_detect(lastFrz$Sample.Code, "[0-9]R$"),]
lastFrz<-lastFrz[!str_detect(lastFrz$Sample.Code, "QC"),]
lastFrz<-lastFrz[!str_detect(lastFrz$Sample.Code, "_r"),]
lastFrz<-lastFrz[!str_detect(lastFrz$Sample.Code, "Duplicate"),]
ggplot(lastFrz, aes(x=lastFrz))+geom_density()

l=unique(c(as.character(lastFrz$Site.Code)))
lastFrz$Site.Number<-as.numeric(factor(lastFrz$Site.Code, levels=l))
with(lastFrz, rkt(date = Year, y = lastFrz, block = Site.Number, correct = FALSE, rep="a"))

cor.test(lastFrz$Overall.Score, lastFrz$lastFrz, method="spearman")

lastFrz<-ddply(lastFrz,.(Site.Code),mutate, mk.tau=kendallTrendTest( lastFrz~ as.numeric(Year))$estimate[1])
lastFrz<-ddply(lastFrz,.(Site.Code),mutate, mk.slope=kendallTrendTest( lastFrz~ as.numeric(Year))$estimate[2])

lastFrz_trends<-unique(subset(lastFrz, select=c(Site.Code, mk.slope, mk.tau))) 

# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code))

lastFrz_trends<-merge(KC_trends, lastFrz_trends,  by.x="Site.Code", by.y="Site.Code")
summary(lastFrz_trends$mk.slope)
# cor.test(lastFrz_trends$MKtau_Overall.Score, lastFrz_trends$mk.tau)
cor.test(lastFrz_trends$MKSlope_Overall.Score, lastFrz_trends$mk.slope, method="spearman")



######Worst Freeze#########
DD1<-DegreeDays_Below32_2

DD1$time<-format(as.Date(DD1$time, '%Y/%m/%d'), "%m/%d/%Y")
DD1$time_chr<-as.character(paste0(DD1$time))
DD1$time2<-as.Date(DD1$time, '%m/%d/%Y')
DD1$time_chr<-gsub("(\\D)0|^0", "\\1", DD1$time_chr)

wrstfrz<-ddply(DD1, .(wtr_year, Site), summarize,minT=min(x32))

# KC_scores<-read.csv("B-IBI_results_1182023_2002-2021.csv")
# KC_scores$Visit.Date<-as.Date(KC_scores$Visit.Date, "%Y-%m-%d")
samples<-subset(KC_scores, select=c(Sample.Code,Site.Code, Visit.Date, Overall.Score)) ### make sure to fix 09DUW0277/05, incorrectly given site as 08BEA3312 in PSSB and B-IBI results
samples$Year<-as.numeric(format(as.Date(samples$Visit.Date, '%Y-%m-%d'), "%Y"))
wrstfrz<-inner_join(samples, wrstfrz, by=c("Site.Code"="Site", "Year"="wtr_year"))


wrstfrz<-wrstfrz[!str_detect(wrstfrz$Sample.Code, "_R"),]
wrstfrz<-wrstfrz[!str_detect(wrstfrz$Sample.Code, "[0-9]R[0-9]"),]
wrstfrz<-wrstfrz[!str_detect(wrstfrz$Sample.Code, "[0-9]R$"),]
wrstfrz<-wrstfrz[!str_detect(wrstfrz$Sample.Code, "QC"),]
wrstfrz<-wrstfrz[!str_detect(wrstfrz$Sample.Code, "_r"),]
wrstfrz<-wrstfrz[!str_detect(wrstfrz$Sample.Code, "Duplicate"),]
ggplot(wrstfrz, aes(x=minT))+geom_density()


l=unique(c(as.character(wrstfrz$Site.Code)))
wrstfrz$Site.Number<-as.numeric(factor(wrstfrz$Site.Code, levels=l))
with(wrstfrz, rkt(date = Year, y = minT, block = Site.Number, correct = FALSE, rep="a"))

cor.test(wrstfrz$Overall.Score, wrstfrz$minT, method="spearman")

# wrstfrz<- ddply(wrstfrz, .(STAID), mutate, slope =lm(minT ~ as.numeric(Year))[[1]][2])
wrstfrz<-ddply(wrstfrz,.(Site.Code),mutate, mk.tau=kendallTrendTest( minT~ as.numeric(Year))$estimate[1])
wrstfrz<-ddply(wrstfrz,.(Site.Code),mutate, mk.slope=kendallTrendTest( minT~ as.numeric(Year))$estimate[2])

wrstfrz_trends<-unique(subset(wrstfrz, select=c(Site.Code, mk.slope, mk.tau))) 

# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code))

wrstfrz_trends<-merge(KC_trends, wrstfrz_trends,  by.x="Site.Code", by.y="Site.Code")
summary(wrstfrz_trends$mk.slope)
# cor.test(wrstfrz_trends$MKtau_Overall.Score, wrstfrz_trends$mk.tau)
cor.test(wrstfrz_trends$MKSlope_Overall.Score, wrstfrz_trends$mk.slope, method="spearman")


######average rainy season temp########
DD1<-DegreeDays_Below32_2

DD1$time<-format(as.Date(DD1$time, '%Y/%m/%d'), "%m/%d/%Y")
DD1$time_chr<-as.character(paste0(DD1$time))
DD1$time2<-as.Date(DD1$time, '%m/%d/%Y')
DD1$time_chr<-gsub("(\\D)0|^0", "\\1", DD1$time_chr)
DD1$month<-month(DD1$time2)
subset(DD1, month>=10|month<=3)
meanmin<-ddply(subset(DD1, month>=10|month<=3), .(wtr_year, Site), summarize, minAvg=mean(tmean))### I changed this to spring temps!

# KC_scores<-read.csv("B-IBI_results_1182023_2002-2021.csv")
# KC_scores$Visit.Date<-as.Date(KC_scores$Visit.Date, "%Y-%m-%d")
samples<-subset(KC_scores, select=c(Sample.Code,Site.Code, Visit.Date, Overall.Score)) ### make sure to fix 09DUW0277/05, incorrectly given site as 08BEA3312 in PSSB and B-IBI results
samples$Year<-as.numeric(format(as.Date(samples$Visit.Date, '%Y-%m-%d'), "%Y"))
meanmin<-inner_join(samples, meanmin, by=c("Site.Code"="Site", "Year"="wtr_year"))

meanmin<-meanmin[!str_detect(meanmin$Sample.Code, "_R"),]
meanmin<-meanmin[!str_detect(meanmin$Sample.Code, "[0-9]R[0-9]"),]
meanmin<-meanmin[!str_detect(meanmin$Sample.Code, "[0-9]R$"),]
meanmin<-meanmin[!str_detect(meanmin$Sample.Code, "QC"),]
meanmin<-meanmin[!str_detect(meanmin$Sample.Code, "_r"),]
meanmin<-meanmin[!str_detect(meanmin$Sample.Code, "Duplicate"),]
ggplot(meanmin, aes(x=minAvg))+geom_density()


l=unique(c(as.character(meanmin$Site.Code)))
meanmin$Site.Number<-as.numeric(factor(meanmin$Site.Code, levels=l))
with(meanmin, rkt(date = Year, y = minAvg, block = Site.Number, correct = FALSE, rep="a"))

cor.test(meanmin$Overall.Score, meanmin$minAvg, method="spearman")

meanmin<-ddply(meanmin,.(Site.Code),mutate, mk.tau=kendallTrendTest( minAvg~ as.numeric(Year))$estimate[1])
meanmin<-ddply(meanmin,.(Site.Code),mutate, mk.slope=kendallTrendTest( minAvg~ as.numeric(Year))$estimate[2])

meanmin_trends<-unique(subset(meanmin, select=c(Site.Code, mk.slope, mk.tau))) 

# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code))

meanmin_trends<-merge(KC_trends, meanmin_trends,  by.x="Site.Code", by.y="Site.Code")
summary(meanmin_trends$mk.slope)
# cor.test(meanmin_trends$MKtau_Overall.Score, meanmin_trends$mk.tau)
cor.test(meanmin_trends$MKSlope_Overall.Score, meanmin_trends$mk.slope, method="spearman")
ggplot(meanmin_trends, aes(y=MKSlope_Overall.Score, x=mk.slope))+geom_point()



##########worst heat#############
DegreeDays_above32 <- readRDS("DegreeDays_above32F.rds")

DD1<-DegreeDays_above32
DD1$time<-format(as.Date(DD1$time, '%Y/%m/%d'), "%m/%d/%Y")
DD1$time_chr<-as.character(paste0(DD1$time))
DD1$time2<-as.Date(DD1$time, '%m/%d/%Y')
DD1$time_chr<-gsub("(\\D)0|^0", "\\1", DD1$time_chr)
wrstht<-ddply(DD1, .(Year, Site), summarize,maxT=max(x32))
wrstht$Year<-as.numeric(wrstht$Year)

# KC_scores<-read.csv("B-IBI_results_1182023_2002-2021.csv")
# KC_scores$Visit.Date<-as.Date(KC_scores$Visit.Date, "%Y-%m-%d")
samples<-subset(KC_scores, select=c(Sample.Code,Site.Code, Visit.Date, Overall.Score)) ### make sure to fix 09DUW0277/05, incorrectly given site as 08BEA3312 in PSSB and B-IBI results
samples$Year<-as.numeric(format(as.Date(samples$Visit.Date, '%Y-%m-%d'), "%Y"))
wrstht<-inner_join(samples, wrstht, by=c("Site.Code"="Site", "Year"="Year"))

wrstht<-wrstht[!str_detect(wrstht$Sample.Code, "_R"),]
wrstht<-wrstht[!str_detect(wrstht$Sample.Code, "[0-9]R[0-9]"),]
wrstht<-wrstht[!str_detect(wrstht$Sample.Code, "[0-9]R$"),]
wrstht<-wrstht[!str_detect(wrstht$Sample.Code, "QC"),]
wrstht<-wrstht[!str_detect(wrstht$Sample.Code, "_r"),]
wrstht<-wrstht[!str_detect(wrstht$Sample.Code, "Duplicate"),]


l=unique(c(as.character(wrstht$Site.Code)))
wrstht$Site.Number<-as.numeric(factor(wrstht$Site.Code, levels=l))
with(wrstht, rkt(date = Year, y = maxT, block = Site.Number, correct = FALSE, rep="a"))

cor.test(wrstht$Overall.Score, wrstht$maxT, method="spearman")

wrstht<-ddply(wrstht,.(Site.Code),mutate, mk.tau=kendallTrendTest( maxT~ as.numeric(Year))$estimate[1])
wrstht<-ddply(wrstht,.(Site.Code),mutate, mk.slope=kendallTrendTest( maxT~ as.numeric(Year))$estimate[2])
wrstht_trends<-unique(subset(wrstht, select=c(Site.Code, mk.slope, mk.tau))) 

# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code))

wrstht_trends<-merge(KC_trends, wrstht_trends,  by.x="Site.Code", by.y="Site.Code")
summary(wrstht_trends$mk.slope)
# cor.test(wrstht_trends$MKtau_Overall.Score, wrstht_trends$mk.tau)
cor.test(wrstht_trends$MKSlope_Overall.Score, wrstht_trends$mk.slope, method="spearman")

######average non-rainy season temp######
DD1<-DegreeDays_above32

DD1$time<-format(as.Date(DD1$time, '%Y/%m/%d'), "%m/%d/%Y")
DD1$time_chr<-as.character(paste0(DD1$time))
DD1$time2<-as.Date(DD1$time, '%m/%d/%Y')
DD1$time_chr<-gsub("(\\D)0|^0", "\\1", DD1$time_chr)
DD1$month<-format(as.Date(DD1$time, "%m/%d/%Y"), "%m")
unique(subset(DD1, month<="10"&month>="03")$month)
meanmax<-ddply(subset(DD1, month<="09"&month>="04"), .(Year, Site), summarize, maxAvg=mean(tmean))
meanmax$Year<-as.numeric(paste0(meanmax$Year))

# KC_scores<-read.csv("B-IBI_results_1182023_2002-2021.csv")
# KC_scores$Visit.Date<-as.Date(KC_scores$Visit.Date, "%Y-%m-%d")
samples<-subset(KC_scores, select=c(Sample.Code,Site.Code, Visit.Date, Overall.Score)) ### make sure to fix 09DUW0277/05, incorrectly given site as 08BEA3312 in PSSB and B-IBI results
samples$Year<-as.numeric(format(as.Date(samples$Visit.Date, '%Y-%m-%d'), "%Y"))
meanmax<-inner_join(samples, meanmax, by=c("Site.Code"="Site", "Year"="Year"))

meanmax<-meanmax[!str_detect(meanmax$Sample.Code, "_R"),]
meanmax<-meanmax[!str_detect(meanmax$Sample.Code, "[0-9]R[0-9]"),]
meanmax<-meanmax[!str_detect(meanmax$Sample.Code, "[0-9]R$"),]
meanmax<-meanmax[!str_detect(meanmax$Sample.Code, "QC"),]
meanmax<-meanmax[!str_detect(meanmax$Sample.Code, "_r"),]
meanmax<-meanmax[!str_detect(meanmax$Sample.Code, "Duplicate"),]

ggplot(meanmax, aes(x=maxAvg))+geom_density()



l=unique(c(as.character(meanmax$Site.Code)))
meanmax$Site.Number<-as.numeric(factor(meanmax$Site.Code, levels=l))
with(meanmax, rkt(date = Year, y = maxAvg, block = Site.Number, correct = FALSE, rep="a"))

cor.test(meanmax$Overall.Score, meanmax$maxAvg, method="spearman")

meanmax<-ddply(meanmax,.(Site.Code),mutate, mk.tau=kendallTrendTest( maxAvg~ as.numeric(Year))$estimate[1])
meanmax<-ddply(meanmax,.(Site.Code),mutate, mk.slope=kendallTrendTest( maxAvg~ as.numeric(Year))$estimate[2])

meanmax_trends<-unique(subset(meanmax, select=c(Site.Code, mk.slope, mk.tau))) 

# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code))

meanmax_trends<-merge(KC_trends, meanmax_trends,  by.x="Site.Code", by.y="Site.Code")
summary(meanmax_trends$mk.slope)
# cor.test(meanmax_trends$MKtau_Overall.Score, meanmax_trends$mk.tau)
cor.test(meanmax_trends$MKSlope_Overall.Score, meanmax_trends$mk.slope, method="spearman")
ggplot(data2_score_trends, aes(y=MKSlope_Overall_Score, x=slope))+geom_point()


########mean daily precipitation##########

DDpr <- readRDS("Precip.rds")

DD1<-DDpr
DD1$time<-format(as.Date(DD1$time, '%Y/%m/%d'), "%m/%d/%Y")
DD1$time_chr<-as.character(paste0(DD1$time))
DD1$time2<-as.Date(DD1$time, '%m/%d/%Y')
DD1$time_chr<-gsub("(\\D)0|^0", "\\1", DD1$time_chr)
DD1$month<-format(as.Date(DD1$time, "%m/%d/%Y"), "%m")
unique(subset(DD1, month>="03"&month<="06")$month)


# precip_data<-ddply(DD1, .(Site, wtr_year, month), summarize, precip=sum(data))
# ggplot(precip_data, aes(x=as.numeric(wtr_year), y=precip, group=as.numeric(wtr_year)))+geom_boxplot()+facet_wrap(~month)
# precip_max_data<-ddply(DD1, .(Site, wtr_year, month), summarize, precipdailymax=max(data))
# ggplot(precip_max_data, aes(x=as.numeric(wtr_year), y=precipmax, group=as.numeric(wtr_year)))+geom_boxplot()+facet_wrap(~month)
# precip_days_data<-ddply(DD1, .(Site, wtr_year, month), summarize, precipdays=sum(data>0))
# ggplot(precip_days_data, aes(x=as.numeric(wtr_year), y=precipdays, group=as.numeric(wtr_year)))+geom_boxplot()+facet_wrap(~month)

precip_mean_data<-ddply(subset(DD1), .(Site, wtr_year), summarize, meandailyprecip=mean(data))
# ggplot(precip_mean_data, aes(x=as.numeric(wtr_year), y=meandailyprecip, group=as.numeric(wtr_year)))+geom_boxplot()

precip_mean_data$wtr_year<-as.numeric(paste0(precip_mean_data$wtr_year))

# KC_scores<-read.csv("B-IBI_results_1182023_2002-2021.csv")
# KC_scores$Visit.Date<-as.Date(KC_scores$Visit.Date, "%Y-%m-%d")
samples<-subset(KC_scores, select=c(Sample.Code,Site.Code, Visit.Date, Overall.Score)) ### make sure to fix 09DUW0277/05, incorrectly given site as 08BEA3312 in PSSB and B-IBI results
samples$Year<-as.numeric(format(as.Date(samples$Visit.Date, '%Y-%m-%d'), "%Y"))
precip_mean_data<-inner_join(samples, precip_mean_data, by=c("Site.Code"="Site", "Year"="wtr_year"))

precip_mean_data<-precip_mean_data[!str_detect(precip_mean_data$Sample.Code, "_R"),]
precip_mean_data<-precip_mean_data[!str_detect(precip_mean_data$Sample.Code, "[0-9]R[0-9]"),]
precip_mean_data<-precip_mean_data[!str_detect(precip_mean_data$Sample.Code, "[0-9]R$"),]
precip_mean_data<-precip_mean_data[!str_detect(precip_mean_data$Sample.Code, "QC"),]
precip_mean_data<-precip_mean_data[!str_detect(precip_mean_data$Sample.Code, "_r"),]
precip_mean_data<-precip_mean_data[!str_detect(precip_mean_data$Sample.Code, "Duplicate"),]

ggplot(precip_mean_data, aes(x=meandailyprecip))+geom_density()



library(rkt)
l=unique(c(as.character(precip_mean_data$Site.Code)))
precip_mean_data$Site.Number<-as.numeric(factor(precip_mean_data$Site.Code, levels=l))
with(precip_mean_data, rkt(date = as.numeric(Year), y = meandailyprecip, block = Site.Number, correct = FALSE, rep="a"))
# ggplot(precip_mean_data, aes(y=meandailyprecip, x=Year, group=Year))+geom_boxplot()+labs(y="mean daily precip", x="water year")

cor.test(precip_mean_data$Overall.Score, precip_mean_data$meandailyprecip, method="spearman")

precip_mean_data<-ddply(precip_mean_data,.(Site.Code),mutate, mk.tau=kendallTrendTest( meandailyprecip~ as.numeric(Year))$estimate[1])
precip_mean_data<-ddply(precip_mean_data,.(Site.Code),mutate, mk.slope=kendallTrendTest( meandailyprecip~ as.numeric(Year))$estimate[2])

precip_mean_data<-unique(subset(precip_mean_data, select=c(Site.Code, mk.slope, mk.tau))) 

# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code))

precip_mean_data<-merge(KC_trends, precip_mean_data,  by.x="Site.Code", by.y="Site.Code")
summary(precip_mean_data$mk.slope)
# cor.test(precip_mean_data$MKtau_Overall.Score, precip_mean_data$mk.tau)
cor.test(precip_mean_data$MKSlope_Overall.Score, precip_mean_data$mk.slope, method="spearman")
ggplot(precip_mean_data, aes(x=mk.slope, y=MKSlope_Overall.Score))+geom_point()
#############annual max daily precipitation#################
DD1<-DDpr
DD1$time<-format(as.Date(DD1$time, '%Y/%m/%d'), "%m/%d/%Y")
DD1$time_chr<-as.character(paste0(DD1$time))
DD1$time2<-as.Date(DD1$time, '%m/%d/%Y')
DD1$time_chr<-gsub("(\\D)0|^0", "\\1", DD1$time_chr)
DD1$month<-format(as.Date(DD1$time, "%m/%d/%Y"), "%m")
unique(subset(DD1, month>="03"&month<="06")$month)

# DD1$month<-format(as.Date(DD1$time, "%m/%d/%Y"), "%m")
max_precip<-ddply(subset(DD1), .(Site, wtr_year), summarize, maxprecip=max(data))
max_precip$wtr_year<-as.numeric(paste0(max_precip$wtr_year))

# KC_scores<-read.csv("B-IBI_results_1182023_2002-2021.csv")
# KC_scores$Visit.Date<-as.Date(KC_scores$Visit.Date, "%Y-%m-%d")
samples<-subset(KC_scores, select=c(Sample.Code,Site.Code, Visit.Date, Overall.Score)) ### make sure to fix 09DUW0277/05, incorrectly given site as 08BEA3312 in PSSB and B-IBI results
samples$Year<-as.numeric(format(as.Date(samples$Visit.Date, '%Y-%m-%d'), "%Y"))
max_precip<-inner_join(samples, max_precip, by=c("Site.Code"="Site", "Year"="wtr_year"))

max_precip<-max_precip[!str_detect(max_precip$Sample.Code, "_R"),]
max_precip<-max_precip[!str_detect(max_precip$Sample.Code, "[0-9]R[0-9]"),]
max_precip<-max_precip[!str_detect(max_precip$Sample.Code, "[0-9]R$"),]
max_precip<-max_precip[!str_detect(max_precip$Sample.Code, "QC"),]
max_precip<-max_precip[!str_detect(max_precip$Sample.Code, "_r"),]
max_precip<-max_precip[!str_detect(max_precip$Sample.Code, "Duplicate"),]

ggplot(max_precip, aes(x=maxprecip))+geom_density()

l=unique(c(as.character(max_precip$Site.Code)))
max_precip$Site.Number<-as.numeric(factor(max_precip$Site.Code, levels=l))
with(max_precip, rkt(date = as.numeric(Year), y = maxprecip, block = Site.Number, correct = FALSE, rep="a"))

cor.test(max_precip$Overall.Score, max_precip$maxprecip, method="spearman")
max_precip<-ddply(max_precip,.(Site.Code),mutate, mk.tau=kendallTrendTest( maxprecip~ as.numeric(Year))$estimate[1])
max_precip<-ddply(max_precip,.(Site.Code),mutate, mk.slope=kendallTrendTest( maxprecip~ as.numeric(Year))$estimate[2])

max_precip<-unique(subset(max_precip, select=c(Site.Code, mk.slope, mk.tau))) 

# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code))

max_precip<-merge(KC_trends, max_precip,  by.x="Site.Code", by.y="Site.Code")
summary(max_precip$mk.slope)
# cor.test(max_precip$MKtau_Overall.Score, max_precip$mk.tau)
cor.test(max_precip$MKSlope_Overall.Score, max_precip$mk.slope, method="spearman")
ggplot(max_precip, aes(y=MKSlope_Overall.Score, x=mk.slope))+geom_point()

###############Last Rain (1 inch)###############

DD1<-DDpr
DD1$time<-format(as.Date(DD1$time, '%Y/%m/%d'), "%m/%d/%Y")
DD1$time_chr<-as.character(paste0(DD1$time))
DD1$time2<-as.Date(DD1$time, '%m/%d/%Y')
DD1$time_chr<-gsub("(\\D)0|^0", "\\1", DD1$time_chr)
DD1$month<-format(as.Date(DD1$time, "%m/%d/%Y"), "%m")


DD1$julian<-as.numeric(format(as.Date(DD1$time, '%m/%d/%Y'), "%j"))
366-as.numeric(format(as.Date("10/1/2002", '%m/%d/%Y'), "%j"))
DD1$WYjulian<-(DD1$julian+92) %% 366
lastrain<-ddply(subset(DD1, month<="06"&data>=25.4), .(wtr_year, Site), summarize,lastRain=max(WYjulian))

# KC_scores<-read.csv("B-IBI_results_1182023_2002-2021.csv")
# KC_scores$Visit.Date<-as.Date(KC_scores$Visit.Date, "%Y-%m-%d")
samples<-subset(KC_scores, select=c(Sample.Code,Site.Code, Visit.Date, Overall.Score)) ### make sure to fix 09DUW0277/05, incorrectly given site as 08BEA3312 in PSSB and B-IBI results


samples$Year<-as.numeric(format(as.Date(samples$Visit.Date, '%Y-%m-%d'), "%Y"))
lastRain<-inner_join(samples, lastrain, by=c("Site.Code"="Site", "Year"="wtr_year"))
# subset(samples, !Sample.Code %in% lastFrz$Sample.Code)
library(ggpmisc)

lastRain<-lastRain[!str_detect(lastRain$Sample.Code, "_R"),]
lastRain<-lastRain[!str_detect(lastRain$Sample.Code, "[0-9]R[0-9]"),]
lastRain<-lastRain[!str_detect(lastRain$Sample.Code, "[0-9]R$"),]
lastRain<-lastRain[!str_detect(lastRain$Sample.Code, "QC"),]
lastRain<-lastRain[!str_detect(lastRain$Sample.Code, "_r"),]
lastRain<-lastRain[!str_detect(lastRain$Sample.Code, "Duplicate"),]
ggplot(lastRain, aes(x=lastRain))+geom_density()

l=unique(c(as.character(lastRain$Site.Code)))
lastRain$Site.Number<-as.numeric(factor(lastRain$Site.Code, levels=l))
with(lastRain, rkt(date = Year, y = lastRain, block = Site.Number, correct = FALSE, rep="a"))

cor.test(lastRain$Overall.Score, lastRain$lastRain, method="spearman")

lastRain<-ddply(lastRain,.(Site.Code),mutate, mk.tau=kendallTrendTest( lastRain~ as.numeric(Year))$estimate[1])
lastRain<-ddply(lastRain,.(Site.Code),mutate, mk.slope=kendallTrendTest( lastRain~ as.numeric(Year))$estimate[2])

lastRain_trends<-unique(subset(lastRain, select=c(Site.Code, mk.slope, mk.tau))) 

# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code))

lastRain_trends<-merge(KC_trends, lastRain_trends,  by.x="Site.Code", by.y="Site.Code")
# cor.test(lastFrz_trends$MKtau_Overall.Score, lastFrz_trends$mk.tau)
cor.test(lastRain_trends$MKSlope_Overall.Score, lastRain_trends$mk.slope, method="spearman")





###################################################messing around######
minmean_precip<-merge(meanmin, precip, by=c("Sample.Code", "Site.Code", "Visit.Date", "Year"))
minmean_precip$month<-format(as.Date(minmean_precip$Visit.Date, "%Y-%m-%d"), "%m")
minmean_precip<-subset(minmean_precip, month<"10")
ggplot(minmean_precip, aes(x=minAvg, y=precipdays))+geom_point()+geom_smooth()+facet_wrap(.~Site.Code)
ggplot(minmean_precip, aes(x=minAvg, y=precip))+geom_point()+geom_smooth()+facet_wrap(.~Site.Code)

ggplot(minmean_precip, aes(x=precipdays, y=precip))+geom_point()



DDpr <- readRDS("Precip.rds")

DD1<-DDpr
DD1$time<-format(as.Date(DD1$time, '%Y/%m/%d'), "%m/%d/%Y")
DD1$time_chr<-as.character(paste0(DD1$time))
DD1$time2<-as.Date(DD1$time, '%m/%d/%Y')
DD1$time_chr<-gsub("(\\D)0|^0", "\\1", DD1$time_chr)
DD1$month<-format(as.Date(DD1$time, "%m/%d/%Y"), "%m")



DD1$julian<-as.numeric(format(as.Date(DD1$time, '%m/%d/%Y'), "%j"))
lastrain<-ddply(subset(DD1, data>0&month<"08"), .(wtr_year, Site), summarize,lastrain=max(julian))

# KC_scores<-read.csv("B-IBI_results_1182023_2002-2021.csv")
# KC_scores$Visit.Date<-as.Date(KC_scores$Visit.Date, "%Y-%m-%d")
samples<-subset(KC_scores, select=c(Sample.Code,Site.Code, Visit.Date, Overall.Score)) ### make sure to fix 09DUW0277/05, incorrectly given site as 08BEA3312 in PSSB and B-IBI results
KC_scores$juliansample<-as.numeric(format(as.Date(KC_scores$Visit.Date), "%j"))

samples$Year<-as.numeric(format(as.Date(samples$Visit.Date, '%Y-%m-%d'), "%Y"))
lastrain<-inner_join(samples, lastrain, by=c("Site.Code"="Site", "Year"="wtr_year"))
ggplot(lastrain, aes(x=lastrain, y=Year))+geom_jitter()
ggplot(lastrain, aes(x=lastrain, y=Overall.Score))+geom_point()

rainduration<-merge(lastrain, precip, by=c("Sample.Code"))
ggplot(rainduration, aes(x=precipdays, y=lastrain))+geom_point()
rainduration<-merge(lastrain, meanmin, by=c("Sample.Code"))
ggplot(rainduration, aes(x=minAvg, y=lastrain))+geom_point()

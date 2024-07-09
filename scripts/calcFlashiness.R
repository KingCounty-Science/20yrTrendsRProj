# calcFlashiness takes data from the stream database and calculates several stream flashiness metrics
require('plyr')
require('dplyr')
require('data.table')
require('Rmisc')
source('G:/GreenWQA/Biota/BenthosProjects_KC/BradyProjects/wlRd-package/wlRd-package/R/wtr_yr.R')

calcFlashiness <- function(data, min.days = 360){

# remove records where D_MeanDis is NA
flow <- data[complete.cases(data$D_MeanDis),]

# convert date to 'water year'
flow <- data.frame(flow, water_year=wtr_yr(flow$D_Date, 10))

# add standard year
flow$year <- format(flow$D_Date, '%Y')

# add grouping column for site_water_year
flow$SITE_WATER_YEAR <- paste(flow$SITE_CODE, flow$water_year, sep = "_")
# remove incomplete years, where there are fewer than 360 obersvations in a water year
fl <- flow %>%
     group_by(SITE_WATER_YEAR) %>%
     filter(n() >= min.days) %>%
     as.data.frame()
# flow <- data.table(flow)[,if(.N >= min.days).SD,by=SITE_WATER_YEAR]
# # convert back to data frame
# fl <- as.data.frame(flow)

# aggregate by site and water year
fl.ag <- summarySE(measurevar = 'D_MeanDis', groupvars = c('SITE_CODE', 'G_ID', 'water_year'), data = fl, na.rm = T)

# METRICS
## I.  High Pulse Count = number of pulses per water year where the mean daily flow rate exceeds 2x the long-term mean flow rate, only needs one day below the threshold to start a new pulse
### step 1 calculate 2x long-term mean value (thus we use all values for a given site) and append it to each each row
fl <- ddply(fl, "SITE_CODE", mutate, MeanD2x = 2*mean(D_MeanDis))

# step 2 create logical representing days where flow is high ('high flow day' = 'hpd')
fl$hpd <- ifelse(fl$D_MeanDis > fl$MeanD2x, 1, 0)

# ste 3 cumulate the consecutive high flow days by summing the diff values, i.e. where flow changes from high back to high
fl <- ddply(fl, "SITE_WATER_YEAR", mutate, hpc = sum(rle(hpd)[[2]]))

# make high pulse count data frame to export as file
hpc <- aggregate(hpc ~ SITE_WATER_YEAR + SITE_CODE + water_year + G_ID, FUN = "mean", data = fl)

# II. High Pulse Duration = mean annual duration of high pulse events
# create dummy grouping variable. Ordinal numeric given to each change in status from non high pulse day to high pulse day
fl$hpg <- cumsum(c(1, diff(fl$hpd) != 0))

# get length in days of high pulse count (note this also gets length of non-low pulse counts, ie where hpd == 0, so we will subset in the next step)
fl <- ddply(fl, "hpg", mutate, hpdur = length(hpc))

# compute hpdur as the total hpd divided by the mean high pulse count
fl<- ddply(fl, "SITE_WATER_YEAR", mutate, hpdur = sum(hpd) / mean(hpc))

# compute annual mean high pulse duration, then export data frame to export as file
hpdur <- aggregate(hpdur ~ SITE_WATER_YEAR + SITE_CODE + water_year + G_ID, FUN = "mean", data = fl)

# III. High Pulse Range = number of days between the first and last pulse during the water year
# step 1 calculate high pulse range for all groups as the duration between the first and last pulse for where there is a high pulse count

# STEP 1: subset to dates with high pulse events only
# subset to high pulse days
fl.hp <- fl[fl$hpd == 1,]

# STEP 2: using date with high pulse events only (i.e. hpdur from above), compute time difference
fl.hpdur<- ddply(fl.hp, "SITE_WATER_YEAR", mutate, hpr = round(difftime(max(D_Date), min(D_Date))), 0)

# STEP 3 summarize high pulse range to site and year and export as file. Using 'mean' to aggregate, howerver all values should be identical at the water_year X SITE_CODE level. Can verify by replacing mena with var, which should be equal to 0.
#### QUESTION ### What to do about years with only one high pulse? Is the range 0, or does it go back to previous years? e.g. fl.hpdur[fl.hpdur$SITE_WATER_YEAR=="02a_2001",]
hpr <- aggregate(hpr ~ SITE_WATER_YEAR + G_ID + water_year + SITE_CODE, data = fl.hpdur, FUN = mean)

# IV. TQmean = fraction of time during a water year that the daily average flow rate is greater than the average annual flow rate of that year
# step 1 Compute and append annual average flow rate to each row
fl <- ddply(fl, "SITE_WATER_YEAR", mutate, afr = mean(D_MeanDis))

# step 2 create logical representing daily average flow > annual average flow (i.e. tqm)
fl$tqm <- ifelse(fl$D_MeanDis > fl$afr, 1, 0)

# step 3 compute proportion of aaaf
fl.tqm <- aggregate(tqm ~ SITE_WATER_YEAR + SITE_CODE + water_year, data = fl, function(x){(sum(x)/length(x))})

# V. Merge together flashiness metrics
flash<-merge(x = hpc, y = hpdur[, c('SITE_WATER_YEAR', 'hpdur')], all = T, by = 'SITE_WATER_YEAR')
flash<-merge(x = flash, y = hpr[, c('SITE_WATER_YEAR', 'hpr')], all = T, by = 'SITE_WATER_YEAR')
flash<-merge(x = flash, y = fl.tqm[, c('SITE_WATER_YEAR', 'tqm')], all = T, by = 'SITE_WATER_YEAR')

return(flash)
} # function closing brace

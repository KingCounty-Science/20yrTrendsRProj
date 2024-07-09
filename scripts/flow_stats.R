setwd(here::here())
setwd("./scripts")

if(!exists("calcFlashiness", mode="function")) source ("calcFlashiness.R")
if(!exists("RBIcalc", mode="function")) source("RBI_calc.R")


library(data.table)
library(dbplyr)
library(odbc)
library(DBI)
library(dplyr)
library(dataRetrieval)
library(stringr)
library(Rmisc)

setwd(here::here())

con <- dbConnect(odbc(), "HIC")


D_Discharge<-dbGetQuery(con,'
                        select * from tblDischargeDaily')

gaugeLLID<-dbGetQuery(con,'
                        select * from tblGaugeLLID')

gaugeLLID<-subset(gaugeLLID, select=c('G_ID','SITE_CODE','SITE_NAME','LAT','LON'))
DD<-inner_join(D_Discharge, gaugeLLID, by='G_ID')
DD$Year<-format(DD$D_Date, "%Y")
DD$Month<-format(DD$D_Date, "%m")

unique(DD$SITE_CODE)


DDsub<-subset(DD, Year>2000)
DDsub<-subset(DDsub, D_Date<="2023-01-23" )### the results of this whole analysis don't exactly match what was used in the report. The analysis originally was run 1/23/2023, but it looks like some gauge data was subsequently edited by the gagers afterwards. 
DDsub$DTYear<-format(DDsub$AutoDTStamp, "%Y")
DDsub$DTMonth<-format(DDsub$AutoDTStamp, "%m")

min.days<-360

DDsub <- DDsub %>%
  group_by(Year, G_ID) %>%
  filter(sum(!is.na(D_MeanDis)) >= min.days ) %>%
  as.data.frame()


kc_stats<-calcFlashiness(DDsub)


##Calculate and append RBI
  min.days<-360
  # remove records where D_MeanDis is NA
  flow <- DDsub[complete.cases(DDsub$D_MeanDis),]
  
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

  fl <- ddply(fl, "SITE_WATER_YEAR", mutate, rbi = RBIcalc(D_MeanDis))
rbi<-aggregate(D_MeanDis~SITE_WATER_YEAR, fl, FUN=RBIcalc)
kc_stats<-merge(x = kc_stats, y = rbi[, c('SITE_WATER_YEAR', 'D_MeanDis')], all = T, by = 'SITE_WATER_YEAR')
names(kc_stats)[names(kc_stats)=="D_MeanDis"]<-"RBI"


ddply(fl, .(SITE_CODE), summarise, n=length(unique(water_year)))
flowsYMax<-ddply(fl, .(SITE_CODE, water_year), summarise, flowsYMax2=max(D_MeanDis))
m2flow<-ddply(fl, .(SITE_CODE), summarise, meanflow=mean(D_MeanDis))
maxmeanQ<-merge(flowsYMax, m2flow, by="SITE_CODE")
maxmeanQ$maxtomeanQ<-maxmeanQ$flowsYMax2/maxmeanQ$meanflow
maxmeanQ$SITE_WATER_YEAR <- paste(maxmeanQ$SITE_CODE, maxmeanQ$water_year, sep = "_")
kc_stats<-merge(x = kc_stats, y = maxmeanQ[, c('SITE_WATER_YEAR', 'maxtomeanQ')], all = T, by = 'SITE_WATER_YEAR')

library(zoo)

minrollmean<-ddply(fl, .(water_year, SITE_CODE), summarize, minrmean=min(rollapply(D_MeanDis, 60, mean, align='center', partial=TRUE)))
# mflow<-ddply(flowsDMean2, .(G_ID), summarize, meanflow=mean(D_MeanDis))
mflow<-ddply(minrollmean, .(SITE_CODE), summarize, meanbaseflow=mean(minrmean))
lfi<-merge(minrollmean, mflow, by="SITE_CODE")
lfi$LFI<-lfi$minrmean/lfi$meanbaseflow
lfi$SITE_WATER_YEAR <- paste(lfi$SITE_CODE, lfi$water_year, sep = "_")
kc_stats<-merge(x = kc_stats, y = lfi[, c('SITE_WATER_YEAR', 'LFI')], all = T, by = 'SITE_WATER_YEAR')


length_of_record<-ddply(kc_stats, .(SITE_CODE), summarize, length_of_record=length(unique(water_year)))
kc_stats<-merge(kc_stats, length_of_record, by="SITE_CODE")
kc_stats_filter<-subset(kc_stats, length_of_record>9)
unique(kc_stats$SITE_CODE)

# ggplot(kc_stats_filter, aes(x=water_year, y=hpc, group=water_year))+geom_boxplot()
# ggplot(kc_stats_filter, aes(x=water_year, y=hpdur, group=water_year))+geom_boxplot()
# ggplot(kc_stats_filter, aes(x=water_year, y=hpr, group=water_year))+geom_boxplot()
# ggplot(kc_stats_filter, aes(x=water_year, y=tqm, group=water_year))+geom_boxplot()
# ggplot(kc_stats_filter, aes(x=water_year, y=RBI, group=water_year))+geom_boxplot()
# ggplot(kc_stats_filter, aes(x=water_year, y=maxtomeanQ, group=water_year))+geom_boxplot()
# ggplot(kc_stats_filter, aes(x=water_year, y=LFI, group=water_year))+geom_boxplot()
# 
# l=unique(c(as.character(kc_stats_filter$G_ID)))
# kc_stats_filter$Site.Number<-as.numeric(factor(kc_stats_filter$G_ID, levels=l))
# kc_stats_filter$Year<-as.numeric(paste0(kc_stats_filter$water_year))
# kc_stats_filter$hprange<-as.numeric(paste0(kc_stats_filter$hpr))
# 
# 
# library(rkt)
# with(kc_stats_filter, rkt(date = Year, y = hpc, block = Site.Number, correct = FALSE, rep="a"))
# with(kc_stats_filter, rkt(date = Year, y = hpdur, block = Site.Number, correct = FALSE, rep="a"))
# with(kc_stats_filter, rkt(date = Year, y = hprange, block = Site.Number, correct = FALSE, rep="a"))
# with(kc_stats_filter, rkt(date = Year, y = tqm, block = Site.Number, correct = FALSE, rep="a"))
# with(kc_stats_filter, rkt(date = Year, y = RBI, block = Site.Number, correct = FALSE, rep="a"))
# with(kc_stats_filter, rkt(date = Year, y = maxtomeanQ, block = Site.Number, correct = FALSE, rep="a"))
# with(kc_stats_filter, rkt(date = Year, y = LFI, block = Site.Number, correct = FALSE, rep="a"))


#####################USGS discharge##########
USGS <- readNWISdv(siteNumbers=c("12099600",
                                     "12109500",
                                     "12112600",
                                     "12113347",
                                     "12118500",
                                     "12119500",
                                     "12119600",
                                     "12121700",
                                     "12121815",
                                     "12121830"
                                     ), #annual stats
                       parameterCd=c("00060"), "","") 
USGS<-renameNWISColumns(USGS)

USGS2 <- readNWISdv(siteNumbers=c("12126000",
                                  12121600,
                                 "12126100",
                                 "12141500",
                                 "12144000",
                                 "12147000",
                                 "12147470"
), #annual stats
parameterCd=c("00060"), "","") 
USGS2<-renameNWISColumns(USGS2)
USGS<-rbind(USGS, USGS2)
rm(USGS2)
names(USGS)<-c("Agency", "SITE_CODE", "D_Date", "D_MeanDis", "Flow_cd")
USGS$D_Date<-as.Date(USGS$D_Date, "%Y-%m-%d")
USGS$G_ID<-USGS$SITE_CODE
USGS$Year<-format(USGS$D_Date, "%Y")
USGS<-subset(USGS, Year>2000)
USGS<-subset(USGS, D_Date<"2023-01-23")
USGS_stats<-calcFlashiness(USGS, 360)

min.days<-360 
# remove records where D_MeanDis is NA
flow <- USGS[complete.cases(USGS$D_MeanDis),]

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

fl <- ddply(fl, "SITE_WATER_YEAR", mutate, rbi = RBIcalc(D_MeanDis))
rbi<-aggregate(D_MeanDis~SITE_WATER_YEAR, fl, FUN=RBIcalc)
USGS_stats<-merge(x = USGS_stats, y = rbi[, c('SITE_WATER_YEAR', 'D_MeanDis')], all = T, by = 'SITE_WATER_YEAR')
names(USGS_stats)[names(USGS_stats)=="D_MeanDis"]<-"RBI"

ddply(fl, .(SITE_CODE), summarise, n=length(unique(water_year)))
flowsYMax<-ddply(fl, .(SITE_CODE, water_year), summarise, flowsYMax2=max(D_MeanDis))
m2flow<-ddply(fl, .(SITE_CODE), summarise, meanflow=mean(D_MeanDis))
maxmeanQ<-merge(flowsYMax, m2flow, by="SITE_CODE")
maxmeanQ$maxtomeanQ<-maxmeanQ$flowsYMax2/maxmeanQ$meanflow
maxmeanQ$SITE_WATER_YEAR <- paste(maxmeanQ$SITE_CODE, maxmeanQ$water_year, sep = "_")
USGS_stats<-merge(x = USGS_stats, y = maxmeanQ[, c('SITE_WATER_YEAR', 'maxtomeanQ')], all = T, by = 'SITE_WATER_YEAR')

library(zoo)

minrollmean<-ddply(fl, .(water_year, SITE_CODE), summarize, minrmean=min(rollapply(D_MeanDis, 60, mean, align='center', partial=TRUE)))
# mflow<-ddply(flowsDMean2, .(G_ID), summarize, meanflow=mean(D_MeanDis))
mflow<-ddply(minrollmean, .(SITE_CODE), summarize, meanbaseflow=mean(minrmean))
lfi<-merge(minrollmean, mflow, by="SITE_CODE")
lfi$LFI<-lfi$minrmean/lfi$meanbaseflow
lfi$SITE_WATER_YEAR <- paste(lfi$SITE_CODE, lfi$water_year, sep = "_")
USGS_stats<-merge(x = USGS_stats, y = lfi[, c('SITE_WATER_YEAR', 'LFI')], all = T, by = 'SITE_WATER_YEAR')


length_of_record<-ddply(USGS_stats, .(SITE_CODE), summarize, length_of_record=length(unique(water_year)))
USGS_stats<-merge(USGS_stats, length_of_record, by="SITE_CODE")
USGS_stats_filter<-subset(USGS_stats, length_of_record>9)
unique(USGS_stats_filter$SITE_CODE)


####add em all together####
names(kc_stats)
names(USGS_stats)

flow_stats<-rbind(kc_stats, USGS_stats)
length_of_record<-ddply(flow_stats, .(SITE_CODE), summarize, length_of_record=length(unique(water_year)))
flow_stats<-merge(flow_stats, length_of_record, by="SITE_CODE")
flow_stats_filter<-subset(flow_stats, length_of_record.y>9)


library(ggplot2)
ggplot(flow_stats_filter, aes(x=water_year, y=hpc, group=water_year))+geom_boxplot()
ggplot(flow_stats_filter, aes(x=water_year, y=hpdur, group=water_year))+geom_boxplot()
ggplot(flow_stats_filter, aes(x=water_year, y=hpr, group=water_year))+geom_boxplot()
ggplot(flow_stats_filter, aes(x=water_year, y=tqm, group=water_year))+geom_boxplot()
ggplot(flow_stats_filter, aes(x=water_year, y=RBI, group=water_year))+geom_boxplot()
ggplot(flow_stats_filter, aes(x=water_year, y=maxtomeanQ, group=water_year))+geom_boxplot()
ggplot(flow_stats_filter, aes(x=water_year, y=LFI, group=water_year))+geom_boxplot()

l=unique(c(as.character(flow_stats_filter$G_ID)))
flow_stats_filter$Site.Number<-as.numeric(factor(flow_stats_filter$G_ID, levels=l))
flow_stats_filter$Year<-as.numeric(paste0(flow_stats_filter$water_year))
flow_stats_filter$hprange<-as.numeric(paste0(flow_stats_filter$hpr))

unique(flow_stats_filter$SITE_CODE)

library(rkt)
with(flow_stats_filter, rkt(date = Year, y = hpc, block = Site.Number, correct = FALSE, rep="a"))
with(flow_stats_filter, rkt(date = Year, y = hpdur, block = Site.Number, correct = FALSE, rep="a"))
with(flow_stats_filter, rkt(date = Year, y = hprange, block = Site.Number, correct = FALSE, rep="a"))
with(flow_stats_filter, rkt(date = Year, y = tqm, block = Site.Number, correct = FALSE, rep="a"))
with(flow_stats_filter, rkt(date = Year, y = RBI, block = Site.Number, correct = FALSE, rep="a"))
with(flow_stats_filter, rkt(date = Year, y = maxtomeanQ, block = Site.Number, correct = FALSE, rep="a"))
with(flow_stats_filter, rkt(date = Year, y = LFI, block = Site.Number, correct = FALSE, rep="a"))

##filter the results by the lookup table. This table is generated from ArcGIS, using network analyst to identify discharge gauges within 1/2 mile of a site
setwd(here::here())
setwd("./inputs")

lookup<-read.csv("flow_gauges.csv", header=T)
setwd(here::here())
setwd("./outputs")

lookup[1,1]<-"MAY1"
flow_stats_filt<-inner_join(flow_stats_filter, lookup, by=c("SITE_CODE"="Gauge"), multiple="all")##keep only gauges that are matched to a site in the lookup table
unique(flow_stats_filt$Site)
setdiff(levels(flow_stats_filt$Site), flow_stats_filt$Site)## These were culled for not having enough data within the water year
flow_stats_filt$SampleID<-paste0(flow_stats_filt$Site, "_", str_sub(flow_stats_filt$water_year, 3,4))
write.csv(flow_stats_filt, 'flow_metrics01232023.csv')
flow_stats_filt<-read.csv('flow_metrics01232023.csv')
# check<-read.csv('flow_metrics01232023.csv')
# unique(check$SITE_CODE)

flow_stats_filt<-subset(flow_stats_filt, water_year>2001)

length_of_record<-ddply(flow_stats_filt, .(SITE_CODE), summarize, length_of_record=length(unique(water_year)))
flow_stats_filt<-merge(flow_stats_filt, length_of_record, by="SITE_CODE")
flow_stats_filt<-subset(flow_stats_filt, length_of_record>9)
unique(flow_stats_filt$Site)

# l=unique(c(as.character(flow_stats_filt$G_ID)))
# flow_stats_filt$Site.Number<-as.numeric(factor(flow_stats_filt$G_ID, levels=l))
# flow_stats_filt$Year<-as.numeric(paste0(flow_stats_filt$water_year))
# flow_stats_filt$hprange<-as.numeric(paste0(flow_stats_filt$hpr))
# library(rkt)
# with(flow_stats_filt, rkt(date = Year, y = hpc, block = Site.Number, correct = FALSE, rep="a"))
# with(flow_stats_filt, rkt(date = Year, y = hpdur, block = Site.Number, correct = FALSE, rep="a"))
# with(flow_stats_filt, rkt(date = Year, y = hprange, block = Site.Number, correct = FALSE, rep="a"))
# with(flow_stats_filt, rkt(date = Year, y = tqm, block = Site.Number, correct = FALSE, rep="a"))
# with(flow_stats_filt, rkt(date = Year, y = RBI, block = Site.Number, correct = FALSE, rep="a"))
# with(flow_stats_filt, rkt(date = Year, y = maxtomeanQ, block = Site.Number, correct = FALSE, rep="a"))
# with(flow_stats_filt, rkt(date = Year, y = LFI, block = Site.Number, correct = FALSE, rep="a"))


# ddply(flow_stats_filt, .(water_year), summarize, length(G_ID))
# ggplot(flow_stats_filt, aes(x=water_year, y=hpc, group=water_year))+geom_boxplot()
# ggplot(flow_stats_filt, aes(x=water_year, y=hpdur, group=water_year))+geom_boxplot()
# ggplot(flow_stats_filt, aes(x=water_year, y=hpr, group=water_year))+geom_boxplot()
# ggplot(flow_stats_filt, aes(x=water_year, y=tqm, group=water_year))+geom_boxplot()
# ggplot(flow_stats_filt, aes(x=water_year, y=RBI, group=water_year))+geom_boxplot()


KC_scores<-read.csv("B-IBI_results_03082023_2002-2021_NoUnique_subsamp_450_LR_exclude.csv")
KC_scores<-subset(KC_scores, Tot_Abund>=450)
KC_trends<-read.csv("Site_Trends_coarse_LR_03092023_2002-2021_LRexclude.csv")
KC_trends<-subset(KC_trends, !(Site.Code=="09DUW0277"&Stream.or.River=="Riverton Creek (003D)"))
KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))
KC_scores<-subset(KC_scores, Site.Code %in% KC_trends$Site.Code)

samples<-subset(KC_scores, select=c(Sample.Code,Site.Code, Visit.Date, Overall.Score)) ### make sure to fix 09DUW0277/05, incorrectly given site as 08BEA3312 in PSSB and B-IBI results
samples$Year<-as.numeric(format(as.Date(samples$Visit.Date, '%Y-%m-%d'), "%Y"))
flows_stats_scores<-inner_join(samples, flow_stats_filt, by=c("Site.Code"="Site", "Year"="Year"))
flows_stats_scores$hprange<-as.numeric(paste0(flows_stats_scores$hpr))

unique(flows_stats_scores$Site.Code)

cor.test(flows_stats_scores$Overall.Score, flows_stats_scores$hpc, method="spearman")
cor.test(flows_stats_scores$Overall.Score, flows_stats_scores$hpdur, method="spearman")
cor.test(flows_stats_scores$Overall.Score, flows_stats_scores$hprange, method="spearman")
cor.test(flows_stats_scores$Overall.Score, flows_stats_scores$tqm, method="spearman")
ggplot(flows_stats_scores, aes(y=Overall.Score, x=tqm))+geom_point()
cor.test(flows_stats_scores$Overall.Score, flows_stats_scores$RBI, method="spearman")
ggplot(flows_stats_scores, aes(y=Overall.Score, x=RBI))+geom_point()
cor.test(flows_stats_scores$Overall.Score, flows_stats_scores$maxtomeanQ, method="spearman")
ggplot(flows_stats_scores, aes(y=Overall.Score, x=maxtomeanQ))+geom_point()
cor.test(flows_stats_scores$Overall.Score, flows_stats_scores$LFI, method="spearman")




list<-c("hpc", "hpdur", "hprange", "tqm", "RBI", "maxtomeanQ", "LFI")
for (each in list){
  flow_stats_filt$rkt.pval <- with(flow_stats_filt, rkt(date = Year, y = get(each), block = Site.Number, correct = FALSE, rep="a")[1])
  flow_stats_filt$rkt.Sen <- with(flow_stats_filt, rkt(date = Year, y = get(each), block = Site.Number, correct = FALSE, rep="a")[3])
  flow_stats_filt$rkt.tau <- with(flow_stats_filt, rkt(date = Year, y = get(each), block = Site.Number, correct = FALSE, rep="a")[12])
  flow_stats_filt$RKTtrend <- with(flow_stats_filt, ifelse(rkt.pval < 0.05 & rkt.tau < 0, "negative", ifelse(rkt.pval < 0.05 & rkt.tau > 0, "positive", "none")))
    colnames(flow_stats_filt)[ncol(flow_stats_filt)-3]<-paste0("rkt.pval_", each)
    colnames(flow_stats_filt)[ncol(flow_stats_filt)-2]<-paste0("rkt.Sen_", each)
    colnames(flow_stats_filt)[ncol(flow_stats_filt)-1]<-paste0("rkt.tau_", each)
    colnames(flow_stats_filt)[ncol(flow_stats_filt)]<-paste0("RKTtrend_", each)
}
names(flow_stats_filt)
results<-unique(flow_stats_filt[,c(20:47)])
results<-as.matrix(results)
write.csv(results, "nearbygauges.csv")

flow_stats_filt<-flow_stats_filt[,c(1:19)]
any(is.na(flow_stats_filt))
flow_stats_filt[is.na(flow_stats_filt$hpdur),"hpdur"]<-c(0,0,0)
flow_stats_filt[is.na(flow_stats_filt$hprange),"hprange"]<-c(0,0,0)
# flow_stats_filt$Year<-as.numeric(paste0(flow_stats_filt$water_year))
# flow_stats_filt$hprange<-as.numeric(paste0(flow_stats_filt$hpr))
scal<-"SITE_CODE" # "Site.Code" or "Subbasin"

# with( subset(flow_stats_filt, Site=="08BEA3571"), summary(lm(get(paste0(var)) ~ as.numeric(Year))))[[4]][8]
library(EnvStats)
for (var in list) {
  
  flow_stats_filt<- ddply(flow_stats_filt, .variables =scal, mutate, slope =lm(get(paste0(var)) ~ as.numeric(Year))[[1]][2])
  flow_stats_filt<-ddply(flow_stats_filt, .variables =scal, mutate, pval = summary(lm(get(paste0(var)) ~ as.numeric(Year)))[[4]][8])
  flow_stats_filt$trend <- ifelse(flow_stats_filt$pval < 0.05 & flow_stats_filt$slope < 0, "negative", ifelse(flow_stats_filt$pval < 0.05 & flow_stats_filt$slope > 0, "positive", "none")) # add trend col
  flow_stats_filt<-ddply(flow_stats_filt,.(get(scal)),mutate, mk.tau=kendallTrendTest( get(paste0(var))~ as.numeric(Year))$estimate[1])
  # bibi.lr<-ddply(bibi.lr,.(get(scal)),mutate, mk.tau=kendallTrendTest(as.formula(paste0(colnames(bibi.lr[i]), " ~ ", "as.numeric(Year)")))$estimate[1])
  flow_stats_filt<-ddply(flow_stats_filt,.(get(scal)),mutate, mk.Sen=kendallTrendTest( get(paste0(var))~ as.numeric(Year))$estimate[2])
  flow_stats_filt<-ddply(flow_stats_filt,.(get(scal)),mutate, mk.pval=kendallTrendTest( get(paste0(var))~ as.numeric(Year))$p.value)
  flow_stats_filt<-ddply(flow_stats_filt,.(get(scal)),mutate, mk.inter=kendallTrendTest( get(paste0(var))~ as.numeric(Year))$estimate[3])
  flow_stats_filt$MKtrend <- ifelse(flow_stats_filt$mk.pval < 0.05 & flow_stats_filt$mk.tau < 0, "negative", ifelse(flow_stats_filt$mk.pval < 0.05 & flow_stats_filt$mk.tau > 0, "positive", "none"))
  colnames(flow_stats_filt)[ncol(flow_stats_filt)-7]<-paste0("slope_",var)
  colnames(flow_stats_filt)[ncol(flow_stats_filt)-6]<-paste0("LRpval_", var)
  colnames(flow_stats_filt)[ncol(flow_stats_filt)-5]<-paste0("LRtrend_", var)
  colnames(flow_stats_filt)[ncol(flow_stats_filt)-4]<-paste0("MKtau_", var)
  colnames(flow_stats_filt)[ncol(flow_stats_filt)-3]<-paste0("MKSlope_", var)
  colnames(flow_stats_filt)[ncol(flow_stats_filt)-2]<-paste0("MKpval_", var)
  colnames(flow_stats_filt)[ncol(flow_stats_filt)-1]<-paste0("MKinter_", var)
  colnames(flow_stats_filt)[ncol(flow_stats_filt)]<-paste0("MKtrend_", var)
}

flow_stats_filt<-ddply(flow_stats_filt, .variables =scal, mutate, Nyears= length(unique(Year)))

names(flow_stats_filt)
unique(flow_stats_filt$`get(scal)`)
overall_trends<-unique(flow_stats_filt[c(19,21:(ncol(flow_stats_filt)))])
write.csv(overall_trends, "nearbygauges_sites.csv")
# 
# ggplot(flow_stats_filt, aes(x=water_year, y=hpc, group=water_year))+geom_point()+facet_wrap(.~Site)
# ggsave("hpc.png")
# ggplot(flow_stats_filt, aes(x=water_year, y=hpdur, group=water_year))+geom_point()+facet_wrap(.~Site)
# ggsave("hpdur.png")
# ggplot(flow_stats_filt, aes(x=water_year, y=hpr, group=water_year))+geom_point()+facet_wrap(.~Site)
# ggsave("hpr.png")
# ggplot(flow_stats_filt, aes(x=water_year, y=tqm, group=water_year))+geom_point()+facet_wrap(.~Site)
# ggsave("tqm.png")
# ggplot(flow_stats_filt, aes(x=water_year, y=RBI, group=water_year))+geom_point()+facet_wrap(.~Site)
# ggsave("RBI.png")


# flow_site_trends<-unique(flow_stats_filt[,c(2, 15, 18, 21:60, 101:116)])


overall_trends<-read.csv("nearbygauges_sites.csv")

flow_score_trends<-merge(KC_trends, overall_trends,  by.x="Site.Code", by.y="Site")
summary(flow_score_trends$MKSlope_hpc)
summary(flow_score_trends$MKSlope_hpdur)
summary(flow_score_trends$MKSlope_hprange)
summary(flow_score_trends$MKSlope_tqm)
summary(flow_score_trends$MKSlope_RBI)
summary(flow_score_trends$MKSlope_maxtomeanQ)
summary(flow_score_trends$MKSlope_LFI)

unique(flow_score_trends$Site.Code)

# cor.test(flow_score_trends$MKtau_Overall.Score, flow_score_trends$MKtau_hpc)
cor.test(flow_score_trends$MKSlope_Overall.Score, flow_score_trends$MKSlope_hpc, method = "spearman")
plot(flow_score_trends$MKSlope_Overall.Score, flow_score_trends$MKSlope_hpc)

# cor.test(flow_score_trends$MKtau_Overall.Score, flow_score_trends$MKtau_hpdur)
cor.test(flow_score_trends$MKSlope_Overall.Score, flow_score_trends$MKSlope_hpdur, method = "spearman")

# cor.test(flow_score_trends$MKtau_Overall.Score, flow_score_trends$MKtau_hprange)
cor.test(flow_score_trends$MKSlope_Overall.Score, flow_score_trends$MKSlope_hprange, method = "spearman")

# cor.test(flow_score_trends$MKtau_Overall.Score, flow_score_trends$MKtau_tqm)
cor.test(flow_score_trends$MKSlope_Overall.Score, flow_score_trends$MKSlope_tqm, method = "spearman")

# cor.test(flow_score_trends$MKtau_Overall.Score, flow_score_trends$MKtau_RBI)
cor.test(flow_score_trends$MKSlope_Overall.Score, flow_score_trends$MKSlope_RBI, method = "spearman")

# cor.test(flow_score_trends$MKtau_Overall.Score, flow_score_trends$MKtau_maxtomeanQ)
cor.test(flow_score_trends$MKSlope_Overall.Score, flow_score_trends$MKSlope_maxtomeanQ, method = "spearman")

# cor.test(flow_score_trends$MKtau_Overall.Score, flow_score_trends$MKtau_LFI)
cor.test(flow_score_trends$MKSlope_Overall.Score, flow_score_trends$MKSlope_LFI, method = "spearman")

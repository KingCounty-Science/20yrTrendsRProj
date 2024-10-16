
setwd(here::here())
setwd("./outputs")

library(data.table)
library(dbplyr)
library(odbc)
library(DBI)
library(dplyr)
library(stringr)
library(sf)

con <- dbConnect(odbc(), "bibihabitat")
dbListTables(con)
dbListTables(con)
column.types <- dbGetQuery(con, "SELECT COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME='SAMPLE'")
ct <- column.types %>%
  mutate(cml = case_when(
    is.na(CHARACTER_MAXIMUM_LENGTH) ~ 10,
    CHARACTER_MAXIMUM_LENGTH == -1 ~ 100000,
    TRUE ~ as.double(CHARACTER_MAXIMUM_LENGTH)
  )
  ) %>%
  arrange(cml) %>%
  pull(COLUMN_NAME)

fields <- paste(ct, collapse=", ")
query <- paste("SELECT", fields, "FROM SAMPLE")

Sample<-dbGetQuery(con, query)
# Sample<-dbGetQuery(con,' select * from SAMPLE')
Sample$IsReplicateSample[Sample$SiteName=="08BEA3747"&Sample$OBJECTID=="16804"]<-"Yes"##Didn't mark it as replicate in the form
Sample$IsReplicateSample[Sample$SiteName=="08BEA3312"&Sample$OBJECTID=="42405"]<-"Yes"## was sampled twice in 2021, marking this as a replicate.
Sample$year<-year(Sample$SampleDate)
Sample<-subset(Sample, !is.na(year))
replicates<-subset(Sample, IsReplicateSample=="Yes")#### In some cases, both data sheets were being marked as replicates, when only one should be. Takes a few lines to clean up
reps1<-replicates[duplicated(replicates[, c("SiteName", "year")]),]
reps2<-replicates[duplicated(replicates[, c("SiteName", "year")], fromLast = T),]
intersect(Sample[Sample$IsReplicateSample=="No",c("SiteName", "year"),], reps2[,c("SiteName", "year")])
intersect(Sample[Sample$IsReplicateSample=="No",c("SiteName", "year"),], reps1[,c("SiteName", "year")])
Sample[Sample$SiteName=="07LMS033235"&Sample$year=="2021",]
reps2<-subset(reps2, OBJECTID!="60405")
Sample[Sample$OBJECTID %in% reps2$OBJECTID,"IsReplicateSample"]<-"No"
Sample<-subset(Sample, IsReplicateSample=="No")
Sample<-subset(Sample, IsSampleTaken=="Yes")
Sample[Sample$OBJECTID=="12001","SiteName"]<-"07LNT163403"
dups<-Sample[which(duplicated(subset(Sample, select=c(SiteName, year)))),]
dups2<-Sample[which(duplicated(subset(Sample, select=c(SiteName, year)), fromLast=T)),]
Sample$SampleName<-paste0(Sample$SiteName, "_", str_sub(Sample$year, 3, 4))
Sample<-subset(Sample, !OBJECTID=="74022")### habitat for this site entered twice, this one had an error in the date
Sample<-subset(Sample, !OBJECTID=="54806")## habitat for this site entered twice, this one was entered first, maybe had error that was fixed in second entry?
Sample[which(Sample$SiteName=="07LNT075179"&Sample$year=="2021"),]
Sample[Sample$OBJECTID=="65221","SiteName"]<-"07LNT075179"
Sample[Sample$OBJECTID=="65221","SampleName"]<-"07LNT075179_21"
dups<-Sample[which(duplicated(subset(Sample, select=c(SiteName, year)))),]
dups2<-Sample[which(duplicated(subset(Sample, select=c(SiteName, year)), fromLast=T)),]
Sample[Sample$SiteName=="VashShin","SiteName"]<-"VashShing"
# test<-Sample[str_detect(Sample$Notes, "mussel"),]
# subset(test, year==2021)$SiteName
Sample<-subset(Sample, year<2022)

invert<-dbGetQuery(con,' select * from INVERTEBRATE')
invert_join<-inner_join(invert, Sample, by=c("SampleGUID"="GlobalID"))
subset(invert_join, year==2021&InvertebrateName=="Mussel")$SiteName



pools<-subset(Sample, select=c(SiteName, SampleDate,NumberOfPools, ReachLength, SampleName))

lwd<-subset(Sample, select=c(SiteName, SampleDate,LargeWoodyDebrisNumber, SampleName))


depth<-dbGetQuery(con,'select * from RIFFLEDEPTH')
depth_join<-inner_join(depth, Sample, by=c("SampleGUID"="GlobalID"))
RDepth<-aggregate(RiffleDepth~SiteName+ SampleName+SampleDate, depth_join, FUN=mean)
RDepth$RiffleDepth<-RDepth$RiffleDepth/2.54##NEED TO CONVERT 2018 from CM to IN


stability<-dbGetQuery(con,' select * from STREAMBANKSTABILITY')
stabilityjoin<-inner_join(stability, Sample, by=c("SampleGUID"="GlobalID"))
stability<-aggregate(StreamBankStability~BankType+SiteName+ SampleName+SampleDate, stabilityjoin, FUN=mean)
add<-subset(Sample[which(!Sample$SampleName %in% stability$SampleName),][1,], select=c("SiteName", "SampleName", "SampleDate"))
add$StreamBankStability<-2
add$BankType<-"Right Bank"
add[2,]<-add[1,]
add[2,"BankType"]<-"Left Bank"
add[2, "StreamBankStability"]<-1
# add$StreamBankStability<-as.integer(add$StreamBankStability)
subset(stability, SiteName=="08EAS1536")
stability<-rbind(stability, add)

canopy<-dbGetQuery(con,'select * from CANOPYMEASUREMENT')
canopyjoin<-inner_join(canopy, Sample, by=c("SampleGUID"="GlobalID"))
canopy<-aggregate(US+RB+LB+DS~SiteName+ SampleName+SampleDate, canopyjoin, FUN=mean)
subset(Sample[which(!Sample$SampleName %in% canopy$SampleName),][1,], select=c("SiteName", "SampleName", "SampleDate"))

pebble<-dbGetQuery(con,'select * from PEBBLECOUNT')
pebble<-inner_join(pebble, Sample, by=c("SampleGUID"="GlobalID"))
pebble<-subset(pebble, select=c("SiteName","SampleName", "OBJECTID.x",str_subset(names(pebble), "SubstrateClass")))
add<-subset(Sample[which(!Sample$SampleName %in% pebble$SampleName),], select=c("SiteName", "SampleName", "SampleDate", "OBJECTID"))
names(pebble)
add$ASubstrateClass<-c(0,NA,NA,NA,NA,NA)
add$BSubstrateClass<-c(0,NA,NA,NA,NA,NA)
add$CSubstrateClass<-c(1,NA,NA,NA,NA,NA)
add$DSubstrateClass<-c(4,NA,NA,NA,NA,NA)
add$ESubstrateClass<-c(0,NA,NA,NA,NA,NA)
add$FSubstrateClass<-c(4,NA,NA,NA,NA,NA)
add$GSubstrateClass<-c(4,NA,NA,NA,NA,NA)
add$HSubstrateClass<-c(5,NA,NA,NA,NA,NA)
add$ISubstrateClass<-c(10,NA,NA,NA,NA,NA)
add$JSubstrateClass<-c(16,NA,NA,NA,NA,NA)
add$KSubstrateClass<-c(10,NA,NA,NA,NA,NA)
add$LSubstrateClass<-c(16,NA,NA,NA,NA,NA)
add$MSubstrateClass<-c(20,NA,NA,NA,NA,NA)
add$NSubstrateClass<-c(17,NA,NA,NA,NA,NA)
add$OSubstrateClass<-c(10,NA,NA,NA,NA,NA)
add$PSubstrateClass<-c(2,NA,NA,NA,NA,NA)
add$QSubstrateClass<-c(0,NA,NA,NA,NA,NA)
add$RSubstrateClass<-c(0,NA,NA,NA,NA,NA)
names(add)[4]<-"OBJECTID.x"
add<-subset(add, select=!names(add) %in% "SampleDate")
pebble<-rbind(pebble, add)
# names(pebble)[1:3]<-c("SITE_ID", "SAMPLE_NUMBER", "SampleID")

reach<-dbGetQuery(con,'select * from REACH')
reach_join<-inner_join(reach, Sample, by=c("SampleGUID"="GlobalID"))
WW<-aggregate(WettedWidth~SiteName+SampleName+SampleDate, reach_join, FUN=mean)
WW$WettedWidth<-WW$WettedWidth*3.281 # convert to feet from meters
Sample[which(!Sample$SampleName %in% WW$SampleName),]$Notes
Sample[which(!Sample$SampleName %in% WW$SampleName),c("SiteName", "year")]#### all these did not have transects done

BF<-aggregate(BankfulWidth~SiteName+SampleName+SampleDate, reach_join, FUN=mean)
BF$BankfulWidth<-BF$BankfulWidth*3.281 # convert to feet from meters
Sample[which(!Sample$SampleName %in% BF$SampleName),c("SiteName", "year")]###all these had incomplete or not taken transects

BFD<-aggregate(BankfulDepth~SiteName+SampleName+SampleDate, reach_join, FUN=mean)
BFD$BankfulDepth<-BFD$BankfulDepth/2.54 # convert to inches
Sample[which(!Sample$SampleName %in% BFD$SampleName),c("SiteName", "year")]###all these had incomplete or not taken transects

TD<-aggregate(ThalwegDepth~SiteName+SampleName+SampleDate, reach_join, FUN=mean)
TD$ThalwegDepth<-TD$ThalwegDepth/2.54 # convert to inches
Sample[which(!Sample$SampleName %in% TD$SampleName),c("SiteName", "year")]##all these had incomplete or not taken transects

flowtime<-dbGetQuery(con,'select * from RIFFLEFLOWTIME')
flowt_join<-inner_join(flowtime, Sample, by=c("SampleGUID"="GlobalID"))
Flowspeed<-aggregate(cbind(RiffleFlowTime,ReachFlowDistance)~SiteName+SampleName+SampleDate, flowt_join, FUN=mean)
Flowspeed$ReachFlowDistance<-Flowspeed$ReachFlowDistance*3.28
Flowspeed$speed<-Flowspeed$ReachFlowDistance/Flowspeed$RiffleFlowTime##ft/sec
Sample[which(!Sample$SampleName %in% Flowspeed$SampleName),c("SiteName", "year")]
add<-Sample[which(!Sample$SampleName %in% Flowspeed$SampleName),c("SiteName", "SampleName", "SampleDate")][2,]
add$RiffleFlowTime<-mean(1.25, 1.42, 1.31)
add$ReachFlowDistance<-1.5*3.28
add$speed<-add$ReachFlowDistance/add$RiffleFlowTime
Flowspeed<-rbind(Flowspeed, add)
rm(flowtime,flowt_join,reach_join,reach,canopyjoin,stabilityjoin,depth_join,depth,Sample, add)

sample2017<-st_read(dsn='G:/GreenWQA/Biota/GIS/ArcCollector_habitat_data_2017/BIBIHabitat_June112018.gdb', layer="Sample")
sample2017<-subset(sample2017, year(sample2017$SampleDate)=="2017")
sample2017$year<-year(sample2017$SampleDate)
sample2017<-subset(sample2017, !is.na(year))

replicates<-subset(sample2017, IsReplicateSample=="Yes")#### In some cases, both data sheets were being marked as replicates, when only one should be. Takes a few lines to clean up
reps1<-replicates[duplicated(replicates[, c("SiteName", "year")]),]
reps2<-replicates[duplicated(replicates[, c("SiteName", "year")], fromLast = T),]
sample2017[sample2017$SiteName=="08CED2711","IsReplicateSample"]<-"No"
sample2017[sample2017$SiteName=="08CED4192","IsReplicateSample"]<-"No"
sample2017[sample2017$SiteName=="08ISS4294","IsReplicateSample"]<-"No"
sample2017[sample2017$SiteName=="08ISS4730","IsReplicateSample"]<-"No"
sample2017[sample2017$SiteName=="08LAK2827","IsReplicateSample"]<-"No"
sample2017[sample2017$SiteName=="08LAK3627","IsReplicateSample"]<-"No"
sample2017[sample2017$SiteName=="08LAK3699","IsReplicateSample"]<-"No"
sample2017[sample2017$SiteName=="08LIT2602","IsReplicateSample"]<-"No"
sample2017[sample2017$SiteName=="08LIT2603","IsReplicateSample"]<-"No"
sample2017[sample2017$SiteName=="08NOR1370","IsReplicateSample"]<-"No"
sample2017[sample2017$SiteName=="08NOR1750","IsReplicateSample"]<-"No"
sample2017[sample2017$SiteName=="08SAM2862","IsReplicateSample"]<-"No"
sample2017[sample2017$SiteName=="09BLA0722","IsReplicateSample"]<-"No"
sample2017[sample2017$SiteName=="09BLA0768","IsReplicateSample"]<-"No"
sample2017[sample2017$SiteName=="09SOO1130","IsReplicateSample"]<-"No"
sample2017[sample2017$SiteName=="VashShin","SiteName"]<-"VashShing"
sample2017<-subset(sample2017, IsReplicateSample=="No")
sample2017<-subset(sample2017, IsSampleTaken=="Yes")
sample2017[which(duplicated(subset(sample2017, select=c(SiteName, year)))),]
sample2017$SampleName<-paste0(sample2017$SiteName, "_", str_sub(sample2017$year, 3, 4))

# Soos<-subset(sample2017, str_detect(sample2017$SiteName, "09SOO")|str_detect(sample2017$SiteName, "09JEN")|str_detect(sample2017$SiteName, "09COV"), select=c(SiteName, SampleDate, Notes, GlobalID))
# Riparian<-st_read(dsn='G:/GreenWQA/Biota/GIS/ArcCollector_habitat_data_2017/BIBIHabitat_June112018.gdb', layer="RiparianCondition")
# Riparianjoin17<-left_join( Soos,Riparian, by=c("GlobalID"="SampleGUID"))
# st_geometry(Riparianjoin17)<-NULL
# Soos<-subset(Riparianjoin17, str_detect(Riparianjoin17$SiteName, "09SOO")|str_detect(Riparianjoin17$SiteName, "09JEN")|str_detect(Riparianjoin17$SiteName, "09COV"), select=c(SiteName, SampleDate, Notes, RecentLandscapeChange, LandscapeChangeDesc))

# write.csv(Soos, "Sooslandscape17.csv")
# write.csv(test, "Sooslandscape18-20.csv")

pools2017<-subset(sample2017, select=c(SiteName,SampleName, SampleDate,NumberOfPools, ReachLength))
st_geometry(pools2017)<-NULL

lwd17<-subset(sample2017,select=c(SiteName, SampleDate,LargeWoodyDebris,SampleName))
st_geometry(lwd17)<-NULL

StreamBankStability17<-subset(sample2017,select=c(SiteName, SampleDate,StreamBankStability,SampleName))
st_geometry(StreamBankStability17)<-NULL

names(lwd17)<-names(lwd)

# st_layers (dsn='G:/GreenWQA/Biota/GIS/ArcCollector_habitat_data_2017/BIBIHabitat_June112018.gdb')

riffle2017<-st_read(dsn='G:/GreenWQA/Biota/GIS/ArcCollector_habitat_data_2017/BIBIHabitat_June112018.gdb', layer="Riffle")
rifflejoin17<-inner_join(riffle2017, sample2017, by=c("SampleGUID"="GlobalID"))
# test<-left_join(riffle2017, sample2017, by=c("SampleGUID"="GlobalID"))
WW_2017<-aggregate(WettedWidth~SiteName+SampleName+SampleDate, rifflejoin17, FUN=mean)
sample2017[which(!sample2017$SampleName %in% WW_2017$SampleName),]
BF_2017<-aggregate(BankfulWidth~SiteName+SampleName+SampleDate, rifflejoin17, FUN=mean)
sample2017[which(!sample2017$SampleName %in% BF_2017$SampleName),]

pebble2017<-st_read(dsn='G:/GreenWQA/Biota/GIS/ArcCollector_habitat_data_2017/BIBIHabitat_June112018.gdb', layer="PebbleCount")
pebble2017<-inner_join(pebble2017, sample2017, by=c("SampleGUID"="GlobalID"))
pebble2017<-dcast(pebble2017, SiteName+SampleName~SubstrateClass, value.var="PebbleCount")

pebble2017$SampleID<-c(1:nrow(pebble2017))
pebble2017<-pebble2017[,c(1,2, ncol(pebble2017),3:(ncol(pebble2017)-1))]
# names(pebble2017)[1:3]<-c("SITE_ID", "SAMPLE_NUMBER", "SampleID")
pebble2017<-pebble2017[,c(1,2,3,13,18,19,6,7,12,11,4,5,16,17,15,10,8,21,14,9,20)]
names(pebble2017)<-names(pebble)
sample2017[which(!sample2017$SampleName %in% pebble2017$SampleName),]


canopy2017<-st_read(dsn='G:/GreenWQA/Biota/GIS/ArcCollector_habitat_data_2017/BIBIHabitat_June112018.gdb', layer="RiffleCanopyMeasurement")
canopyjoin2017<-inner_join(canopy2017, riffle2017, by=c("RiffleGUID"="GlobalID"))
canopyjoin2_2017<-inner_join(canopyjoin2017, sample2017, by=c("SampleGUID"="GlobalID"))
canopy17<-aggregate(North+South+East+West~SiteName+SampleName+SampleDate, canopyjoin2_2017, FUN=mean)
names(canopy17)<-names(canopy)
sample2017[which(!sample2017$SampleName %in% canopy17$SampleName),]### some site sheets have place for values, some don't. Some sites have canopy values entered in DB but not on datasheet
rm(canopyjoin2017, canopyjoin2_2017)

flowtime2017<-st_read(dsn='G:/GreenWQA/Biota/GIS/ArcCollector_habitat_data_2017/BIBIHabitat_June112018.gdb', layer="RiffleFlow")
flowtime2017_join<-inner_join(flowtime2017, rifflejoin17, by=c("RiffleGUID"= "GlobalID"))
Flowspeed_2017<-aggregate(cbind(RiffleFlowTime,RiffleFlowDistance)~SiteName+SampleName+SampleDate, flowtime2017_join, FUN=mean)
Flowspeed_2017$speed<-Flowspeed_2017$RiffleFlowDistance/Flowspeed_2017$RiffleFlowTime
names(Flowspeed_2017)<-names(Flowspeed)
sample2017[which(!sample2017$SampleName %in% Flowspeed_2017$SampleName),]
rm(flowtime2017, flowtime2017_join)

riffledepth2017<-st_read(dsn='G:/GreenWQA/Biota/GIS/ArcCollector_habitat_data_2017/BIBIHabitat_June112018.gdb', layer="RiffleDepth")
# riffle_join<-inner_join(riffle2017, sample2017, by=c("SampleGUID"="GlobalID"))
riffledepth_join<-inner_join(riffledepth2017, rifflejoin17, by=c("RiffleGUID"= "GlobalID"))
RDepth_2017<-aggregate(RiffleDepth~SiteName+SampleName+SampleDate, riffledepth_join, FUN=mean)
sample2017[which(!sample2017$SampleName %in% RDepth_2017$SampleName),]

# setwd("G:/GreenWQA/Biota/BenthosProjects_KC/trends analysis/2021 trends analysis")
pebble<-rbind(pebble2017,pebble)
write.csv( pebble, "pebble_17-21.csv")

BF<-rbind(BF_2017, BF)
WW<-rbind(WW_2017,WW)
WW_BF<-left_join(WW, BF)
WW_BF$expr1<-WW_BF$WettedWidth/WW_BF$BankfulWidth
write.csv(WW_BF, "WW_BF_17-21.csv")

lwd<-rbind(lwd17, lwd)
# lwd<-lwd[,-c(2,4)]
# names(lwd)<-c("SAMPLE_NUMBER", "NUM_LWD")
write.csv(lwd, "LWD_17-21.csv")

pools<-rbind(pools2017, pools)
pools$pools_per_ft<-pools$NumberOfPools/pools$ReachLength
# pools<-pools[,-c(2,3,4,5)]
# names(pools)<-c("SAMPLE_NUMBER", "pools_per_ft")
pools<-na.omit(pools)
write.csv(pools, "pools_per_ft_17-21.csv")

Rdepth<-rbind(RDepth_2017, RDepth)
# names(Rdepth)<-c("SAMPLE_NUMBER", "AvgOfDepth")
write.csv(Rdepth, "Avg Riffle Depth_17-21.csv")

canopy<-rbind(canopy17, canopy)
write.csv(canopy, "CanopyCover_17-21.csv")

Flowspeed<-rbind(Flowspeed_2017, Flowspeed)
write.csv(Flowspeed, "FlowSpeed_17-21.csv")

stability2<-dcast(stability, SiteName+SampleName+SampleDate~BankType)
write.csv(stability2, "BankStability_17-21.csv")
write.csv(BFD, "BFDepth_17-21.csv")

write.csv(TD, "ThalwegDepth_18-22.csv")

rm(riffledepth2017, riffle_join, riffledepth_join, depth_join, RDepth_2017,RDepth_2018, depth, BF_2017, BF_2018, canopy17, canopy18, Flowspeed_2017, Flowspeed_2018, lwd17, lwd18, pebble18, pebble2017, pools2017, pools2018, riffle2017, rifflejoin17, sample2017, Sample2018, WW_2017, WW_2018)

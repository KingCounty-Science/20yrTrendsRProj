setwd(here::here())
setwd("./outputs")

library("reshape2")
library("stringr")
library("TITAN2")
library(plyr)
library("reshape2")
library("data.table")
library(dplyr)
library("sf")
library(openxlsx)
library(ggplot2)
RD<-read.csv("Avg Riffle Depth_17-21.csv")
CC<-read.csv("CanopyCover_17-21.csv")
FS<-read.csv("FlowSpeed_17-21.csv")
LWD<-read.csv("LWD_17-21.csv")
pools<-read.csv("pools_per_ft_17-21.csv")
Peb<-read.csv("pebble_17-21.csv")##done in wetted width
WWBF<-read.csv("WW_BF_17-21.csv")
imp<-read.csv("NLCD_basin_impervious.csv")
imp_RB<-read.csv("NLCD_buffer_impervious.csv")
ccap<-read.csv("NLCD_basin.csv")
ccapRB<-read.csv("NLCD_RB.csv")

setwd(here::here())
setwd("./inputs")
peb1316<-read.csv("Wolman_13-16.csv")##done in wetted width

hist_RD<-read.csv("02-16_Avg Riffle Depth.csv", header=T)
hist_LWD<-read.csv("02-16_LWD.csv")
hist_pools<-read.csv("02-16_pools_per_ft.csv")
hist_WWBF<-read.csv("02-16_WW_BF.csv")
hist_riffle<-read.csv("02-16_Riffle.csv")
# peb512<-read.csv("Wolman_05-12.csv")##done in BF width
# peb23<-read.csv("Wolman_02-03.csv")##done in BF width
peb0202<-read.xlsx("Wolman_2002-2003.xlsx", sheet = 2)##done in BF width
peb0512<-read.xlsx("tblWolman_2005-2012.xlsx", sheet = 2)##done in BF width

setwd(here::here())
setwd("./scripts")

if(!exists("Wolman", mode="function")) source ("PSSB_utilities.r")
setwd(here::here())
setwd("./inputs")

D<-.5

names(Peb)
# Peb[duplicated(Peb$SampleName),]$SampleName<-paste0(Peb[duplicated(Peb$SampleName),]$SampleName, "_R")
SC<- c(2, 2.8, 4, 5.6, 8, 11,16,22.6,32,45,64,90,128,180,256,512,1024,2048)
Peb[is.na(Peb)]<-0
sand<-Peb$ASubstrateClass/rowSums(Peb[5:ncol(Peb)], na.rm=T)*100
# sand<-(sand+5.5)/.98
sand1720<-cbind(Peb[,2:3],sand)
w<-Wolman(data=Peb, sizeclass=SC, D=D, Samplenames="SampleName", start=5, end=ncol(Peb))

names(peb1316)
SC<- c(2, 2.8, 4, 5.6, 8, 11,16,22.6,32,45,64,90,128,180,256,512,1024,2048)
peb1316[is.na(peb1316)]<-0
sand<-peb1316$Sand..A./rowSums(peb1316[4:ncol(peb1316)], na.rm=T)*100
# sand<-(sand+5.5)/.98
sand1316<-cbind(peb1316[,1:2],sand)
x<-Wolman(peb1316, SC, D, Samplenames="SAMPLE_NUMBER", start=4, end=ncol(peb1316))

# names(peb512)
names(peb0512)
peb512<-peb0512

SC<- c(2, 4, 8, 16,32,64,128,256,512,1024,2048,4096,10000)
peb512[is.na(peb512)]<-0
# peb512$MR_PC_0.2_mm<-(peb512$MR_PC_0.2_mm*.98)-(peb512$MR_PC_0.2_mm*.055)
sand<-peb512$`MR_PC_0-2_mm`/rowSums(peb512[4:ncol(peb512)], na.rm=T)*100
sand512<-cbind(peb512[,1:2],sand)
y<-Wolman(peb512, SC, D, Samplenames="SAMPLE_NUMBER", start=4, end=ncol(peb512))

names(peb23)
names(peb0202)
peb23<-peb0202
peb23$`PC_65-128_mm`<-as.numeric(paste0(peb23$`PC_65-128_mm`))
peb23<-peb23[,c(1:6, 10, 7:9)]
SC<- c(2,8,64,128,256,512,1024)
peb23[is.na(peb23)]<-0
# peb23$PC_0.2_mm<-(peb23$PC_0.2_mm*.98)-(peb23$PC_0.2_mm*.055)
sand<-peb23$`PC_0-2_mm`/rowSums(peb23[4:ncol(peb23)], na.rm=T)*100
sand23<-cbind(peb23[,1:2],sand)
z<-Wolman(peb23, SC, D, Samplenames="SAMPLE_NUMBER", start=4, end=ncol(peb23))

sand<-rbindlist(list(sand1720, sand1316, sand512, sand23), use.names = F)
# sand<-rbindlist(list(sand1720, sand1316), use.names = F)
sand$Year<-paste0(20,str_sub(str_remove(sand$SampleName, "_R"), nchar(str_remove(sand$SampleName, "_R"))-1, nchar(str_remove(sand$SampleName, "_R"))))
p1<-ggplot(sand, aes(x=Year, y=sand, group=Year))+geom_boxplot()
# ggsave("percent_0-2mm over time_13_20.png", p1, width=16, height=10, dpi=300)

Wolm<-rbind(x,y,z)
Wolm$Year<-paste0(20,str_sub(str_remove(Wolm$SAMPLE_NUMBER, "_R"), nchar(str_remove(Wolm$SAMPLE_NUMBER, "_R"))-1, nchar(str_remove(Wolm$SAMPLE_NUMBER, "_R"))))

names(w)
w<-subset(w, select=-c(X, OBJECTID.x))
w$Year<-paste0(20,str_sub(str_remove(w$SampleName, "_R"), nchar(str_remove(w$SampleName, "_R"))-1, nchar(str_remove(w$SampleName, "_R"))))

names(Wolm)
Wolm<-subset(Wolm, select=-c(SampleID))

names(Wolm)<-names(w)

Wolm<-rbind(Wolm, w)
ggplot(Wolm, aes(x=Year, y=D50, group=Year))+geom_boxplot()
rm(Peb, peb1316, peb23, peb512, w, x, y, z, SC, D)
sum(is.na(Wolm$D50))
################
names(hist_RD)
hist_RD$SiteName<-str_sub(str_remove(hist_RD$SAMPLE_NUMBER, "_R"), 1, nchar(str_remove(hist_RD$SAMPLE_NUMBER, "_R"))-3)
hist_RD<-hist_RD[,c(1,3,2)]

names(RD)
# RD[duplicated(RD$SampleName),]$SampleName<-paste0(RD[duplicated(RD$SampleName),]$SampleName, "_R")
RD<-subset(RD, select=c(SampleName, SiteName, RiffleDepth))

names(hist_RD)<-names(RD)
RD<-rbind(hist_RD, RD)
RD$Year<-paste0(20,str_sub(str_remove(RD$SampleName, "_R"), nchar(str_remove(RD$SampleName, "_R"))-1, nchar(str_remove(RD$SampleName, "_R"))))

rm(hist_RD)
###########################
names(hist_LWD)
hist_LWD$SiteName<-str_sub(str_remove(hist_LWD$SAMPLE_NUMBER, "_R"), 1, nchar(str_remove(hist_LWD$SAMPLE_NUMBER, "_R"))-3)
hist_LWD<-hist_LWD[,c(1,3,2)]

names(LWD)
# LWD[duplicated(LWD$SampleName),]$SampleName<-paste0(LWD[duplicated(LWD$SampleName),]$SampleName, "_R")
LWD<-subset(LWD, select=c(SampleName, SiteName, LargeWoodyDebrisNumber))

names(hist_LWD)<-names(LWD)
LWD<-rbind(hist_LWD, LWD)
LWD$Year<-paste0(20,str_sub(str_remove(LWD$SampleName, "_R"), nchar(str_remove(LWD$SampleName, "_R"))-1, nchar(str_remove(LWD$SampleName, "_R"))))

rm(hist_LWD)
#########################
names(hist_pools)
hist_pools$SiteName<-str_sub(str_remove(hist_pools$SAMPLE_NUMBER, "_R"), 1, nchar(str_remove(hist_pools$SAMPLE_NUMBER, "_R"))-3)
hist_pools<-hist_pools[,c(1,3,2)]

names(pools)
# pools[duplicated(pools$SampleName),]$SampleName<-paste0(pools[duplicated(pools$SampleName),]$SampleName, "_R")
pools<-subset(pools, select=c(SampleName, SiteName, pools_per_ft))

names(hist_pools)<-names(pools)
pools<-rbind(hist_pools, pools)
pools$Year<-paste0(20,str_sub(str_remove(pools$SampleName, "_R"), nchar(str_remove(pools$SampleName, "_R"))-1, nchar(str_remove(pools$SampleName, "_R"))))

rm(hist_pools)

##########################
names(hist_WWBF)
hist_WWBF$SiteName<-str_sub(str_remove(hist_WWBF$SAMPLE_NUMBER, "_R"), 1, nchar(str_remove(hist_WWBF$SAMPLE_NUMBER, "_R"))-3)
hist_WWBF<-hist_WWBF[,c(1,5,2:4)]

names(WWBF)
# WWBF[duplicated(WWBF$SampleName),]$SampleName<-paste0(WWBF[duplicated(WWBF$SampleName),]$SampleName, "_R")
WWBF<-subset(WWBF, select=c(SampleName, SiteName, WettedWidth, BankfulWidth, expr1))

names(hist_WWBF)<-names(WWBF)
WWBF<-rbind(hist_WWBF, WWBF)
WWBF$Year<-paste0(20,str_sub(str_remove(WWBF$SampleName, "_R"), nchar(str_remove(WWBF$SampleName, "_R"))-1, nchar(str_remove(WWBF$SampleName, "_R"))))

rm(hist_WWBF)
###############

hist_flow<-subset(hist_riffle, select=c(tblSampleEvent_SAMPLE_NUMBER, SITE_ID, SampleDate, Riffle_Flow_Distance_ft, Riffle_Flow_Time_sec_Rep1,  Riffle_Flow_Time_sec_Rep2,  Riffle_Flow_Time_sec_Rep3))
hist_flow$AvgVel<-((hist_flow$Riffle_Flow_Distance_ft/hist_flow$Riffle_Flow_Time_sec_Rep1)+(hist_flow$Riffle_Flow_Distance_ft/hist_flow$Riffle_Flow_Time_sec_Rep2)+(hist_flow$Riffle_Flow_Distance_ft/hist_flow$Riffle_Flow_Time_sec_Rep3))/3
hist_flow<-aggregate(AvgVel~tblSampleEvent_SAMPLE_NUMBER+SITE_ID+SampleDate, hist_flow, FUN=mean)
names(hist_flow)
names(FS)
# FS[duplicated(FS$SampleName),]$SampleName<-paste0(FS[duplicated(FS$SampleName),]$SampleName, "_R")
FS<-subset(FS, select=c(SampleName, SiteName, SampleDate, speed))
names(hist_flow)<-names(FS)

FS<-rbind(hist_flow, FS)
FS$Year<-paste0(20,str_sub(str_remove(FS$SampleName, "_R"), nchar(str_remove(FS$SampleName, "_R"))-1, nchar(str_remove(FS$SampleName, "_R"))))

rm(hist_flow, hist_riffle)

#########################
WWBF[WWBF$SiteName=="08EVA3474",]$SiteName<-"08BEA3474"
pools[pools$SiteName=="08EVA3474",]$SiteName<-"08BEA3474"
RD[RD$SiteName=="08EVA3474",]$SiteName<-"08BEA3474"
LWD[LWD$SiteName=="08EVA3474",]$SiteName<-"08BEA3474"
FS[FS$SiteName=="08EVA3474",]$SiteName<-"08BEA3474"
CC[CC$SiteName=="08EVA3474",]$SiteName<-"08BEA3474"

# Wolm[Wolm$Site=="09mil0390","SiteName"]<-"09MIL0390"
Wolm[Wolm$Site=="08EVA3474","SiteName"]<-"08BEA3474"
Wolm[Wolm$Site=="09COV1862/10","SiteName"]<-"09COV1862"
Wolm[Wolm$Site=="09COV1864/10","SiteName"]<-"09COV1864"
Wolm[Wolm$Site=="09JEN1357/10","SiteName"]<-"09JEN1357"
Wolm[Wolm$Site=="MILLER_SWSSD","SiteName"]<-"Miller_SWSSD"
Wolm[Wolm$Site=="09/LOW0406","SiteName"]<-"09LOW0406"
Wolm[Wolm$Site=="09Soo1106","SiteName"]<-"09SOO1106"
Wolm$SampleName<-str_replace(Wolm$SampleName, "Bse", "BSE")
Wolm$SampleName<-str_replace(Wolm$SampleName, "MILLER", "Miller")
Wolm$SampleName<-str_replace(Wolm$SampleName, "Golfcrs", "GolfCrs")
# Wolm$SampleName<-str_replace(Wolm$SampleName, "09mil0390", "09MIL0390")

FS[FS$Site=="Bse_1MudMtnRd","SiteName"]<-"BSE_1_MudMtnRd"
FS[FS$Site=="Bse_21_golfcrs","SiteName"]<-"BSE_21_GolfCrs"
FS[FS$Site=="Bse_8_268AveSE","SiteName"]<-"BSE_8_268thAveSE"
FS[FS$Site=="08EVA3474","SiteName"]<-"08BEA3474"
FS[FS$Site=="09COV1862/10","SiteName"]<-"09COV1862"
FS[FS$Site=="09COV1864/10","SiteName"]<-"09COV1864"
FS[FS$Site=="09JEN1357/10","SiteName"]<-"09JEN1357"
FS[FS$Site=="MILLER_SWSSD","SiteName"]<-"Miller_SWSSD"
FS[FS$Site=="09/LOW0406","SiteName"]<-"09LOW0406"
FS[FS$Site=="09Soo1106","SiteName"]<-"09SOO1106"
FS$SampleName<-str_replace(FS$SampleName, "Bse", "BSE")
FS$SampleName<-str_replace(FS$SampleName, "MILLER", "Miller")
FS$SampleName<-str_replace(FS$SampleName, "golfcrs", "GolfCrs")

sand<-as.data.frame(sand)
sand[sand$Site=="Bse_1MudMtnRd","SiteName"]<-"BSE_1_MudMtnRd"
sand[sand$Site=="Bse_21_golfcrs","SiteName"]<-"BSE_21_GolfCrs"
sand[sand$Site=="Bse_8_268AveSE","SiteName"]<-"BSE_8_268thAveSE"
# sand[sand$Site=="09mil0390","SiteName"]<-"09MIL0390"
sand[sand$Site=="08EVA3474","SiteName"]<-"08BEA3474"
sand[sand$Site=="09COV1862/10","SiteName"]<-"09COV1862"
sand[sand$Site=="09COV1864/10","SiteName"]<-"09COV1864"
sand[sand$Site=="09JEN1357/10","SiteName"]<-"09JEN1357"
sand[sand$Site=="MILLER_SWSSD","SiteName"]<-"Miller_SWSSD"
sand[sand$Site=="09/LOW0406","SiteName"]<-"09LOW0406"
sand[sand$Site=="09Soo1106","SiteName"]<-"09SOO1106"
sand$SampleName<-str_replace(sand$SampleName, "Bse", "BSE")
sand$SampleName<-str_replace(sand$SampleName, "MILLER", "Miller")
sand$SampleName<-str_replace(sand$SampleName, "Golfcrs", "GolfCrs")
# sand$SampleName<-str_replace(sand$SampleName, "09mil0390", "09MIL0390")


# merged<-Reduce(merge, list(WWBF, Wolm, RD, pools, LWD, FS, sand, CC))
merged<-Reduce(function(x, y) merge(x, y, all=TRUE), list(WWBF, Wolm, RD, pools, LWD, FS, sand, CC))
merged<-subset(merged, !is.na(SiteName))
merged[merged$SiteName=="09mil0390",]$SiteName<-"09MIL0390"
merged[merged$SiteName=="BSE_8_268hAveSE",]$SiteName<-"BSE_8_268thAveSE"
merged[merged$SiteName=="VASHCHRIS","SiteName"]<-"VashChris"
merged[merged$SiteName=="VASHSHING","SiteName"]<-"VashShing"
merged[merged$SiteName=="09COV1862/10","SiteName"]<-"09COV1862"
merged[merged$SiteName=="09COV1864/10","SiteName"]<-"09COV1864"
merged[merged$SiteName=="09JEN1357/10","SiteName"]<-"09JEN1357"
merged[merged$SiteName=="MILLER_SWSSD","SiteName"]<-"Miller_SWSSD"
sum(is.na(merged$D50))
merged[is.na(merged$D50), c("SiteName", "Year")]###these all don't have pebble counts


merged1<-merge(merged, imp, by.x=c("SiteName", "Year"), by.y=c("Name", "Year"), all=T)
merged1<-merge(merged1, imp_RB, by.x=c("SiteName", "Year"), by.y=c("Name", "Year"), all=T)

unique(merged[which(!merged$SampleName %in% merged1$SampleName),]$SiteName)

merged1<-merge(merged1, ccap, by.x=c("SiteName", "Year"), by.y=c("Name", "Year"), all.x=T)
merged1<-merge(merged1, ccapRB, by.x=c("SiteName", "Year"), by.y=c("NameRB", "YearRB"), all.x=T)
# unique(merged1[which(!merged1$SampleName %in% test$SampleName),]$SiteName)

# basins<-read.csv("G:/GreenWQA/Biota/BenthosProjects_KC/trends analysis/Data_for_Trends_FWS_manuscript/LandUse.csv", header=T)
# basins<-subset(basins, Trends.==T)$Name

# merged2<-subset(merged1, SiteName %in% basins)
merged2<-merged1

merged3<-merged2[which(!str_detect(merged2$SampleName, "_R")),]
rm(merged, merged1, merged2, WWBF, pools, RD, LWD, FS,  imp, Wolm)

####based on DB cleanup notes, exclude 2002 LWD counts
setwd(here::here())
setwd("./inputs")
strlevel<-read.csv("KCSites_w_StrLevel.csv", header=T)
names(strlevel)
strlevel<-subset(strlevel, select=c(Name, STR_LVL))
# LU<-read.csv("G:/GreenWQA/Biota/BenthosProjects_KC/trends analysis/Data_for_Trends_FWS_manuscript/LandUse.csv", header=T)
setwd(here::here())
setwd("./outputs")
LU<-read.csv("NLCD_LULC_2001-2019.csv", header = T)
names(LU)
acres<-subset(LU, select=c(Name, X2019))
names(acres)[2]<-"Acres"
size<-merge(acres, strlevel)

merged4<-merge(merged3, size, by.x="SiteName", by.y="Name", all.x=T)

rm(size, acres, strlevel, merged3)

merged4[merged4$Year=="2002"&!is.na(merged4$LargeWoodyDebrisNumber),"LargeWoodyDebrisNumber"]<-NA

merged4[duplicated(merged4[, c("Year", "SiteName")]),]
merged4[duplicated(merged4[, c("Year", "SampleName")]),]##none of these are trend sites, so I don't care at the moment...
write.csv(merged4, "ENV_data_2022_03062023.csv")


########## Add WQI ######
df_clean<-read.csv("Stream_chem.csv", header=T)
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

df_clean$wtr_yr<-wtr_yr(df_clean$CollectDate, 10)
metrics<-names(df_clean)[8:20]
mean_vals<-df_clean %>% group_by(V1, wtr_yr) %>% summarize_at(metrics, mean)
# data<-merge(BIBI_scores, mean_vals, by.x=c("Site.Code", "Year"), by.y=c("V1", "wtr_yr"))
merged5<-merge(merged4, mean_vals, by.x=c("SiteName", "Year"), by.y=c("V1", "wtr_yr"))

write.csv(merged5, "ENV_WQI_data_022323.csv")

# ####################### add degree days from Jan 1st to collect date ###
# degreedays<-read.csv("G:/GreenWQA/Biota/BenthosProjects_KC/trends analysis/Data_for_Trends_FWS_manuscript/CAP/DegreeDays0C.csv", header=T)
# merged4$SampleName
# 
# merged4$SampleDate<-as.Date(ifelse(grepl("-", merged4$SampleDate),as.Date(merged4$SampleDate, format = c("%Y-%m-%d")), as.Date(merged4$SampleDate, format = c('%m/%d/%Y'))), origin = "1970-01-01")
# degreedays$Visit.Date<-as.Date(ifelse(grepl("-", degreedays$Visit.Date),as.Date(degreedays$Visit.Date, format = c("%Y-%m-%d")), as.Date(degreedays$Visit.Date, format = c('%m/%d/%Y'))), origin = "1970-01-01")
# degreedays$Sample.Code<-str_replace(degreedays$Sample.Code, "/", "_")
# degreedays$Sample.Code<-str_replace(degreedays$Sample.Code, "-", "_")
# degreedays<-degreedays[!str_detect(degreedays$Sample.Code, "_R"),]
# degreedays<-degreedays[!str_detect(degreedays$Sample.Code, "[0-9]R[0-9]"),]
# degreedays<-degreedays[!str_detect(degreedays$Sample.Code, "[0-9]R$"),]
# degreedays<-degreedays[!str_detect(degreedays$Sample.Code, "QC"),]
# degreedays<-degreedays[!str_detect(degreedays$Sample.Code, "_r"),]
# degreedays<-degreedays[!str_detect(degreedays$Sample.Code, "Duplicate"),]
# # degreedays$Sample.Code<-ifelse(str_detect(degreedays$Sample.Code, "_[0-9][0-9][0-9][0-9]"), str_replace(degreedays$Sample.Code, str_sub(degreedays$Sample.Code, str_locate(degreedays$Sample.Code, "_[0-9][0-9][0-9][0-9]")[,1], str_locate(degreedays$Sample.Code, "_[0-9][0-9][0-9][0-9]")[,2]-2), "_"),degreedays$Sample.Code) 
# 
# merged5<-merge(merged4, degreedays, by.x=c("SiteName", "Year"), by.y=c("Site.Code", "Year"), all.x=T, all.y=F) ##merge on year because visit dates were entered incorrectly in habitat collector sometimes
# subset(merged5, select=c(SiteName, Year))[duplicated(subset(merged5, select=c(SiteName, Year))),]
# merged5<-subset(merged5, select=-c(x32, Visit.Date, Sample.Code, X.y, X.x, X.y, X.x))
# merged5<-subset(merged5, select=-c(X.y, X.x))
# write.csv(merged5, "ENV_data.csv")
# 
# ########## Add WQI ######
# df_clean<-read.csv("G:/GreenWQA/Biota/BenthosProjects_KC/trends analysis/Data_for_Trends_FWS_manuscript/CAP/Stream_chem.csv", header=T)
# wtr_yr <- function(dates, start_month=10) {
#   # Convert dates into POSIXlt
#   dates.posix = as.POSIXlt(dates)
#   # Year offset
#   offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
#   # Water year
#   adj.year = dates.posix$year + 1900 + offset
#   # Return the water year
#   adj.year
# }
# 
# df_clean$wtr_yr<-wtr_yr(df_clean$CollectDate, 10)
# 
# metrics<-names(df_clean)[8:20]
# 
# mean_vals<-df_clean %>% group_by(V1, wtr_yr) %>% summarize_at(metrics, mean)
# # data<-merge(BIBI_scores, mean_vals, by.x=c("Site.Code", "Year"), by.y=c("V1", "wtr_yr"))
# merged6<-merge(merged5, mean_vals, by.x=c("SiteName", "Year"), by.y=c("V1", "wtr_yr"))
# 
# write.csv(merged6, "ENV_WQI_data.csv")
# 
# 
# KC_taxa<-read.csv("G:/GreenWQA/Biota/BenthosProjects_KC/trends analysis/Data_for_Trends_FWS_manuscript/KC_noAmbig_noReps_Rolledup_density_02092021.csv")
# KC_taxa$year <- format(as.Date(KC_taxa$CollectionDate, '%m/%d/%Y'), "%Y") 
# combined<-merge(merged5, KC_taxa, by.x=c("SiteName", "Year"), by.y=c("STAID", "year"))
# 
# ggplot(combined, aes(x=sand, y=Chironomidae, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# ggplot(combined, aes(x=sand, y=Zapada.cinctipes, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# ggplot(combined, aes(x=sand, y=Simuliidae, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# ggplot(combined, aes(x=sand, y=Rhyacophila, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# ggplot(combined, aes(x=sand, y=Oligochaeta, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# ggplot(combined, aes(x=sand, y=Glossosoma, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# ggplot(combined, aes(x=sand, y=Dicranota, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# ggplot(combined, aes(x=sand, y=Baetis.rhodani.group, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# ggplot(combined, aes(x=sand, y=Sweltsa, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# 
# 
# density_re<-reshape2::melt(KC_taxa, id.vars=c(1:4, (ncol(KC_taxa)-3):ncol(KC_taxa)), value.name="Density")
# density_org<-ddply(density_re, .(STAID, CollectionDate, Visit.ID, Stream, Basin, year), summarize, sum_orgs=sum(Density))
# ggplot(density_org, aes(x=year, y=sum_orgs, color=STAID, group=STAID))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~STAID)
# ggplot(merged5, aes(x=Year, y=sand,color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)
# ggplot(subset(merged5, Year>2012), aes(x=Year, y=sand,color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)
# 

# ggplot(combined, aes(x=Year, y=Chironomidae, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# ggplot(combined, aes(x=Year, y=Zapada.cinctipes, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# ggplot(combined, aes(x=Year, y=Simuliidae, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# ggplot(combined, aes(x=Year, y=Rhyacophila, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# ggplot(combined, aes(x=Year, y=Oligochaeta, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# ggplot(combined, aes(x=Year, y=Glossosoma, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# 
# combined_sumorgs<-merge(merged5, density_org, by.x=c("SiteName", "Year"), by.y=c("STAID", "year"))
# ggplot(combined_sumorgs, aes(x=sand, y=sum_orgs, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+ylim(c(0,2000))+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# ggplot(combined_sumorgs, aes(x=sand, y=sum_orgs))+geom_point()+geom_smooth(method="lm", se=F)+ylim(c(0,2000))+theme(legend.position = "none")#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# 
# 
# decliners<-subset(density_re, variable=="Chironomidae"| variable=="Baetis.rhodani.group"| variable=="Oligochaeta"| variable=="Simuliidae"| variable=="Zapada.cinctipes"| variable=="Sweltsa"|variable=="Dicranota")
# decliners<-ddply(decliners, .(STAID, CollectionDate, Visit.ID, Stream, Basin, year), summarize, sum_orgs=sum(Density))
# decliners_sumorgs<-merge(merged5, decliners, by.x=c("SiteName", "Year"), by.y=c("STAID", "year"))
# ggplot(decliners_sumorgs, aes(x=sand, y=sum_orgs, color=SiteName, group=SiteName))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(.~SiteName)+ theme(legend.position = "none") +ylim(c(0,2000))#+geom_text(aes(label=SiteName),hjust=0, vjust=0)
# ggplot(decliners_sumorgs, aes(x=sand, y=sum_orgs))+geom_point()+geom_smooth(method="lm", se=F)+ theme(legend.position = "none") +ylim(c(0,2000))#+geom_text(aes(label=SiteName),hjust=0, vjust=0)

# trends<-read.csv("G:/GreenWQA/Biota/BenthosProjects_KC/trends analysis/Data_for_Trends_FWS_manuscript/CSV/B-IBI_sites_w_trends.csv")
# scores<-read.csv("G:/GreenWQA/Biota/BenthosProjects_KC/trends analysis/Data_for_Trends_FWS_manuscript/CSV/B-IBI_results.csv")
# sand$Year<-as.numeric(paste0(sand$Year))
# Wolm$Year<-as.numeric(paste0(Wolm$Year))
# wlmoscores<-merge(Wolm, scores, by.x=c("SiteName", "Year"), by.y=c("Site.Code", "Year"))
# ggplot(wlmoscores, aes(x=D50, y=Overall.Score, color=SiteName))+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# sandscores<-merge(sand, scores, by.x=c("SiteName", "Year"), by.y=c("Site.Code", "Year"))
# ggplot(sandscores, aes(x=sand, y=Overall.Score, color=SiteName))+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# wlmoscores<-merge(wlmoscores, trends, by.x=c("SiteName"), by.y=c("Site_Code"))
# 
# ggplot(wlmoscores[wlmoscores$MKtau_Overall_Score>0,], aes(x=Year, y=D50, color=SiteName))+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(wlmoscores[wlmoscores$MKtau_Overall_Score<0,], aes(x=Year, y=D50, color=SiteName))+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(wlmoscores[wlmoscores$MKtrend_Overall_Score=="positive",], aes(x=Year, y=D50, color=SiteName))+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# 
# sandscores<-merge(sandscores, trends, by.x=c("SiteName"), by.y=c("Site_Code"))
# 
# ggplot(sandscores[sandscores$MKtau_Overall_Score>0,], aes(x=Year, y=sand, color=SiteName))+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(sandscores[sandscores$MKtau_Overall_Score<0,], aes(x=Year, y=sand, color=SiteName))+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(sandscores[sandscores$MKtrend_Overall_Score=="positive",], aes(x=Year, y=sand, color=SiteName))+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)

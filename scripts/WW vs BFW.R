
setwd(here::here())

library(data.table)
library(dbplyr)
library(odbc)
library(DBI)
library(dplyr)
library(stringr)
library(sf)

con <- dbConnect(odbc(), "bibihabitat")
dbListTables(con)
Sample<-dbGetQuery(con,' select * from SAMPLE')
Sample$IsReplicateSample[Sample$SiteName=="08BEA3747"&Sample$OBJECTID=="16804"]<-"Yes"##Didn't mark it as replicate in the form
Sample$year<-year(Sample$SampleDate)
Sample<-subset(Sample, !is.na(year))
Sample<-subset(Sample, IsReplicateSample=="No")
Sample<-subset(Sample, IsSampleTaken=="Yes")
Sample[Sample$OBJECTID=="12001","SiteName"]<-"07LNT163403"
Sample[which(duplicated(subset(Sample, select=c(SiteName, year)))),]
Sample[which(duplicated(subset(Sample, select=c(SiteName, year)), fromLast=T)),]
Sample$SampleName<-paste0(Sample$SiteName, "_", str_sub(Sample$year, 3, 4))
# Sample[Sample$OBJECTID=="74022", "SampleDate"]<-"2022-08-03 18:15:00.0000000"
Sample<-subset(Sample, !OBJECTID=="74022")
Sample$year<-year(Sample$SampleDate)
# Sample<-subset(Sample, !OBJECTID=="54806")
dupobj1<-Sample[which(duplicated(subset(Sample, select=c(SiteName, year)))),]$OBJECTID
dupobj2<-Sample[which(duplicated(subset(Sample, select=c(SiteName, year)), fromLast=T)),]$OBJECTID

Sample<-subset(Sample, year==2022)

pebble<-dbGetQuery(con,'select * from PEBBLECOUNT')
pebble<-inner_join(pebble, Sample, by=c("SampleGUID"="GlobalID"))
pebble[pebble$OBJECTID.y==74022,]
pebble<-subset(pebble, select=c("SiteName","SampleName", "OBJECTID.x",str_subset(names(pebble), "SubstrateClass")))

setwd(here::here())
setwd("./scripts")
if(!exists("Wolman", mode="function")) source ("PSSB_utilities.r")
D<-.5

names(pebble)
SC<- c(2, 2.8, 4, 5.6, 8, 11,16,22.6,32,45,64,90,128,180,256,512,1024,2048)
pebble[is.na(pebble)]<-0
sand<-pebble$ASubstrateClass/rowSums(pebble[4:ncol(pebble)], na.rm=T)*100
# sand<-(sand+5.5)/.98
sand22<-cbind(pebble[,1:2],sand)
w<-Wolman(data=pebble, sizeclass=SC, D=D, Samplenames="SampleName", start=4, end=ncol(pebble))
WW22<-merge(sand22, w)

setwd(here::here())
setwd("./inputs")
library(openxlsx)
BFW<-openxlsx::read.xlsx("BFW_pebble_counts_2022.xlsx", sheet=2)
names(BFW)
BFW<-BFW[, -22]
sandBFW<-BFW$A/rowSums(BFW[4:ncol(BFW)], na.rm=T)*100
sandBFW22<-cbind(BFW[,2],sandBFW)
colnames(sandBFW22)[1]<-"Site"
wBFW<-Wolman(data=BFW, sizeclass=SC, D=D, Samplenames="Site", start=4, end=21)
BFW22<-merge(sandBFW22, wBFW)

library(ggplot2)
library(ggpubr)
comparison<-merge(BFW22, WW22, by.x="Site", by.y = "SiteName")
ggplot(comparison, aes(x=as.numeric(paste0(sand)), y=as.numeric(paste0(sandBFW))))+geom_point()+stat_smooth(method="lm")+
  xlab("2022 WW sand")+ylab("2022 BFW sand")+stat_regline_equation()+stat_cor(aes(label=..rr.label..), label.y=60)
ggplot(comparison, aes(x=as.numeric(paste0(D50.y)), y=as.numeric(paste0(D50.x))))+geom_point()+stat_smooth(method="lm")+
  xlab("2022 WW D50")+ylab("2022 BFW D50")+stat_regline_equation()+stat_cor(aes(label=..rr.label..), label.y=60)


peb512<-openxlsx::read.xlsx("tblWolman_2005-2012.xlsx", sheet= 2)

names(peb512)


SC<- c(2, 4, 8, 16,32,64,128,256,512,1024,2048,4096,10000)
peb512[is.na(peb512)]<-0
# peb512$MR_PC_0.2_mm<-(peb512$MR_PC_0.2_mm*.98)-(peb512$MR_PC_0.2_mm*.055)
sand512<-peb512$`MR_PC_0-2_mm`/rowSums(peb512[4:ncol(peb512)], na.rm=T)*100
sand512<-cbind(peb512[,1:2],sand512)
y<-Wolman(peb512, SC, D, Samplenames="SAMPLE_NUMBER", start=4, end=ncol(peb512))

substrate512<-merge(sand512, y)
substrate512$Year<-paste0(20,str_sub(str_remove(substrate512$SAMPLE_NUMBER, "_R"), nchar(str_remove(substrate512$SAMPLE_NUMBER, "_R"))-1, nchar(str_remove(substrate512$SAMPLE_NUMBER, "_R"))))

BFW22$Year<-"2022"
names(BFW22)
names(substrate512)
BFW22$SAMPLE_NUMBER<-""
substrate512<-substrate512[,c(1,2,3,5,6)]
BFW22<-BFW22[,c(1,2,5,6,7)]
names(BFW22)<-c("Site", "Sand", "D50", "Year", "SAMPLE_NUMBER")
names(substrate512)<-c("Site", "SAMPLE_NUMBER", "Sand", "D50", "Year")
yearlyBFW<-rbind(BFW22, substrate512)
yearlyBFW$Year<-as.numeric(paste0(yearlyBFW$Year))
yearlyBFW$Sand<-as.numeric(paste0(yearlyBFW$Sand))
ggplot(yearlyBFW, aes(y=Sand, x=Year,group=Year))+geom_boxplot()
ggplot(yearlyBFW, aes(y=D50, x=Year,group=Year))+geom_boxplot()

filt<-ddply(substrate512, .(Site), summarize, length=length(unique(Year)))
filt<-subset(filt, length>6)

yearlyBFW2<-subset(yearlyBFW, Site %in% filt$Site)
yearlyBFW2<-subset(yearlyBFW2, Site %in% BFW22$Site)
# yearlyBFW2<-yearlyBFW
# ggplot(yearlyBFW2, aes(y=Sand, x=Year,group=Year))+geom_boxplot()
# ggplot(yearlyBFW2, aes(y=D50, x=Year,group=Year))+geom_boxplot()

ggplot(yearlyBFW2, aes(y=D50, x=Year,color=Site))+geom_point()+facet_wrap(.~Site)+
  theme(legend.position="none")
ggplot(yearlyBFW2, aes(y=Sand, x=Year,color=Site))+geom_point()+facet_wrap(.~Site)+
  theme(legend.position="none")


peb1316<-read.csv("Wolman_13-16.csv")##done in wetted width
names(peb1316)
SC<- c(2, 2.8, 4, 5.6, 8, 11,16,22.6,32,45,64,90,128,180,256,512,1024,2048)
peb1316[is.na(peb1316)]<-0
sand<-peb1316$Sand..A./rowSums(peb1316[4:ncol(peb1316)], na.rm=T)*100
# sand<-(sand+5.5)/.98
sand1316<-cbind(peb1316[,1:2],sand)
sand1316$sand_corrected<-sand1316$sand*.98+7.5
x<-Wolman(peb1316, SC, D, Samplenames="SAMPLE_NUMBER", start=4, end=ncol(peb1316))
x$D50_corrected<-x$D50*.75+2.9

substrate1316<-merge(sand1316, x)
yearlyBFW1316<-subset(substrate1316, SITE_ID %in% filt$Site)
yearlyBFW1316<-subset(yearlyBFW1316, SITE_ID %in% BFW22$Site)
# yearlyBFW1316<-substrate1316
names(yearlyBFW1316)
yearlyBFW1316_corrected<-yearlyBFW1316[,c(1, 2, 4, 7)]
names(yearlyBFW1316_corrected)<-c("Site", "SAMPLE_NUMBER", "Sand", "D50")
  yearlyBFW1316_corrected$Year<-paste0(20,str_sub(str_remove(yearlyBFW1316_corrected$SAMPLE_NUMBER, "_R"), nchar(str_remove(yearlyBFW1316_corrected$SAMPLE_NUMBER, "_R"))-1, nchar(str_remove(yearlyBFW1316_corrected$SAMPLE_NUMBER, "_R"))))

  yearlyBFW5_22<-rbind(yearlyBFW1316_corrected, yearlyBFW2)
  yearlyBFW5_22<-subset(yearlyBFW5_22, !str_detect(SAMPLE_NUMBER, "_R"))

  setwd(here::here())
  setwd("./outputs")
Peb1721<-read.csv("pebble_17-21.csv")  
names(Peb1721)
Peb1721[duplicated(Peb1721$SampleName),]$SampleName<-paste0(Peb1721[duplicated(Peb1721$SampleName),]$SampleName, "_R")
SC<- c(2, 2.8, 4, 5.6, 8, 11,16,22.6,32,45,64,90,128,180,256,512,1024,2048)
Peb1721[is.na(Peb1721)]<-0
sand<-Peb1721$ASubstrateClass/rowSums(Peb1721[5:ncol(Peb1721)], na.rm=T)*100
# sand<-(sand+5.5)/.98
sand1720<-cbind(Peb1721[,2:3],sand)
sand1720$sand_corrected<-sand1720$sand*.98+7.5
w<-Wolman(data=Peb1721, sizeclass=SC, D=D, Samplenames="SampleName", start=5, end=ncol(Peb1721))
w$D50_corrected<-w$D50*.75+2.9
substrate1721<-merge(sand1720, w)

yearlyBFW1721<-subset(substrate1721, SiteName %in% filt$Site)
yearlyBFW1721<-subset(yearlyBFW1721, SiteName %in% BFW22$Site)
# yearlyBFW1721<-substrate1721
names(yearlyBFW1721)
yearlyBFW1721_corrected<-yearlyBFW1721[,c(1, 2, 4, 8)]
names(yearlyBFW1721_corrected)<-c("Site", "SAMPLE_NUMBER", "Sand", "D50")
yearlyBFW1721_corrected$Year<-paste0(20,str_sub(str_remove(yearlyBFW1721_corrected$SAMPLE_NUMBER, "_R"), nchar(str_remove(yearlyBFW1721_corrected$SAMPLE_NUMBER, "_R"))-1, nchar(str_remove(yearlyBFW1721_corrected$SAMPLE_NUMBER, "_R"))))

yearlyBFW5_22<-rbind(yearlyBFW1721_corrected, yearlyBFW5_22)
yearlyBFW5_22<-subset(yearlyBFW5_22, !str_detect(SAMPLE_NUMBER, "_R"))

# ddply(yearlyBFW5_22, .(Site), mutate, por=length(unique(Year)))
  
  ggplot(yearlyBFW5_22, aes(y=D50, x=as.numeric(paste0(Year)),color=Site))+geom_point()+facet_wrap(.~Site)+
    theme(legend.position="none")
  ggplot(yearlyBFW5_22, aes(y=Sand, x=as.numeric(paste0(Year)),color=Site))+geom_point()+facet_wrap(.~Site)+
    theme(legend.position="none")
  
  ggplot(yearlyBFW5_22, aes(y=Sand, x=as.numeric(paste0(Year)),group=Year))+geom_boxplot()
  ggplot(yearlyBFW5_22, aes(y=D50, x=as.numeric(paste0(Year)),group=Year))+geom_boxplot()
  
  # trends<-read.csv("G:/GreenWQA/Biota/PSP/NTA_STORMWATER/score_RKTtrend_summarized_by_site.csv")
  

  substrate1721_corrected<-substrate1721[,c(1, 2, 4, 8)]
  names(substrate1721_corrected)<-c("Site", "SAMPLE_NUMBER", "Sand", "D50")
  substrate1316_corrected<-substrate1316[,c(1, 2, 4, 7)]
  names(substrate1316_corrected)<-c("Site", "SAMPLE_NUMBER", "Sand", "D50")
  substrate1321_corrected<-rbind(substrate1721_corrected, substrate1316_corrected)
  substrate1321_corrected$Year<-paste0(20,str_sub(str_remove(substrate1321_corrected$SAMPLE_NUMBER, "_R"), nchar(str_remove(substrate1321_corrected$SAMPLE_NUMBER, "_R"))-1, nchar(str_remove(substrate1321_corrected$SAMPLE_NUMBER, "_R"))))
  substrate_corrected<-rbind(substrate1321_corrected, substrate512)
  # substrate_corrected<-rbind(substrate_corrected, BFW22)
  substrate_corrected$Year<-as.numeric(paste0(substrate_corrected$Year))

  setwd(here::here())
  setwd("./outputs")
  # setwd("G:/GreenWQA/Biota/BenthosProjects_KC/trends analysis/2021 trends analysis")
  KC_scores<-read.csv("B-IBI_results_03082023_2002-2021_NoUnique_subsamp_450_LR_exclude.csv")
  KC_scores<-subset(KC_scores, Tot_Abund>=450)
  KC_trends<-read.csv("Site_Trends_coarse_LR_03092023_2002-2021_LRexclude.csv")
  KC_trends<-subset(KC_trends, !(Site.Code=="09DUW0277"&Stream.or.River=="Riverton Creek (003D)"))
  KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))
  KC_scores<-subset(KC_scores, Site.Code %in% KC_trends$Site.Code)
  samples<-subset(KC_scores, select=c(Sample.Code,Site.Code, Visit.Date, Overall.Score)) ### make sure to fix 09DUW0277/05, incorrectly given site as 08BEA3312 in PSSB and B-IBI results
  samples$Visit.Date<-as.Date(samples$Visit.Date, "%Y-%m-%d")
  samples$Year<-year(samples$Visit.Date)
  substrate_corrected[substrate_corrected$Site=="09mil0390","Site"]<-"09MIL0390"
  substrate_corrected[substrate_corrected$Site=="08EVA3474","Site"]<-"08BEA3474"
  substrate_corrected[substrate_corrected$Site=="VASHCHRIS","Site"]<-"VashChris"
  substrate_corrected[substrate_corrected$Site=="VASHSHING","Site"]<-"VashShing"
  substrate_corrected[substrate_corrected$Site=="09COV1862/10","Site"]<-"09COV1862"
  substrate_corrected[substrate_corrected$Site=="09COV1864/10","Site"]<-"09COV1864"
  substrate_corrected[substrate_corrected$Site=="09JEN1357/10","Site"]<-"09JEN1357"
  substrate_corrected[substrate_corrected$Site=="MILLER_SWSSD","Site"]<-"Miller_SWSSD"
  
  check<-ddply(substrate_corrected, .(Site, Year), summarize, len=length(SAMPLE_NUMBER))
  check<-subset(check, len>1)
  check<-substrate_corrected[paste0(substrate_corrected$Site, substrate_corrected$Year) %in% paste0(check$Site, check$Year)&str_detect(substrate_corrected$SAMPLE_NUMBER, "_R"),]
  substrate_corrected<-substrate_corrected[!substrate_corrected$SAMPLE_NUMBER %in% check$SAMPLE_NUMBER,]
  # substrate_corrected<-subset(substrate_corrected, !str_detect(SAMPLE_NUMBER, "_R"))

  
  samples<-left_join(samples, substrate_corrected, by=c("Site.Code"="Site", "Year"="Year"))
  samples<-subset(samples, Year>2004)
  missing<-unique(samples[is.na(samples$Sand),c("Site.Code", "Year")])### there are as of 2022 53 records with no matching habitat, either because pebble counts weren't done, habitat sheets weren't entered into a database, and/or habitat sheets are missing. see missing_habitat.csv for accounting.
# unique(subset(substrate_corrected, !Site %in% KC_scores$Site.Code)$Site)
# test<-substrate_corrected[,c("Year", "Site")]
# names(test)[2]<-"Site.Code"
# setdiff(test[,c("Year", "Site.Code")], KC_scores[,c("Year", "Site.Code")])
  
#   
  # samples$Sample.Code<-str_replace(samples$Sample.Code, "/", "_")
  # samples$Sample.Code<-str_replace(samples$Sample.Code, "-", "_")
  samples<-samples[!str_detect(samples$Sample.Code, "_R"),]
  samples<-samples[!str_detect(samples$Sample.Code, "[0-9]R[0-9]"),]
  samples<-samples[!str_detect(samples$Sample.Code, "[0-9]R$"),]
  samples<-samples[!str_detect(samples$Sample.Code, "QC"),]
  samples<-samples[!str_detect(samples$Sample.Code, "_r"),]
  samples<-samples[!str_detect(samples$Sample.Code, "Duplicate"),]
  
  samples$Sand<-as.numeric(paste0(samples$Sand))
  samples$D50<-as.numeric(paste0(samples$D50))

  write.csv(samples, "Scores_w_corrected_BFW_substrate.csv")
  
    samples <- ddply(samples, 'Site.Code', mutate, por = length(Sand)) # add on period of record col
  samples<-droplevels(samples[samples$por > 9,])# look only at sites with >9 years of data
  
  samples <- ddply(samples, 'Site.Code', mutate, timerange = (as.numeric(max(Year))-as.numeric(min(Year))))
  samples<-droplevels(samples[samples$timerange > 9,])# look only at streams with >9 years of data
  
  
  cor.test(samples$Overall.Score, samples$Sand, method="spearman")
  cor.test(samples$Overall.Score, samples$D50, method="spearman")
  
  library(rkt)
  names(samples)
  l=unique(c(as.character(samples$Site.Code)))
  samples$Site.Number<-as.numeric(factor(samples$Site.Code, levels=l))
  with(samples, rkt(date = Year, y =Sand, block = Site.Number, correct = FALSE, rep="a"))

 

  data2<-ddply(samples,.(Site.Code),mutate, mk.tau=kendallTrendTest(Sand~ as.numeric(Year))$estimate[1])
  data2<-ddply(data2,.(Site.Code),mutate, mk.slope=kendallTrendTest(Sand~ as.numeric(Year))$estimate[2])
  data2_trends<-unique(subset(data2, select=c(Site.Code, mk.slope, mk.tau))) 
  # KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
  # KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))
  data2_score_trends<-merge(KC_trends, data2_trends,  by.x="Site.Code", by.y="Site.Code")
  summary(data2_score_trends$mk.slope)
  # cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$mk.tau)
  cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$mk.slope, method="spearman")

  with(samples, rkt(date = Year, y =D50, block = Site.Number, correct = FALSE, rep="a"))

  data2<-ddply(samples,.(Site.Code),mutate, mk.tau=kendallTrendTest(D50~ as.numeric(Year))$estimate[1])
  data2<-ddply(data2,.(Site.Code),mutate, mk.slope=kendallTrendTest(D50~ as.numeric(Year))$estimate[2])
  data2_trends<-unique(subset(data2, select=c(Site.Code, mk.slope, mk.tau))) 
  # KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
  # KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))
  data2_score_trends<-merge(KC_trends, data2_trends,  by.x="Site.Code", by.y="Site.Code")
  summary(data2_score_trends$mk.slope)
  # cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$mk.tau)
  cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$mk.slope, method="spearman")
  
  
  KC_trends<-read.csv("Site_Trends_coarse_LR_03092023_2002-2021_LRexclude.csv")
  yearlyBFW5_22_B<-merge(yearlyBFW5_22, KC_trends, by.x="Site", by.y="Site.Code", all.x=T)  
  ggplot(yearlyBFW5_22_B, aes(y=D50, x=as.numeric(paste0(Year)),color=MKtrend_Overall.Score))+geom_point()+facet_wrap(.~Site)+
    theme(legend.position="none")
  ggplot(yearlyBFW5_22_B, aes(y=Sand, x=as.numeric(paste0(Year)),color=MKtrend_Overall.Score))+geom_point()+facet_wrap(.~Site)+
    theme(legend.position="none")
  
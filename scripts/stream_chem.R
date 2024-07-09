library(RSocrata)

library(dplyr)

library(lubridate)

setwd(here::here())

loc_url_portal<-'https://data.kingcounty.gov/resource/wbhs-bbzf.json'

locs<-read.socrata(loc_url_portal) %>%
  
  transmute(SiteName=sitename,
            
            Locator=locator,
            
            lng=as.numeric(longitude),
            
            lat=as.numeric(latitude),
            
            SiteTypeName=site_type,
            
            Area=area) %>%
  
  filter(SiteTypeName=='Streams and Rivers'&!is.na(lng))



data_url_start<-'https://data.kingcounty.gov/resource/vwmt-pvjw.json' #entire wq portal



download_query<-paste0("?$where=",
                       
                       #"locator = '",input$download_site,
                       
                       "(",paste0("locator='",locs$Locator,"'",collapse=' OR '),')',
                       
                       #   " AND collect_datetime between '",
                       
                       #  download_dates[1],"' AND '",download_dates[2],"'",
                       
                       #  " AND (",paste0("parameter='",c('Total Phosphorus','Total Nitrogen'),"'",collapse=' OR '),')',
                       
                       " AND ","NOT qualityid = 9")



data_out<-read.socrata(paste0(data_url_start,download_query)) %>%
  
  transmute(CollectDate=collect_datetime,
            
            Year=year(CollectDate),
            
            Month=month(CollectDate),
            
            LabSampleNum=sample_number,
            
            Locator=locator,
            
            #Depth=depth,
            
            Parameter=parameter,
            
            Value=value,#if_else(is.na(overridevalue),value,overridevalue),
            
            Units=units,
            
            Qualifier=lab_qualifier,
            
            MDL=mdl,
            
            RDL=rdl,
            
            Text=textvalue) %>%
  
  filter(!grepl('/Kg',Units)) %>%
  
  mutate(nonDetect_Flag=grepl('<MDL',Qualifier),
         
         new_Result_Value=ifelse(nonDetect_Flag,MDL,Value)) %>%
  
  mutate(Locator=ifelse(Locator=='FF321','F321',
                        
                        ifelse(Locator=='A632','0632',
                               
                               ifelse(Locator=='N484A','N484',
                                      
                                      ifelse(Locator %in% c('0456','0456A'),'A456',
                                             
                                             Locator)))))

#merge moved sites
data_out3<-data_out

unique(subset(data_out, select=c(Parameter, Units)))
setwd("./inputs")
route<-read.delim("WQI_BIBI3.txt", header=T, sep=',')
WQI_sites<-as.data.frame(str_split(route$Name, " - ", simplify = T))
data_out<-data_out[which(data_out$Locator %in% WQI_sites$V2),]
data_out<-subset(data_out, CollectDate>"2002-01-01")
unique(data_out$Parameter)
setDT(data_out)
data_out[which(data_out$nonDetect_Flag),]
data_out2<-dcast(data_out, CollectDate+Year+Month+LabSampleNum+Locator~Parameter, value.var=c( "new_Result_Value"))
data_out2<-merge(data_out2, WQI_sites, by.x=c("Locator"), by.y = c("V2" ))

unique(data_out2$V1)
names(data_out2)
l=unique(c(as.character(data_out2$V1)))
# str(data_out2)
df_clean<-data_out2[,6:25]
df_clean <- df_clean %>% mutate_if(is.character, as.numeric)
df_clean<-cbind(data_out2[,c(1:5, 26)], df_clean)
df_clean$Site.Number<-as.numeric(factor(df_clean$V1, levels=l))

l=unique(c(as.character(data_out2$Locator)))
df_clean$Site.Number<-as.numeric(factor(df_clean$Locator, levels=l))

df_clean2<-df_clean
df_clean<-df_clean2
# df_clean<-subset(df_clean, Year>=2001)
# df_clean<-subset(df_clean, Month>=5 & Month<=7)
library(ggplot2)
# ggplot(df_clean, aes(y=log(`Total Phosphorus`), x=Year, group=Year))+geom_boxplot()+facet_wrap(~Month)
# ggplot(df_clean, aes(y=log(`Ammonia Nitrogen`), x=Year, group=Year))+geom_boxplot()+facet_wrap(~Month)
# ggplot(df_clean, aes(y=Temperature, x=Year, group=Year))+geom_boxplot()+facet_wrap(~Month)
# ggplot(df_clean, aes(y=`Total Alkalinity`, x=Year, group=Year))+geom_boxplot()+facet_wrap(~Month)
# ggplot(df_clean, aes(y=`Dissolved Oxygen, Field`, x=Year, group=Year))+geom_boxplot()+facet_wrap(~Month)
# ggplot(df_clean, aes(y=log(`Total Suspended Solids`), x=Year, group=Year))+geom_boxplot()+facet_wrap(~Month)
# ggplot(df_clean, aes(y=`Orthophosphate Phosphorus`, x=Year, group=Year))+geom_boxplot()+facet_wrap(~Month)
# ggplot(df_clean, aes(y=`Conductivity, Field`, x=Year, group=Year))+geom_boxplot()+facet_wrap(~Month)

# ggplot(df_clean, aes(y=`Conductivity, Field`, x=as.numeric(Year)))+geom_point(position="jitter")+stat_smooth(method="loess")
# ggplot(df_clean, aes(y=Temperature, x=as.numeric(Year)))+geom_point(position="jitter")+stat_smooth(method="loess")
# ggplot(df_clean, aes(y=`Total Alkalinity`, x=as.numeric(Year)))+geom_point(position="jitter")+stat_smooth(method="loess")
# 
# ggplot(df_clean, aes(y=log(Turbidity), x=Year, group=Year))+geom_boxplot()+facet_wrap(~Month)
# ggplot(df_clean, aes(y=`Nitrite + Nitrate Nitrogen`, x=Year, group=Year))+geom_boxplot()+facet_wrap(~Month)
# ggplot(df_clean, aes(y=log(`Fecal Coliform`), x=Year, group=Year))+geom_boxplot()+facet_wrap(~Month)
# ggplot(df_clean, aes(y=`pH, Field`, x=Year, group=Year))+geom_boxplot()+facet_wrap(~Month)

# 
ddply(df_clean, .(Year), summarize, length(unique(Locator)))


df_clean<-subset(df_clean, select=-c(pH, `Storm Or Non-Storm`, Silica, `Dissolved Oxygen`, Conductivity, Enterococcus,`E. coli`))
# summary(df_clean)
df_clean$block<-as.numeric(paste0(df_clean$Site.Number,"000", df_clean$Month))
metrics<-colnames(df_clean)[7:(ncol(df_clean)-2)]
setwd(here::here())
setwd("./outputs")
write.csv(df_clean, "Stream_chem.csv")
df_clean<-read.csv("Stream_chem.csv", header=T)
metrics<-colnames(df_clean)[8:(ncol(df_clean)-3)]
unique(df_clean$V1)

# ggplot(df_clean, aes(y=log(`Total Phosphorus`), x=Year, group=Year))+geom_boxplot()
# ggplot(df_clean, aes(y=log(Ammonia.Nitrogen), x=Year, group=Year))+geom_boxplot()
# ggplot(df_clean, aes(y=Temperature, x=Year, group=Year))+geom_boxplot()
# ggplot(df_clean, aes(y=Total.Alkalinity, x=Year, group=Year))+geom_boxplot()
# ggplot(df_clean, aes(y=Dissolved.Oxygen..Field, x=Year, group=Year))+geom_boxplot()
# ggplot(df_clean, aes(y=log(Total.Suspended.Solids), x=Year, group=Year))+geom_boxplot()
# ggplot(df_clean, aes(y=Orthophosphate.Phosphorus, x=Year, group=Year))+geom_boxplot()
# ggplot(df_clean, aes(y=Conductivity..Field, x=Year, group=Year))+geom_boxplot()
# ggplot(df_clean, aes(y=Fecal.Coliform, x=Year, group=Year))+geom_boxplot()
# ggplot(df_clean, aes(y=Nitrite...Nitrate.Nitrogen, x=Year, group=Year))+geom_boxplot()
# ggplot(df_clean, aes(y=Total.Nitrogen, x=Year, group=Year))+geom_boxplot()
# ggplot(df_clean, aes(y=Turbidity, x=Year, group=Year))+geom_boxplot()
# ggplot(df_clean, aes(y=pH..Field, x=Year, group=Year))+geom_boxplot()



trends<-NULL

df_clean$Year
df_clean<-droplevels(df_clean[df_clean$Year < 2022,])# look only at sites with >9 years of data

df_clean <- ddply(df_clean, 'Locator', mutate, por = length(unique(Year))) # add on period of record col

df_clean<-droplevels(df_clean[df_clean$por > 9,])# look only at sites with >9 years of data
unique(df_clean$Locator)
# 
# l=unique(c(as.character(df_clean$Locator)))
# df_clean$block<-as.numeric(factor(df_clean$Locator, levels=l))

library(rkt)
with(df_clean, rkt(date = Year, y = Ammonia.Nitrogen, block = block, correct = FALSE, rep="a")[1])

for (each in metrics) {
  print(each)
  trends$rkt.pval <- with(df_clean, rkt(date = Year, y = get(each), block = block, correct = FALSE, rep="a")[1])
  trends$rkt.Sen <- with(df_clean, rkt(date = Year, y = get(each), block = block, correct = FALSE, rep="a")[3])
  trends$rkt.tau <- with(df_clean, rkt(date = Year, y = get(each), block = block, correct = FALSE, rep="a")[12])
  trends$RKTtrend <- with(trends, ifelse(rkt.pval < 0.05 & rkt.tau < 0, "negative", ifelse(rkt.pval < 0.05 & rkt.tau > 0, "positive", "none")))
  trends<-unlist(trends)
  trends<-as.data.frame(t(trends))
  colnames(trends)[ncol(trends)-3]<-paste0("rkt.pval_", each)
  colnames(trends)[ncol(trends)-2]<-paste0("rkt.Sen_", each)
  colnames(trends)[ncol(trends)-1]<-paste0("rkt.tau_", each)
  colnames(trends)[ncol(trends)]<-paste0("RKTtrend_", each)
}
write.csv(trends, "WQI_trends_01192023_2002-2021.csv")



data.trend<-data.frame(Parameter=character(), Site=character(), mk.tau=double(),mk.Slope=double(),mk.pval.het=double(),mk.pval.z=double(), mk.inter=double(), mk.trend=character(), mk.trend.het=character(),stringsAsFactors = F)

metrics<-names(df_clean)[8:20]
library(EnvStats)

for (var in metrics){
  data.lr<-NULL
  data.lr$Parameter<-ddply(df_clean, .(V1), summarize, var)[2]
  data.lr$Site<-ddply(df_clean, .(V1),summarize, mk.Slope=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$estimate[2])[1]
  data.lr$mk.tau<-ddply(df_clean,.(V1),summarize, mk.tau=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$estimate[1])[2]
  data.lr$mk.Slope<-ddply(df_clean,.(V1),summarize, mk.Slope=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$estimate[2])[2]
  data.lr$mk.pval.het<-ddply(df_clean,.(V1),summarize, mk.pval.het=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$p.value)[seq(1, 48, 2),2]
  data.lr$mk.pval.z<-ddply(df_clean,.(V1),summarize, mk.pval.z=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$p.value)[seq(2, 48, 2),2]
  data.lr$mk.inter<-ddply(df_clean,.(V1),summarize, mk.inter=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$estimate[3])[2]
  
  data.lr$Jan.tau<-ddply(df_clean,.(V1),summarize, season.tau=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$seasonal.estimates)[,2][seq(1,288, 12),1]
  data.lr$Feb.tau<-ddply(df_clean,.(V1),summarize, season.tau=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$seasonal.estimates)[,2][seq(2,288, 12),1]
  data.lr$Mar.tau<-ddply(df_clean,.(V1),summarize, season.tau=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$seasonal.estimates)[,2][seq(3,288, 12),1]
  data.lr$Apr.tau<-ddply(df_clean,.(V1),summarize, season.tau=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$seasonal.estimates)[,2][seq(4,288, 12),1]
  data.lr$May.tau<-ddply(df_clean,.(V1),summarize, season.tau=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$seasonal.estimates)[,2][seq(5,288, 12),1]
  data.lr$Jun.tau<-ddply(df_clean,.(V1),summarize, season.tau=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$seasonal.estimates)[,2][seq(6,288, 12),1]
  data.lr$Jul.tau<-ddply(df_clean,.(V1),summarize, season.tau=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$seasonal.estimates)[,2][seq(7,288, 12),1]
  data.lr$Aug.tau<-ddply(df_clean,.(V1),summarize, season.tau=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$seasonal.estimates)[,2][seq(8,288, 12),1]
  data.lr$Sep.tau<-ddply(df_clean,.(V1),summarize, season.tau=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$seasonal.estimates)[,2][seq(9,288, 12),1]
  data.lr$Oct.tau<-ddply(df_clean,.(V1),summarize, season.tau=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$seasonal.estimates)[,2][seq(10,288, 12),1]
  data.lr$Nov.tau<-ddply(df_clean,.(V1),summarize, season.tau=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$seasonal.estimates)[,2][seq(11,288, 12),1]
  data.lr$Dec.tau<-ddply(df_clean,.(V1),summarize, season.tau=kendallSeasonalTrendTest(get(paste0(var)) ~ Month+Year)$seasonal.estimates)[,2][seq(12,288, 12),1]
  
  data.lr$mk.trend <- ifelse(data.lr$mk.pval.z < 0.05 & data.lr$mk.tau < 0, "negative", ifelse(data.lr$mk.pval.z < 0.05 & data.lr$mk.tau > 0, "positive", "none"))
  data.lr$mk.trend.het <- ifelse(data.lr$mk.pval.het <= 0.05, "hetergeneous trends", "none")
  data.lr<- as.data.frame(data.lr)

  colnames(data.lr)<-c("Parameter","Site", "mk.tau", "mk.Slope", "mk.pval.het","mk.pval.z", "mk.inter", "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","trend", "trend.het")
  data.trend<- rbind(data.lr, data.trend)
  
  data.trend %>% mutate_if(is.factor, as.character) -> data.trend
  
}
getwd()
write.csv(data.trend,  "WQI-BIBI_trends_01192023.csv")
library(plyr)
BIBI_scores<-read.csv("B-IBI_results_03082023_2002-2021_NoUnique_subsamp_450_LR_exclude.csv")
BIBI_scores<-subset(BIBI_scores, Tot_Abund>=450)
KC_trends<-read.csv("Site_Trends_coarse_LR_03092023_2002-2021_LRexclude.csv")
KC_trends<-subset(KC_trends, !(Site.Code=="09DUW0277"&Stream.or.River=="Riverton Creek (003D)"))
KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))
BIBI_scores<-subset(BIBI_scores, Site.Code %in% KC_trends$Site.Code)
unique(BIBI_scores$Site.Code)

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
data<-merge(BIBI_scores, mean_vals, by.x=c("Site.Code", "Year"), by.y=c("V1", "wtr_yr"))
data<-data[!str_ends(data$Sample.Code, "_R"),]
data<-data[!str_ends(data$Sample.Code, "-R"),]
data<-data[!str_ends(data$Sample.Code, "_rep"),]
data<-data[!str_ends(data$Sample.Code, "_Rep"),]
data<-data[!str_detect(data$Sample.Code, "QC"),]
data<-data[!str_ends(data$Sample.Code, "R"),]
data<-data[!str_ends(data$Sample.Code, "R1"),]
data<-data[!str_ends(data$Sample.Code, "R2"),]
data<-data[!str_ends(data$Sample.Code, "R3"),]
data[which(duplicated(subset(data, select=c(Site.Code, Year)))),]
data<-data[!str_detect(data$Sample.Code, "JUDD_0"),]
data<-data[!str_detect(data$Sample.Code, "VashJudd-2"),]
data<-data[!str_detect(data$Sample.Code, "VashJudd -3"),]

test<-as.data.frame(subset(mean_vals, select=c(V1, wtr_yr)))
names(test)<-c("Site.Code", "Year")
test$sample<-paste0(test$Site.Code, test$Year)
unique(test)
BIBI_scores$sample<-paste0(BIBI_scores$Site.Code, BIBI_scores$Year)
setdiff(mean_vals$V1, data$Site.Code)
unique(data$Site.Code)
names(data)

cor.test(data$Overall.Score, data$`Ammonia.Nitrogen`, method="spearman")
cor.test(data$Overall.Score, data$`Conductivity..Field`, method="spearman")
cor.test(data$Overall.Score, data$`Dissolved.Oxygen..Field`, method="spearman")
cor.test(data$Overall.Score, data$`Fecal.Coliform`, method="spearman")
cor.test(data$Overall.Score, data$`Nitrite...Nitrate.Nitrogen`, method="spearman")
cor.test(data$Overall.Score, data$`Orthophosphate.Phosphorus`, method="spearman")
cor.test(data$Overall.Score, data$`pH..Field`, method="spearman")
cor.test(data$Overall.Score, data$Temperature, method="spearman")
cor.test(data$Overall.Score, data$`Total.Alkalinity`, method="spearman")
cor.test(data$Overall.Score, data$`Total.Nitrogen`, method="spearman")
cor.test(data$Overall.Score, data$`Total.Phosphorus`, method="spearman")
cor.test(data$Overall.Score, data$`Total.Suspended.Solids`, method="spearman")
# ggplot(data, aes(x=Total.Suspended.Solids, y=Overall.Score))+geom_point()
cor.test(data$Overall.Score, data$Turbidity, method="spearman")



WQI_trends<-read.csv("WQI-BIBI_trends_01192023.csv")

WQI_trends<-unique(subset(WQI_trends, select=c(Parameter, Site, mk.Slope, mk.tau))) 
WQI_trends<-reshape2::dcast(reshape2::melt(WQI_trends, id.vars=c("Site", "Parameter")), Site~variable+Parameter)

# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code))



data2_score_trends<-merge(KC_trends, WQI_trends,  by.x="Site.Code", by.y="Site")
summary(data2_score_trends$`mk.Slope_Ammonia Nitrogen`)
summary(data2_score_trends$`mk.Slope_Conductivity, Field`)
summary(data2_score_trends$`mk.Slope_Dissolved Oxygen, Field`)
summary(data2_score_trends$`mk.Slope_Fecal Coliform`)
summary(data2_score_trends$`mk.Slope_Nitrite + Nitrate Nitrogen`)
summary(data2_score_trends$`mk.Slope_Orthophosphate Phosphorus`)
summary(data2_score_trends$`mk.Slope_pH, Field`)
summary(data2_score_trends$mk.Slope_Temperature)
summary(data2_score_trends$`mk.Slope_Total Alkalinity`)
summary(data2_score_trends$`mk.Slope_Total Nitrogen`)
summary(data2_score_trends$`mk.Slope_Total Phosphorus`)
summary(data2_score_trends$`mk.Slope_Total Suspended Solids`)
summary(data2_score_trends$mk.Slope_Turbidity)

unique(data2_score_trends$Site.Code)
# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$`mk.tau_Ammonia Nitrogen`)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$`mk.Slope_Ammonia.Nitrogen`, method="spearman")
# ggplot(data2_score_trends, aes(y=MKSlope_Overall.Score, x=slope))+geom_point()

# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$`mk.tau_Conductivity, Field`)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$`mk.Slope_Conductivity..Field`, method="spearman")
# ggplot(data2_score_trends, aes(y=MKSlope_Overall.Score, x=slope))+geom_point()

# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$`mk.tau_Dissolved Oxygen, Field`)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$`mk.Slope_Dissolved.Oxygen..Field`, method="spearman")
# ggplot(data2_score_trends, aes(y=MKSlope_Overall.Score, x=`mk.Slope_Dissolved Oxygen, Field`))+geom_point()

# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$`mk.tau_Fecal Coliform`)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$`mk.Slope_Fecal.Coliform`, method="spearman")
# ggplot(data2_score_trends, aes(y=MKSlope_Overall.Score, x=slope))+geom_point()

# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$`mk.tau_Nitrite + Nitrate Nitrogen`)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$`mk.Slope_Nitrite...Nitrate.Nitrogen`, method="spearman")
# ggplot(data2_score_trends, aes(y=MKSlope_Overall.Score, x=slope))+geom_point()

# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$`mk.tau_Orthophosphate Phosphorus`)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$`mk.Slope_Orthophosphate.Phosphorus`, method="spearman")
# ggplot(data2_score_trends, aes(y=MKSlope_Overall.Score, x=slope))+geom_point()

# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$`mk.tau_pH, Field`)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$`mk.Slope_pH..Field`, method="spearman")
# ggplot(data2_score_trends, aes(y=MKSlope_Overall.Score, x=slope))+geom_point()

# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$mk.tau_Temperature)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$mk.Slope_Temperature, method="spearman")
# ggplot(data2_score_trends, aes(y=MKSlope_Overall.Score, x=slope))+geom_point()

# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$`mk.tau_Total Alkalinity`)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$`mk.Slope_Total.Alkalinity`, method="spearman")
# ggplot(data2_score_trends, aes(y=MKSlope_Overall.Score, x=slope))+geom_point()

# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$`mk.tau_Total Nitrogen`)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$`mk.Slope_Total.Nitrogen`, method="spearman")
# ggplot(data2_score_trends, aes(y=MKSlope_Overall.Score, x=slope))+geom_point()

# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$`mk.tau_Total Phosphorus`)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$`mk.Slope_Total.Phosphorus`, method="spearman")
# ggplot(data2_score_trends, aes(y=MKSlope_Overall.Score, x=slope))+geom_point()

# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$`mk.tau_Total Suspended Solids`)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$`mk.Slope_Total.Suspended.Solids`, method="spearman")
# ggplot(data2_score_trends, aes(y=MKSlope_Overall.Score, x=slope))+geom_point()

# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$mk.tau_Turbidity)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$mk.Slope_Turbidity, method="spearman")
# ggplot(data2_score_trends, aes(y=MKSlope_Overall.Score, x=slope))+geom_point()

# ggplot(data, aes(x=log(Ammonia.Nitrogen), y=Overall.Score, color=Site.Code))+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
#   
# ggplot(data2, aes(x=Conductivity..Field, y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data, aes(x=Dissolved.Oxygen..Field, y=Overall.Score, color=Site.Code))+ geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data, aes(x=Dissolved.Oxygen..Field, y=Overall.Score))+ geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# 
# ggplot(data, aes(x=log(Fecal.Coliform), y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data, aes(x=Nitrite...Nitrate.Nitrogen, y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data2, aes(x=Orthophosphate.Phosphorus, y=Overall.Score, color=Site.Code))+ geom_line() +geom_point() + #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data, aes(x=Temperature, y=Overall.Score, color=Site.Code))+ geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data, aes(x=Total.Nitrogen, y=Overall.Score, color=Site.Code))+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data2, aes(x=Total.Alkalinity, y=Overall.Score, color=Site.Code))+ geom_line() +geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data, aes(x=log(Total.Suspended.Solids), y=Overall.Score, color=Site.Code))+ geom_line() +geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data2, aes(x=Total.Phosphorus, y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x,se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data, aes(x=log(Turbidity), y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data, aes(x=pH..Field, y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# 
# 
# mean_summer_vals<-df_clean %>% group_by(V1, wtr_yr) %>% filter(Month %in% c(6,7,8,9)) %>%  summarize_at(metrics, mean)
# data_summer<-merge(BIBI_scores, mean_summer_vals, by.x=c("Site.Code", "Year"), by.y=c("V1", "wtr_yr"))
# 
# 
# ggplot(data_summer, aes(x=log(Ammonia.Nitrogen), y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_summer, aes(x=Conductivity..Field, y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_summer, aes(x=Dissolved.Oxygen..Field, y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_summer, aes(x=log(Fecal.Coliform), y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_summer, aes(x=Nitrite...Nitrate.Nitrogen, y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_summer, aes(x=Orthophosphate.Phosphorus, y=Overall.Score, color=Site.Code))+ geom_line() +geom_point() + #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_summer, aes(x=Temperature, y=Overall.Score, color=Site.Code))+ geom_line() +geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_summer, aes(x=Total.Nitrogen, y=Overall.Score, color=Site.Code))+ geom_line() +geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_summer, aes(x=Total.Alkalinity, y=Overall.Score, color=Site.Code))+ geom_line() +geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_summer, aes(x=log(Total.Suspended.Solids), y=Overall.Score, color=Site.Code))+ geom_line() +geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_summer, aes(x=Total.Phosphorus, y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_summer, aes(x=log(Turbidity), y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_summer, aes(x=pH..Field, y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# 
# 
# BIBI_scores$Month<-format(as.Date(BIBI_scores$Visit.Date, '%m/%d/%Y'), "%m")
# BIBI_scores$Month<-gsub("(\\D)0|^0", "\\1", BIBI_scores$Month)
# data_nearest<-merge(BIBI_scores, df_clean, by.x=c("Site.Code", "Year", "Month"), by.y=c("V1", "wtr_yr", "Month"))
# 
# ggplot(data_nearest, aes(x=log(Ammonia.Nitrogen), y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_nearest, aes(x=Conductivity..Field, y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_nearest, aes(x=Dissolved.Oxygen..Field, y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_nearest, aes(x=log(Fecal.Coliform), y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_nearest, aes(x=Nitrite...Nitrate.Nitrogen, y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_nearest, aes(x=Orthophosphate.Phosphorus, y=Overall.Score, color=Site.Code))+ geom_line() +geom_point() + #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_nearest, aes(x=Temperature, y=Overall.Score, color=Site.Code))+ geom_line() +geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_nearest, aes(x=Total.Nitrogen, y=Overall.Score, color=Site.Code))+ geom_line() +geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_nearest, aes(x=Total.Alkalinity, y=Overall.Score, color=Site.Code))+ geom_line() +geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_nearest, aes(x=log(Total.Suspended.Solids), y=Overall.Score, color=Site.Code))+ geom_line() +geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_nearest, aes(x=Total.Phosphorus, y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_nearest, aes(x=log(Turbidity), y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(data_nearest, aes(x=pH..Field, y=Overall.Score, color=Site.Code))+ geom_line()+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ##Ammonia, ortho P, Alk, Tot P in WY each explain more than 30% of variation. See how correlated they are, and with urban
# library(Hmisc)
# library(corrplot)
# row.names(df_clean)<-df_clean$LabSampleNum
# df_clean_consolid<-subset(mean_vals, select=c("Ammonia.Nitrogen", "Conductivity..Field","Dissolved.Oxygen..Field", "Orthophosphate.Phosphorus", "Total.Alkalinity", "Total.Phosphorus"))
# res_consoid<-cor(df_clean_consolid, use="na.or.complete")
# round(res_consoid, 2)
# corrplot(res_consoid, order="hclust", hclust.method = "average", tl.col="black", tl.srt=45)
# res2<-rcorr(as.matrix(df_clean_consolid), type="pearson")
# corrplot(res2$r, order="hclust", hclust.method = "average", p.mat=res2$P, sig.level=.05, insig="pch", tl.col="black", tl.srt=45, tl.cex=.5, addCoefasPercent=T,  addCoef.col="black")
# corrplot(res2$r, order="FPC",  p.mat=res2$P, sig.level=.05, insig="pch", tl.col="black", tl.srt=45, tl.cex=.5)
# 
# 
# LU<-read.csv("G:/GreenWQA/Biota/BenthosProjects_KC/trends analysis/Data_for_Trends_FWS_manuscript/CAP/interpolated_impervious.csv", header=T)
# LU_WQI<-merge(mean_vals, LU, by.x=c("wtr_yr", "V1"), by.y=c("Year", "Name"))
# consolid<-subset(LU_WQI, select=c("Ammonia.Nitrogen", "Conductivity..Field","Dissolved.Oxygen..Field", "Orthophosphate.Phosphorus", "Total.Alkalinity", "Total.Phosphorus", "Impervious.Cover"))
# res<-rcorr(as.matrix(consolid), type="pearson")
# corrplot(res$r, order="hclust", hclust.method = "average", p.mat=res$P, sig.level=.05, insig="pch", tl.col="black", tl.srt=45, tl.cex=.5, addCoefasPercent=T,  addCoef.col="black")
# 
# 
# LU_BIBI<-merge(BIBI_scores, LU, by.x=c("Site.Code", "Year"), by.y=c("Name", "Year"))
# 
# ggplot(LU_BIBI, aes(x=Impervious.Cover, y=Overall.Score, color=Site.Code))+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# ggplot(LU, aes(x=Year, y=Impervious.Cover, color=Name))+geom_line()
# 
# ggplot(BIBI_scores, aes(x=Year, y=Overall.Score, color=Site.Code))+geom_point()+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
#   stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)+
#   stat_poly_eq(inherit.aes=T, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)
# 
# 
# library(MASS)
# model<-lm(Overall.Score~Site.Code+Ammonia.Nitrogen+Conductivity..Field+Dissolved.Oxygen..Field+Fecal.Coliform+Nitrite...Nitrate.Nitrogen+Orthophosphate.Phosphorus+Temperature+Total.Alkalinity+Total.Nitrogen+Total.Phosphorus+Total.Suspended.Solids+Turbidity+pH..Field, data=data)
# stepAIC(model, trace=T)
# is.na(data)
# data2<-subset(data, select=c(Site.Code,Overall.Score,Ammonia.Nitrogen,Dissolved.Oxygen..Field,Temperature,Total.Alkalinity,Total.Phosphorus,Conductivity..Field,Nitrite...Nitrate.Nitrogen, Orthophosphate.Phosphorus, Total.Suspended.Solids, Turbidity))
# data2<-data2[complete.cases(data2),]
# # model<-lm(Site.Code+Overall.Score~log(Ammonia.Nitrogen)+Conductivity..Field+Dissolved.Oxygen..Field+Nitrite...Nitrate.Nitrogen+Orthophosphate.Phosphorus+Temperature+Total.Alkalinity+Total.Phosphorus+Total.Suspended.Solids+log(Turbidity), data=data2)
# # modelaic<-stepAIC(model, trace=T)
# # modelaic
# hist(data2$Overall.Score)
# shapiro.test(data2$Overall.Score)
# 
# 
# model<-lm(Overall.Score~log(Ammonia.Nitrogen)+Conductivity..Field+Dissolved.Oxygen..Field+Nitrite...Nitrate.Nitrogen+Orthophosphate.Phosphorus+Temperature+Total.Alkalinity+Total.Phosphorus+Total.Suspended.Solids+log(Turbidity)+Site.Code, data=data2)
# stepmodel<-step(model, direction="both")
# # model<-lm(formula = Overall.Score ~ log(Ammonia.Nitrogen) + Conductivity..Field + 
# #      Dissolved.Oxygen..Field + Nitrite...Nitrate.Nitrogen + Orthophosphate.Phosphorus + 
# #      Total.Alkalinity, data = data2)
# anova(stepmodel)
# model<-lm(formula = Overall.Score ~ log(Ammonia.Nitrogen) + Conductivity..Field + 
#             Dissolved.Oxygen..Field + Orthophosphate.Phosphorus+Site.Code, data = data2)
# anova(model)
# data2$log.ammonia<-log(data2$Ammonia.Nitrogen)
# data2$log.turbidity<-log(data2$Turbidity)
# data2[,3:ncol(data2)]<-as.data.frame(scale(data2[,3:ncol(data2)]))
# model<-lmer(Overall.Score ~ log.ammonia + Conductivity..Field + 
#               Dissolved.Oxygen..Field + Orthophosphate.Phosphorus + Temperature + 
#               Total.Alkalinity + Total.Phosphorus + log.turbidity+(1|Site.Code), data=data2)
# model<-lmer(Overall.Score~log.ammonia+Conductivity..Field+Dissolved.Oxygen..Field+Nitrite...Nitrate.Nitrogen+Orthophosphate.Phosphorus+Temperature+Total.Alkalinity+Total.Phosphorus+Total.Suspended.Solids+log.turbidity+(1|Site.Code), data=data2)
# model2<-lmer(Overall.Score~log.ammonia+Dissolved.Oxygen..Field+Temperature+Total.Alkalinity+(1|Site.Code), data=data2)
# 
# summary(model)
# 
# # data2$log.ammonia<-log(data2$Ammonia.Nitrogen)
# # data2$log.turbidity<-log(data2$Turbidity)
# # data2[,3:ncol(data2)]<-as.data.frame(scale(data2[,3:ncol(data2)]))
# # model<-lmer(Overall.Score~Conductivity..Field+Dissolved.Oxygen..Field+Nitrite...Nitrate.Nitrogen+Orthophosphate.Phosphorus+Temperature+Total.Alkalinity+Total.Phosphorus+Total.Suspended.Solids+Turbidity+log.ammonia+(1|Site.Code), data=data2)
# # summary(model)
# # step(model, direction="both")
# plot(model)
# qqnorm(resid(model))
# qqline(resid(model))
# # model<-lmer(Overall.Score ~ Ammonia.Nitrogen + Dissolved.Oxygen..Field + Temperature + log.ammonia + (1 | Site.Code), data=data2)
# # ggplot(data2, aes(x=Ammonia.Nitrogen, y=Overall.Score, colour=Site.Code))+geom_point()+theme_classic()+geom_line(data=cbind(data2, pred=predict(model)), aes(y=pred), size=1)+theme(legend.position = "none")
# ggplot(data2, aes(x=Dissolved.Oxygen..Field, y=Overall.Score, colour=Site.Code))+geom_point()+theme_classic()+geom_line(data=cbind(data2, pred=predict(model)), aes(y=pred), size=1)+theme(legend.position = "none")
# ggplot(data2, aes(x=Temperature, y=Overall.Score, colour=Site.Code))+geom_point()+theme_classic()+geom_line(data=cbind(data2, pred=predict(model)), aes(y=pred), size=1)+theme(legend.position = "none")
# ggplot(data2, aes(x=log.ammonia, y=Overall.Score, colour=Site.Code))+geom_point()+theme_classic()+geom_line(data=cbind(data2, pred=predict(model)), aes(y=pred), size=1)+theme(legend.position = "none")
# 
# ggplot(data2, aes(x=Conductivity..Field, y=Overall.Score, colour=Site.Code))+geom_point()+theme_classic()+geom_line(data=cbind(data2, pred=predict(model)), aes(y=pred), size=1)+theme(legend.position = "none")
# ggplot(data2, aes(x=Total.Alkalinity, y=Overall.Score, colour=Site.Code))+geom_point()+theme_classic()+geom_line(data=cbind(data2, pred=predict(model)), aes(y=pred), size=1)+theme(legend.position = "none")
# ggplot(data2, aes(x=Orthophosphate.Phosphorus, y=Overall.Score, colour=Site.Code))+geom_point()+theme_classic()+geom_line(data=cbind(data2, pred=predict(model)), aes(y=pred), size=1)+theme(legend.position = "none")
# ggplot(data2, aes(x=log.turbidity, y=Overall.Score, colour=Site.Code))+geom_point()+theme_classic()+geom_line(data=cbind(data2, pred=predict(model)), aes(y=pred), size=1)+theme(legend.position = "none")
# ggplot(data2, aes(x=Total.Phosphorus, y=Overall.Score, colour=Site.Code))+geom_point()+theme_classic()+geom_line(data=cbind(data2, pred=predict(model)), aes(y=pred), size=1)+theme(legend.position = "none")
# 
# 
# 
# model<-lmer(Overall.Score~Ammonia.Nitrogen+Conductivity..Field+Dissolved.Oxygen..Field+Nitrite...Nitrate.Nitrogen+Orthophosphate.Phosphorus+Temperature+Total.Alkalinity+Total.Phosphorus+Total.Suspended.Solids+Turbidity+(1|Site.Code), data=data2)
# summary(model)
# step(model, direction="both")
# plot(model)
# qqnorm(resid(model))
# qqline(resid(model))
# 
# model<-lmer(Overall.Score ~ Dissolved.Oxygen..Field + Temperature + Total.Alkalinity + Total.Phosphorus + (1 | Site.Code), data=data2)
# 
# ggplot(data2, aes(x=Dissolved.Oxygen..Field, y=Overall.Score, colour=Site.Code))+geom_point()+theme_classic()+geom_line(data=cbind(data2, pred=predict(model)), aes(y=pred), size=1)+theme(legend.position = "none")+stat_smooth(inherit.aes=T,method = "lm", formula = y ~ x, se=F)
# ggplot(data2, aes(x=Temperature, y=Overall.Score, colour=Site.Code))+geom_point()+theme_classic()+geom_line(data=cbind(data2, pred=predict(model)), aes(y=pred), size=1)+theme(legend.position = "none")
# ggplot(data2, aes(x=Total.Alkalinity, y=Overall.Score, colour=Site.Code))+geom_point()+theme_classic()+geom_line(data=cbind(data2, pred=predict(model)), aes(y=pred), size=1)+theme(legend.position = "none")
# ggplot(data2, aes(x=Total.Phosphorus, y=Overall.Score, colour=Site.Code))+geom_point()+theme_classic()+geom_line(data=cbind(data2, pred=predict(model)), aes(y=pred), size=1)+theme(legend.position = "none")
# plot(model)
# 
# model<-lmer(Overall.Score~Temperature+log.ammonia+Total.Phosphorus+Total.Alkalinity+ Dissolved.Oxygen..Field+(1 | Site.Code), data=data2)
# summary(model)
# step(model, direction="both")
# plot(model)
# qqnorm(resid(model))
# qqline(resid(model))
# 
# 
# 
# KC_taxa<-read.csv("KC_noAmbig_noReps_Rolledup_density_02092021.csv")
# KC_taxa$year <- format(as.Date(KC_taxa$CollectionDate, '%m/%d/%Y'), "%Y") 
# density_re<-reshape2::melt(KC_taxa, id.vars=c(1:4, (ncol(KC_taxa)-3):ncol(KC_taxa)), value.name="Density")
# density_org<-ddply(density_re, .(STAID, CollectionDate, Visit.ID, Stream, Basin, year), summarize, sum_orgs=sum(Density))
# 
# combined<-merge(data, density_org, by.x=c("Site.Code", "Year"), by.y=c("STAID", "year"))
# ggplot(combined, aes(x=Total.Phosphorus, y=sum_orgs, color=Year, group=Year))+geom_smooth(method="lm", se=F)
# ggplot(combined, aes(x=Ammonia.Nitrogen, y=sum_orgs, color=Year, group=Year))+geom_smooth(method="lm", se=F)
# ggplot(combined, aes(x=Temperature, y=sum_orgs, color=Year, group=Year))+geom_smooth(method="lm", se=F)+facet_wrap(.~Year)
# ggplot(combined, aes(x=Total.Alkalinity, y=sum_orgs, color=Year, group=Year))+geom_smooth(method="lm", se=F)
# ggplot(combined, aes(x=Dissolved.Oxygen..Field, y=sum_orgs, color=Year, group=Year))+geom_smooth(method="lm", se=F)+facet_wrap(.~Year)
# ggplot(combined, aes(x=Total.Suspended.Solids, y=sum_orgs, color=Year, group=Year))+geom_smooth(method="lm", se=F)+facet_wrap(.~Year)
# ggplot(combined, aes(x=Orthophosphate.Phosphorus, y=sum_orgs, color=Year, group=Year))+geom_smooth(method="lm", se=F)
# ggplot(combined, aes(x=Conductivity..Field, y=sum_orgs, color=Year, group=Year))+geom_smooth(method="lm", se=F)
# ggplot(combined, aes(x=Turbidity, y=sum_orgs, color=Year, group=Year))+geom_smooth(method="lm", se=F)
# ggplot(combined, aes(x=Nitrite...Nitrate.Nitrogen, y=sum_orgs, color=Year, group=Year))+geom_smooth(method="lm", se=F)+facet_wrap(.~Year)
# ggplot(combined, aes(x=Fecal.Coliform, y=sum_orgs, color=Year, group=Year))+geom_smooth(method="lm", se=F)
# ggplot(combined, aes(x=pH..Field, y=sum_orgs, color=Year, group=Year))+geom_smooth(method="lm", se=F)
# 
# ggplot(combined, aes(y=Total.Phosphorus, x=Year, group=Year))+geom_boxplot()
# ggplot(combined, aes(y=Ammonia.Nitrogen, x=Year, group=Year))+geom_boxplot()
# ggplot(combined, aes(y=Temperature, x=Year, group=Year))+geom_boxplot()
# ggplot(combined, aes(y=Total.Alkalinity, x=Year, group=Year))+geom_boxplot()
# ggplot(combined, aes(y=Dissolved.Oxygen..Field, x=Year, group=Year))+geom_boxplot()
# ggplot(combined, aes(y=Total.Suspended.Solids, x=Year, group=Year))+geom_boxplot()
# ggplot(combined, aes(y=Orthophosphate.Phosphorus, x=Year, group=Year))+geom_boxplot()
# ggplot(combined, aes(y=Conductivity..Field, x=Year, group=Year))+geom_boxplot()
# ggplot(combined, aes(y=Turbidity, x=Year, group=Year))+geom_boxplot()
# ggplot(combined, aes(y=Nitrite...Nitrate.Nitrogen, x=Year, group=Year))+geom_boxplot()
# ggplot(combined, aes(y=Fecal.Coliform, x=Year, group=Year))+geom_boxplot()
# ggplot(combined, aes(y=pH..Field, x=Year, group=Year))+geom_boxplot()
# subset(combined, Year==2009, select="Total.Alkalinity")

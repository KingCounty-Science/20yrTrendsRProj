library("pastecs")
library("vegan")
library("ggplot2")
library("reshape2")
library("pvclust")
library("stringr")
library("openxlsx")
library(quantreg)
library(dplyr)
library(ggpubr)
library(tidyverse)

setwd(here::here())
setwd("./outputs")
# Trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv", header=T)

# summ<-read.csv("score_RKTtrend_summarized_by_site_01182023_2002-2021.csv")

####Load BIBI data #### 
KC_scores<-read.csv("B-IBI_results_03082023_2002-2021_NoUnique_subsamp_450_LR_exclude.csv")
# KC_taxa<-read.csv("KC_noAmbig_noReps_Rolledup_density_1162020.csv")
# 
# KC_scores<-KC_scores[which(KC_scores$Sample.Code %in% KC_taxa$Visit.ID),] ## remove replicates
# KC_scores[KC_scores$Sample.Code=="09DUW0277/05","Site.Code"]<-"09DUW0277"
#### Bring in Impervious cover data####
# add_envdata<-read.csv("CAP/ENV_data.csv")
# add_envdata$Year<-as.numeric(paste0(add_envdata$Year))
# add_envdata[add_envdata<0]<-0
# Imp.Cov<-subset(add_envdata, select=c(Year, SiteName, Impervious.Cover))

# LU<-read.csv("G:/GreenWQA/Biota/GIS/KC_basins/NLCD 2001to2019 183Basins 181Buffers/NLCD_LULC_2001-2019.csv", header = T)
setwd(here::here())
setwd("./inputs/NLCD 2001to2019 183Basins 181Buffers")
LU<-read.xlsx("Stats_PctDev_Basins_2001to2019.xlsx", sheet = 1, rows = c(7:190))

LU<-subset(LU, LU$Name!="09SOO1209")
# LU<-read.csv("LandUse.csv")
# LU2<-subset(LU, select=c(Name, imp01_per, imp06_per,  imp11_per,  imp16_per))
LU2<-reshape2::melt(LU, id.vars="Name", value.name="Impervious.Cover")

LU2$Year<-str_split(LU2$variable, "_", simplify = T)[,3]
LU2$Year<-as.numeric(paste0(LU2$Year))

##Add year##
KC_scores$Year<-as.numeric(paste0(KC_scores$Year))

metrics<-names(KC_scores)[3:27]
each<-metrics[25]
# for (each in metrics){
detach(package:plyr)
detach(package:dplyr)
library(dplyr)
m2001<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2002"|Year=="2003") %>% summarize(m2001=mean(get(each), na.rm=T))
m2004<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2005") %>% summarize(m2004=mean(get(each), na.rm=T))
m2006<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2006"|Year=="2007") %>% summarize(m2006=mean(get(each), na.rm=T))
m2008<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2008"|Year=="2009"|Year=="2010") %>% summarize(m2008=mean(get(each), na.rm=T))
m2011<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2011"|Year=="2012") %>% summarize(m2011=mean(get(each), na.rm=T))
m2013<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2013"|Year=="2014"|Year=="2015") %>% summarize(m2013=mean(get(each), na.rm=T))
m2016<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2016"|Year=="2017"|Year=="2018") %>% summarize(m2016=mean(get(each), na.rm=T))
m2019<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2019"|Year=="2020"|Year=="2021") %>% summarize(m2019=mean(get(each), na.rm=T))

# m2001<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2002") %>% summarize(m2001=mean(get(each), na.rm=T))
# m2004<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2003") %>% summarize(m2004=mean(get(each), na.rm=T))
# m2006<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2006") %>% summarize(m2006=mean(get(each), na.rm=T))
# m2008<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2008") %>% summarize(m2008=mean(get(each), na.rm=T))
# m2011<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2011") %>% summarize(m2011=mean(get(each), na.rm=T))
# m2013<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2013") %>% summarize(m2013=mean(get(each), na.rm=T))
# m2016<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2016") %>% summarize(m2016=mean(get(each), na.rm=T))
# m2019<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2019") %>% summarize(m2019=mean(get(each), na.rm=T))
# 
# m2001<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2002") %>% summarize(m2001=mean(get(each), na.rm=T))
# m2004<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2005"|Year=="2003") %>% summarize(m2004=mean(get(each), na.rm=T))
# m2006<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2005"|Year=="2006"|Year=="2007") %>% summarize(m2006=mean(get(each), na.rm=T))
# m2008<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2007"|Year=="2008"|Year=="2009") %>% summarize(m2008=mean(get(each), na.rm=T))
# m2011<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2010"|Year=="2011"|Year=="2012") %>% summarize(m2011=mean(get(each), na.rm=T))
# m2013<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2012"|Year=="2013"|Year=="2014") %>% summarize(m2013=mean(get(each), na.rm=T))
# m2016<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2015"|Year=="2016"|Year=="2017") %>% summarize(m2016=mean(get(each), na.rm=T))
# m2019<-KC_scores%>% group_by(Site.Code) %>% filter(Year=="2018"|Year=="2019"|Year=="2020") %>% summarize(m2019=mean(get(each), na.rm=T))
# 


Avg_scores<-list(m2001, m2004, m2006, m2008, m2011,m2013, m2016, m2019) %>%reduce(full_join)
Avg_scores<-Avg_scores[complete.cases(Avg_scores),]
Avg_scores<-reshape2::melt(Avg_scores, id.vars="Site.Code", variable.name="Year")
Avg_scores$Year<-str_remove(Avg_scores$Year, "m")
Avg_scores$Year<-as.numeric(paste0(Avg_scores$Year))
Imp.Cov<-left_join( Avg_scores, LU2, by=c("Site.Code"="Name", "Year"="Year"))

# ##run quantile regression for 2001
# mod01 <- rq(value ~ Impervious.Cover, data = Imp.Cov[Imp.Cov$Year==2001,], tau=0.5)
# pred01 <- as.data.frame(predict(mod01, data.frame(Impervious.Cover = Imp.Cov[Imp.Cov$Year==2001,]$Impervious.Cover),
#                                 interval = "confidence"))
# pred01$Site.Code <-Imp.Cov[Imp.Cov$Year==2001,]$Site.Code
# pred01$Year <- 2001
# 
# ##run quantile regression for 2001
# mod04 <- rq(value ~ Impervious.Cover, data = Imp.Cov[Imp.Cov$Year==2004,], tau=0.5)
# pred04 <- as.data.frame(predict(mod04, data.frame(Impervious.Cover = Imp.Cov[Imp.Cov$Year==2004,]$Impervious.Cover),
#                                 interval = "confidence"))
# pred04$Site.Code <-Imp.Cov[Imp.Cov$Year==2004,]$Site.Code
# pred04$Year <- 2004
# 
# ##quantile regression for 2006
# mod06 <- rq(value ~ Impervious.Cover, data = Imp.Cov[Imp.Cov$Year==2006,], tau=0.5)
# pred06 <- as.data.frame(predict(mod06, data.frame(Impervious.Cover = Imp.Cov[Imp.Cov$Year==2006,]$Impervious.Cover),
#                                 interval = "confidence"))
# pred06$Site.Code <- Imp.Cov[Imp.Cov$Year==2006,]$Site.Code
# pred06$Year <- 2006
# 
# ##quantile regression for 2008
# mod08 <- rq(value ~ Impervious.Cover, data = Imp.Cov[Imp.Cov$Year==2008,], tau=0.5)
# pred08 <- as.data.frame(predict(mod08, data.frame(Impervious.Cover = Imp.Cov[Imp.Cov$Year==2008,]$Impervious.Cover),
#                                 interval = "confidence"))
# pred08$Site.Code <- Imp.Cov[Imp.Cov$Year==2008,]$Site.Code
# pred08$Year <- 2008
# 
# ##quantile regression for 2011
# mod11 <- rq(value ~ Impervious.Cover, data = Imp.Cov[Imp.Cov$Year==2011,], tau=0.5)
# pred11 <- as.data.frame(predict(mod11, data.frame(Impervious.Cover = Imp.Cov[Imp.Cov$Year==2011,]$Impervious.Cover),
#                                 interval = "confidence"))
# pred11$Site.Code <- Imp.Cov[Imp.Cov$Year==2011,]$Site.Code
# pred11$Year <- 2011
# 
# ##quantile regression for 2013
# mod13 <- rq(value ~ Impervious.Cover, data = Imp.Cov[Imp.Cov$Year==2013,], tau=0.5)
# pred13 <- as.data.frame(predict(mod13, data.frame(Impervious.Cover = Imp.Cov[Imp.Cov$Year==2013,]$Impervious.Cover),
#                                 interval = "confidence"))
# pred13$Site.Code <- Imp.Cov[Imp.Cov$Year==2013,]$Site.Code
# pred13$Year <- 2013
# 
# ##quantile regression for 2016
# mod16 <- rq(value ~ Impervious.Cover, data = Imp.Cov[Imp.Cov$Year==2016,], tau = 0.5)
# pred16 <- as.data.frame(predict(mod16, data.frame(Impervious.Cover = Imp.Cov[Imp.Cov$Year==2016,]$Impervious.Cover),
#                                 interval = "confidence"))
# pred16$Site.Code <- Imp.Cov[Imp.Cov$Year==2016,]$Site.Code
# pred16$Year <- 2016
# 
# ##quantile regression for 2019
# mod19 <- rq(value ~ Impervious.Cover, data = Imp.Cov[Imp.Cov$Year==2019,], tau = 0.5)
# pred19 <- as.data.frame(predict(mod19, data.frame(Impervious.Cover = Imp.Cov[Imp.Cov$Year==2019,]$Impervious.Cover),
#                                 interval = "confidence"))
# pred19$Site.Code <- Imp.Cov[Imp.Cov$Year==2019,]$Site.Code
# pred19$Year <- 2019
# 
# ##combine pred datasets
# pred <- list(pred01,pred04, pred06,pred08, pred11, pred13, pred16,pred19) %>%
#   reduce(full_join)
# 
# 
# 
# mod<-rq(value~ Impervious.Cover+Year, data=Imp.Cov, tau = 0.5)
# summary(mod, covariance=T)
# mod2<-rq(value~ Impervious.Cover*Year, data=Imp.Cov, tau = 0.5)
# summary(mod2,  covariance=T)
# mod3<-rq(value~ Impervious.Cover, data=Imp.Cov, tau = 0.5)
# summary(mod3,  covariance=T)
# mod4<-rq(value~ 1, data=Imp.Cov, tau = 0.5)
# summary(mod4,  covariance=T)
# 
# 
# anova(mod4, mod3)
# anova(mod3, mod)
# anova(mod, mod2)
# anova(mod4, mod2)
Imp.Cov$Year2<-as.factor(Imp.Cov$Year)
model1<-lm(value~ Impervious.Cover*Year2, data=Imp.Cov)
anova(model1)
library(lsmeans)
m.lst <- lstrends(model1, "Year2", var="Impervious.Cover")
pairs(m.lst)


test<-aov(value~ Impervious.Cover*Year2, data=Imp.Cov)
summary(test)
testTuk<-TukeyHSD(test)
#combine pred to original dataset
# Imp.Cov <- Imp.Cov %>%
#   left_join(pred, by = c("Site.Code", "Year"))



##make plot
g1 <- ggplot(Imp.Cov, aes(Impervious.Cover, value)) +
  geom_point(aes(colour=factor(Year))) +
  geom_quantile(quantiles = c(.5), aes(group=factor(Year),
                                     colour=factor(Year)), size=1) +
  # scale_linetype_manual(values=c("dotted", "solid", "dotted"))+

  # geom_ribbon( aes(ymin = lower, ymax= higher,
  #                 fill=factor(Year)),
  #             alpha = 0.4) +
  xlab("% Impervious Cover") +
  ylab("B-IBI Score") +
  labs(colour="Year",fill="Year")


g1

ggsave(filename = paste0("KC_ic_overtime_CI_2_test", each, ".png"),
       plot = g1,
       width = 6, height = 3, units = "in")

Imp.Cov$bin<-cut(Imp.Cov$Impervious.Cover, c(-1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
g1 <- ggplot(Imp.Cov,aes(x=bin, y=value, fill=factor(Year))) +

  geom_boxplot()+
  geom_point(aes(fill=factor(Year)), shape=21, position=position_jitterdodge(), size=.85)+
  # geom_dotplot(binaxis = 'y', stackdir = 'center', position=position_dodge(), dotsize=.5)
  # geom_ribbon( aes(ymin = lower, ymax= higher, 
  #                 fill=factor(Year)),
  #             alpha = 0.4) +
  xlab("% Impervious Cover") +
  ylab("B-IBI Score") +
  labs(fill="Year")


g1
ggsave(filename = paste0("KC_ic_overtime_CI_bp", each, ".png"),
       plot = g1,
       width = 6, height = 3, units = "in")





Imp.Cov$Year2<-as.factor(Imp.Cov$Year)
ggscatter(Imp.Cov, x="Impervious.Cover", y="value", color = "Year2", add="reg.line")+
  stat_regline_equation(aes(label=paste(..eq.label..,..rr.label.., sep="~~~~"), color=Year2))#+
  geom_quantile(quantiles = 0.5, aes(group=factor(Year),
                                     colour=factor(Year)))

##save plot as png for insert to word doc
ggsave(filename = paste0("KC_ic_overtime_CI_test", each, ".png"),
       plot = g1,
       width = 6, height = 3, units = "in")
# }

#############################
# setwd("G:/GreenWQA/Biota/BenthosProjects_KC/trends analysis/Data_for_Trends_FWS_manuscript")
# Trends<-read.csv("CSV/Site_Trends_coarse_10162020.csv", header=T)
# bibi.lr<-Trends
# bibi.lr<-ddply(bibi.lr, .(Site.Code), mutate, Mean_Overall.Score=mean(Overall.Score))
# bibi.lr<-ddply(bibi.lr, .(Site.Code), mutate, SD_Overall.Score=sd(Overall.Score))
# want<-ddply(bibi.lr, .(Site.Code),summarise, max(Year))
# bibi.lr<-left_join(bibi.lr, want, by=c("Site.Code"))
# bibi.lr<-subset(bibi.lr, Year==`max(Year)`)
# 
# lulc<-read.csv("LandUse.csv", header = T)
# lulc<-subset(lulc, lulc$Name!="09SOO1209")
# lulc<-merge(bibi.lr, lulc, by.x='Site.Code', by.y='Name')
# lulc$change_imp<-lulc$imp16_per-lulc$imp01_per
# 
# 
# LULC_plot<-ggplot(lulc, aes(x=change_imp, y=MKSlope_Overall.Score))+
#   geom_point(aes(shape=MKtrend_Overall.Score, color=MKtrend_Overall.Score)) +
#   scale_shape_manual(values = c(16,17)) +
#   scale_color_manual(values = c('blue','green')) +
#   geom_line(y=0) +
#   xlab("Change in % Impervious Cover") +
#   ylab("Slope Estimate") +
#   labs(shape="Sig. Trend", color="Sig. Trend")
# 
# ggsave("LULC_plot_Change_Slope.png", plot=LULC_plot, width=6, height=3, units="in")

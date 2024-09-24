library(tidyverse)
library(reshape2)
library(dplyr)
library(lme4)
library(lmerTest)
# library(MuMIn)
library(ggpmisc)
library(grid)
library(gridExtra)
library(Rmisc)
library(plyr)
# library(wlRd)
library(stringr)
# library(Kendall)
library(knitr)
library(plotly)
library(labeling)
library(rkt)
library(EnvStats)

setwd(here::here())
setwd("./outputs")
Trends<-read.csv("Site_Trends_coarse_LR_03092023_2002-2021_LRexclude.csv", header=T)
scores<-read.csv("B-IBI_results_03082023_2002-2021_NoUnique_subsamp_450_LR_exclude.csv")
summ<-read.csv("score_RKTtrend_summarized_by_site_03092023_2002-2021_LRexclude.csv")
# bibi.lr<-Trends
# bibi.lr<-ddply(bibi.lr, .(Site.Code), mutate, Mean_Overall.Score=mean(Overall.Score))
# bibi.lr<-ddply(bibi.lr, .(Site.Code), mutate, SD_Overall.Score=sd(Overall.Score))
# want<-ddply(bibi.lr, .(Site.Code),summarise, max(Year))
# bibi.lr<-left_join(bibi.lr, want, by=c("Site.Code"))
# bibi.lr<-subset(bibi.lr, Year==`max(Year)`)


# lulc<-read.csv("NLCD_basin.csv", header = T)

lulc<-read.csv("NLCD_LULC_2001-2019.csv", header = T)


setwd(here::here())
setwd("./inputs/NLCD 2001to2019 183Basins 181Buffers")
## commented out the following line and replaced with similar line 9/24/24 for Freswater Science Manuscript submission
# imp<-openxlsx::read.xlsx("Stats_PctDev_Basins_2001to2019.xlsx", sheet = 1, rows = c(7:190))
imp<-openxlsx::read.xlsx("Copy of Stats_PctDev_Basins_2001to2021.xlsx", sheet = 1, rows = c(7:190))
lulc<-subset(lulc, lulc$Name!="09SOO1209")
lulc<-merge(summ, lulc, by.x='Site.Code', by.y='Name')
imp<-subset(imp, imp$Name!="09SOO1209")
lulc<-merge(lulc, imp, by.x='Site.Code', by.y='Name')
lulc$change_imp<-lulc$PctDev_Basins_2021-lulc$PctDev_Basins_2001
lulc$change_urb<-lulc$per_Urban_2019 -lulc$per_Urban_2001

summary(lulc$change_imp)
summary(lulc$Change_per_Urban)

setwd(here::here())
setwd("./outputs")
## commented out the following three lines and replaced with similar lines 9/24/24 for Freswater Science Manuscript submission
# imp2<-read.csv("NLCD_basin_impervious.csv")
# imp_mean<-ddply(imp2, .(Name), summarize, meanImp=mean(PctDev_Basins_))
# lulc<-merge(lulc, imp_mean, by.x='Site.Code', by.y='Name')
imp2<-reshape2::melt(imp, id.vars=c("Name"), variable.name="PctDev_Basins_")
imp_mean<-ddply(imp2, .(Name), summarize, meanImp=mean(value))
lulc<-merge(lulc, imp_mean, by.x='Site.Code', by.y='Name')

# lulc$change_imp<-lulc$imp16_per-lulc$imp01_per
names(lulc)

LULC_plot<-ggplot(lulc, aes(x=meanImp, y=mean_Overall.Score))+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
  stat_smooth(inherit.aes=FALSE,aes(x=meanImp, y=mean_Overall.Score),method = "lm", formula = y ~ x)+
  stat_poly_eq(inherit.aes=FALSE, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 8, color = "black", aes(x=meanImp, y=mean_Overall.Score, label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)+
  # geom_errorbar(aes(ymin=Mean_Overall.Score-SD_Overall.Score, ymax=Mean_Overall.Score+SD_Overall.Score), width=.7, position = )+
  geom_point(size=4)+
  # scale_shape_manual(values=c(1,24), breaks=c("none","positive"), labels=c("none", "increasing"))+
  # scale_fill_manual(values=c("black", "green"),breaks=c("none","positive"), labels=c("none", "increasing"))+
  labs(x="% impervious cover in stream basin (mean over time)", y="B-IBI score (mean within site)")+ #fill="Mann-Kendall trend", shape="Mann-Kendall trend"
  ggtitle("B-IBI scores vs Impervious land cover")+
  theme(text=element_text(size=20), plot.title=element_text(hjust=.5, size=24, face="bold"))+
  coord_fixed(ratio=1/2)+
  scale_y_continuous(breaks=seq(0,100,20))
ggsave("Urban_wedge_plot_IC.png", plot=LULC_plot, width=16, height=10)



LULC_plot<-ggplot(lulc, aes(x=meanImp, y=mean_Overall.Score))+ #fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score
  stat_smooth(inherit.aes=FALSE,aes(x=meanImp, y=mean_Overall.Score),method = "lm", formula = y ~ x, color="black")+
  stat_poly_eq(inherit.aes=FALSE, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", color = "black", aes(x=meanImp, y=mean_Overall.Score, label = paste(..eq.label.., ..adj.rr.label.., sep = "*\", \"*")), parse = TRUE)+
  geom_point()+
  labs(x="Impervious cover (%)", y="Mean B-IBI score")+
  theme_bw()+
  theme(text=element_text(family="sans"), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(colour="black"))+
  coord_fixed(ratio=1/2)+
  scale_y_continuous(breaks=seq(0,100,20))
ggsave("Urban_wedge_plot_IC_FWS.tiff", plot=LULC_plot, width=8.4, height=6, units="cm", dpi=600)


LULC_plot<-ggplot(lulc, aes(x=meanImp, y=mean_Overall.Score, fill=MK_trend, shape=MK_trend))+ #
  stat_smooth(inherit.aes=FALSE,aes(x=meanImp, y=mean_Overall.Score),method = "lm", formula = y ~ x)+
  stat_poly_eq(inherit.aes=FALSE, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", size = 8, color = "black", aes(x=meanImp, y=mean_Overall.Score, label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE)+
  # geom_errorbar(aes(ymin=Mean_Overall.Score-SD_Overall.Score, ymax=Mean_Overall.Score+SD_Overall.Score), width=.7, position = )+
  geom_point(size=4)+
  scale_shape_manual(values=c(21,24, 25), breaks=c("none","positive", "negative"), labels=c("non-significant", "increasing", "decreasing"))+
  scale_fill_manual(values=c("gray", "turquoise", "red"),breaks=c("none","positive", "negative"), labels=c("non-significant", "increasing", "decreasing"))+
  labs(x="% impervious cover in stream basin (mean over time)", y="B-IBI score (mean within site)",fill="Mann-Kendall trend", shape="Mann-Kendall trend")+ #
  ggtitle("B-IBI scores vs Impervious land cover")+
  theme(text=element_text(size=20), plot.title=element_text(hjust=.5, size=24, face="bold"))+
  coord_fixed(ratio=1/2)+
  scale_y_continuous(breaks=seq(0,100,20))
ggsave("Urban_wedge_plot_trends_IC.png", plot=LULC_plot, width=16, height=10)

LULC_plot<-ggplot(lulc, aes(x=meanImp, y=mean_Overall.Score, fill=MK_trend, shape=MK_trend))+ #
  stat_smooth(inherit.aes=FALSE,aes(x=meanImp, y=mean_Overall.Score),method = "lm", formula = y ~ x, color="black")+
  stat_poly_eq(inherit.aes=FALSE, formula = y ~ x, rr.digits = 2, coef.digits = 3, label.x=50,position="identity", color = "black", aes(x=meanImp, y=mean_Overall.Score, label = paste(..eq.label.., ..adj.rr.label.., sep = "*\", \"*")), parse = TRUE)+
  geom_point()+
  scale_shape_manual(values=c(21,24, 25), breaks=c("none","positive", "negative"), labels=c("non-significant", "increasing", "decreasing"))+
  scale_fill_manual(values=c("white", "darkgray", "black"),breaks=c("none","positive", "negative"), labels=c("non-significant", "increasing", "decreasing"))+
  labs(x="Impervious cover (%)", y="Mean B-IBI score",fill="Mann-Kendall trend", shape="Mann-Kendall trend")+ #
  theme_bw()+
  theme(text=element_text(family="sans"), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(colour="black"))+
  coord_fixed(ratio=1/2)+
  scale_y_continuous(breaks=seq(0,100,20))+
  guides(fill = "none", shape="none") 
ggsave("Urban_wedge_plot_trends_IC_FWS.tiff", plot=LULC_plot, width=8.4, height=6, units="cm", dpi=600)


# LULC_plot<-ggplot(lulc, aes(x=change_imp, y=mean_Overall.Score, fill=MK_trend, shape=MK_trend))+
#   # geom_errorbar(aes(ymin=Mean_Overall.Score-SD_Overall.Score, ymax=Mean_Overall.Score+SD_Overall.Score), width=.7, position = )+
#   geom_point(size=4)+
#   scale_shape_manual(values=c(19,24, 25), breaks=c("none","positive", "negative"), labels=c("non-significant", "increasing", "decreasing"))+
#   scale_fill_manual(values=c("black", "turquoise", "red"),breaks=c("none","positive", "negative"), labels=c("non-significant", "increasing", "decreasing"))+
#   labs(x="Change in  % Impervious Cover", y="B-IBI Score (mean within Site)", fill="Mann-Kendall trend", shape="Mann-Kendall trend")+ggtitle("B-BIBI scores vs Change impervious land cover")+
#   theme(text=element_text(size=20), plot.title=element_text(hjust=.5, size=24, face="bold"))
# ggsave("LULC_plot_Change_IC.png", plot=LULC_plot, width=16, height=10)
# 
# LULC_plot<-ggplot(lulc, aes(x=change_imp, y=Avg_MKSlope_Overall.Score, fill=MK_trend, shape=MK_trend))+
#   #geom_errorbar(aes(ymin=mean_Overall.Score-SD_overallscore, ymax=mean_Overall.Score+SD_overallscore), width=.7, position = )+
#   geom_point(size=4)+
#   # geom_point(lulc[lulc$Avg_MKSlope_Overall.Score>0,], inherit.aes=T, mapping=aes(x=Change_per_Urban, y=Avg_MKSlope_Overall.Score), color="blue", size=4)+
#   # geom_point(lulc[lulc$Avg_MKSlope_Overall.Score<0,], inherit.aes=T, mapping=aes(x=Change_per_Urban, y=Avg_MKSlope_Overall.Score), color="red", size=4)+
#   geom_line(y=0)+
#   scale_shape_manual(values=c(19,24, 25), breaks=c("none","positive", "negative"), labels=c("none", "increasing", "decreasing"))+
#   scale_fill_manual(values=c("black", "green", "red"),breaks=c("none","positive", "negative"), labels=c("none", "increasing", "decreasing"))+
#   
#   labs(x="Change in % Impervious land cover from 2001-2019", y="Estimated Slope", fill="Mann-Kendall trend", shape="Mann-Kendall trend")+ggtitle("Mann-Kendall Estimated Slope vs Change in % Impervious land cover")+
#   theme(legend.position="none",text=element_text(size=20), plot.title=element_text(hjust=.1, size=24, face="bold"))+
#   ylim(-3,3)
# ggsave("LULC_plot_Change_Slope_IC.png", plot=LULC_plot, width=16, height=10)


###########################

LULC_plot<-ggplot(lulc, aes(x=change_imp, y=Avg_MKSlope_Overall.Score))+
  geom_point(aes(shape=MK_trend,  fill=MK_trend)) +
  scale_shape_manual(values=c(21,24, 25), breaks=c("none","positive", "negative"), labels=c("non-significant", "increasing", "decreasing"))+
  scale_fill_manual(values=c("gray", "turquoise", "red"),breaks=c("none","positive", "negative"), labels=c("non-significant", "increasing", "decreasing"))+
  geom_line(y=0) +
  xlab("Increase in % Impervious Cover") +
  ylab("Slope Estimate") +
  labs(shape="Trend Direction", fill="Trend Direction")

ggsave("LULC_plot_Change_Slope.png", plot=LULC_plot, width=6, height=3, units="in")


LULC_plot<-ggplot(lulc, aes(x=change_imp, y=Avg_MKSlope_Overall.Score))+
  geom_point(aes(shape=MK_trend,  fill=MK_trend)) +
  stat_smooth(method = "lm", formula = y ~ x, color="black")+
  stat_poly_eq(formula = y ~ x, rr.digits = 2, coef.digits = 3,label.x=5, label.y=-1, position="identity", color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "*\", \"*")), parse = TRUE)+
  scale_shape_manual(values=c(21,24, 25), breaks=c("none","positive", "negative"), labels=c("non-significant", "increasing", "decreasing"))+
  scale_fill_manual(values=c("white", "darkgray", "black"),breaks=c("none","positive", "negative"), labels=c("non-significant", "increasing", "decreasing"))+
  geom_line(y=0, lty="dashed") +
  theme_bw()+
  theme(text=element_text(family="sans"), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(colour="black"))+
  xlab("Increase in % impervious cover") +
  ylab("Slope Estimate") +
  labs(shape="Trend Direction", fill="Trend Direction")+
  guides(fill = "none", shape="none") 

ggsave("LULC_plot_Change_Slope_FWS_fixed.tiff", plot=LULC_plot, width=8.4, height=6, units="cm", dpi=600)

################################


# 
# LULC_plot<-ggplot(lulc, aes(x=Change_per_Forest, y=Mean_Overall.Score, fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score))+
#   # geom_errorbar(aes(ymin=Mean_Overall.Score-SD_Overall.Score, ymax=Mean_Overall.Score+SD_Overall.Score), width=.7, position = )+
#   geom_point(size=4)+
#   scale_shape_manual(values=c(19,24))+
#   scale_fill_manual(values=c("black", "green"))+
#   labs(x="Change in  % Forest Cover", y="B-IBI Score (mean within Site)", fill="Mann-Kendall trend", shape="Mann-Kendall trend")+ggtitle("B-BIBI scores vs Change Forest land cover")+
#   theme(text=element_text(size=20), plot.title=element_text(hjust=.5, size=24, face="bold"))
# ggsave("LULC_plot_Change_Forest.png", plot=LULC_plot, width=10, height=10)
# 
# LULC_plot<-ggplot(lulc, aes(x=Change_per_Forest, y=MKSlope_Overall.Score, fill=MKtrend_Overall.Score, shape=MKtrend_Overall.Score))+
#   #geom_errorbar(aes(ymin=mean_Overall.Score-SD_overallscore, ymax=mean_Overall.Score+SD_overallscore), width=.7, position = )+
#   geom_point(size=4)+
#   scale_shape_manual(values=c(19,24), breaks=c("none","positive"), labels=c("none", "increasing"))+
#   scale_fill_manual(values=c("black", "green"),breaks=c("none","positive"), labels=c("none", "increasing"))+
#   labs(x="Change in % Forest land cover from 2001-2016", y="Mann-Kendall Slope", fill="Mann-Kendall trend", shape="Mann-Kendall trend")+ggtitle("Mann-Kendall Slope vs Change in % Forest land cover")+
#   theme(text=element_text(size=20), plot.title=element_text(hjust=.1, size=24, face="bold"))+
#   ylim(-3,3)
# ggsave("LULC_plot_Change_Slope_Forest.png", plot=LULC_plot, width=10, height=10)
# 
# lulc<-subset(lulc, lulc$Site.Code!="09COV1864")
# ddply(lulc, .(MKtrend_Overall.Score), summarise, mean.per.urb= mean(per_Urban_x2016), med.per.urba=median(per_Urban_x2016), mean.per.change=mean(Change_per_Urban), med.per.change=median(Change_per_Urban))
# 
# summarise(lulc, mean.per.urb= mean(per_Urban_x2016), med.per.urba=median(per_Urban_x2016), mean.per.change=mean(Change_per_Urban), med.per.change=median(Change_per_Urban))

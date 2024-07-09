library(stringr)
library(dplyr)
library(plyr)
library(ggplot2)


setwd(here::here())
setwd("./outputs")

data<-read.csv("dpac_out_03092023_LRexclude.csv")
counts<-read.csv("Collapsed_Coarse_Taxa_density_LRexclude.csv")
rawcounts<-read.csv("Collapsed_Coarse_Taxa_LR_exclude.csv")

setwd(here::here())
setwd("./inputs")
raw<-read.csv("Raw_taxa_data_2002-2022.csv")
# replicates_check<-read.csv("replicate samples.csv")

# summbugs<-ddply(raw, .(Sample.ID, Site.Code, Visit.Date), summarize, SumBugs=sum(Quantity))

data<-subset(data, Quantity_new!=0)### remove ambiguous taxa that have been re-assigned
data[which(duplicated(data[,c(1,2)])),]
data$OTU_COARSE_Unique<-str_replace(data$OTU_COARSE_Unique, "_UNIQUE", "") ##Remove "_UNIQUE"
data[which(duplicated(data[,c(1,2)])),]
excludevisits<-unique(subset(counts, Project=="Regulatory Effectiveness")$Sample.Code) ## remove reg effectiveness samples, since they use three replicates
data<-subset(data, !ID %in% excludevisits)
replicates<-unique(subset(raw, QC.Replicate.Of.Sample.Code!="")$Sample.Code)
excludereplicates<-unique(subset(counts, Sample.Code %in% replicates)$Sample.Code) ### remove all replicates and QC samples
data<-subset(data, !ID %in% excludereplicates)
data[which(duplicated(data[,c(1,2)])),]

keep<-ddply(rawcounts, .(Sample.Code, Visit.ID), summarize, sumorgs=sum(Quantity_OTU))
keep<-subset(keep, sumorgs>=450)
data<-subset(data, ID %in% keep$Sample.Code)



##clean up species data##
lookup<-subset(counts, select=c(Site.Code, Sample.Code,Visit.ID, Project, Stream.or.River, Subbasin, WRIA.Number, Visit.Date, Year, Basin))
lookup<-unique(lookup)
data<-reshape2::dcast(data, ID~OTU_COARSE_Unique, value.var = "Quantity_new")
data<-merge(data, lookup, by.x="ID", by.y="Sample.Code")
data$month <- format(as.Date(data$Visit.Date, '%Y-%m-%d'), "%m") # add month column
data$year <- format(as.Date(data$Visit.Date, '%Y-%m-%d'), "%Y") # add year column
data <- droplevels(data[data$month %in% c('07', '08', '09', '10'),]) # exclude samples collected outside of July - Oct
data <- droplevels(data[data$year > 2001,]) # subset to sites since 2002
# data<-subset(data, year<=2020) ##subset thru 2020
fixDUW<-unique(counts[counts$Sample.Code=="09DUW0277/05",]$Visit.ID)
data[data$ID==fixDUW,"Site.Code"] <-"09DUW0277" ##Correct SiteID
data[data$ID==fixDUW,"Stream.or.River"] <-"Riverton Creek (003D)" ##Correct SiteID
data[data$ID==fixDUW,"Subbasin"] <-"Duwamish River Subbasin" ##Correct SiteID
data[data$ID==fixDUW,"Basin"] <-"Duwamish - Green River Basin" ##Correct SiteID
data[data$ID==fixDUW,"WRIA.Number"] <-9 ##Correct SiteID
library(plyr)
data <- ddply(data, 'Site.Code', mutate, por = length(unique(year))) # add on period of record col
data<-droplevels(data[data$por > 9,])# look only at sites with >9 years of data

# summbugs<-summbugs[summbugs$Site.Code %in% data$Site.Code,]
# summbugs$year <- format(as.Date(summbugs$Visit.Date, '%Y-%m-%d'), "%Y")
# ggplot(summbugs, aes(x=year, y=SumBugs))+geom_boxplot()
# library(rkt)
# l=unique(c(as.character(summbugs$Site.Code)))
# summbugs$Site.Number<-as.numeric(factor(summbugs$Site.Code, levels=l))
# with(summbugs, rkt(date = as.numeric(year), y = SumBugs, block = Site.Number, correct = FALSE, rep="a"))


data[is.na(data)] <- 0
colnames(data)<-str_replace_all(colnames(data), " ", ".")
colnames(data)<-str_replace_all(colnames(data), "/", "_")
colnames(data)<-str_replace_all(colnames(data), "\\(", "_.")
colnames(data)<-str_replace_all(colnames(data), "\\)", "._")
colnames(data)<-str_replace_all(colnames(data), "#", "num")
data[which(duplicated(subset(data, select=c(Site.Code, Visit.Date, ID)))),]
data[duplicated(subset(data, select=c(Stream.or.River, ID, year))),]
data[duplicated(subset(data, select=c(Stream.or.River, ID, year)), fromLast=TRUE),]

names(data)
# data$Sample.Code <- paste(data$Site.Code, format(as.Date(data$Visit.Date, '%Y-%m-%d'), "%Y"), sep = "_") # add sample identifier
data$Sample.Code <- data$ID # add sample identifier

###2/9/2021 update fix some taxa that should be rolled up###
data$Leuctridae<-rowSums(data[, c("Moselia", "Despaxia.augusta", "Leuctridae")], na.rm=T)
data$Clinocerinae<-rowSums(data[, c("Wiedemannia", "Trichoclinocera", "Roederiodes", "Empididae.Genus.B", "Clinocera")], na.rm=T)
data$Hemerodromiinae<-rowSums(data[, c("Hemerodromiinae", "Hemerodromia", "Chelifera_Metachela", "Neoplasta")], na.rm=T)
data$Sphaeriidae<-rowSums(data[,c("Pisidium","Sphaerium","Sphaeriidae")], na.rm=T)
data$Glossiphoniidae<-rowSums(data[,c("Glossiphoniidae","Glossiphonia.elegans","Helobdella.stagnalis")], na.rm=T)
data$Hydrophilidae<-rowSums(data[,c("Hydrophilidae","Ametor","Anacaena", "Laccobius")], na.rm=T) ###,"Laccobius"
data$Gomphidae<-rowSums(data[,c("Gomphidae","Octogomphus.specularis")], na.rm=T)
data$Kogotus.Rickera.Cultus<-rowSums(data[,c("Kogotus_Rickera","Perlodidae","Cultus")], na.rm=T)
data$Hydroptila<-rowSums(data[,c("Hydroptilidae","Hydroptila")], na.rm=T)
data$Uenoidae..s.l.<-data$Uenoidae.s.l.##"Thremmatidae"
data$Trepaxonemata<-rowSums(data[,c("Trepaxonemata","Polycelis")], na.rm=T)
data$Optioservus<-rowSums(data[,c("Optioservus","Elmidae")], na.rm=T) ##Rotate through adding Elmidae to these taxa to see if it effects trends: Cleptelmis addenda, Heterlimnius.corpulentus, Lara, Narpus, Optioservus, Zaitzevia



data<-subset(data, select=-c(Sphaerium, Pisidium, Glossiphonia.elegans,Helobdella.stagnalis,
                             Ametor,Anacaena,Octogomphus.specularis,Kogotus_Rickera,Perlodidae,
                             Cultus,Hydroptilidae,Uenoidae.s.l.,Polycelis,Elmidae, Moselia, Laccobius, Despaxia.augusta, 
                             Wiedemannia, Trichoclinocera, Roederiodes, Empididae.Genus.B, Clinocera, Hemerodromia, Chelifera_Metachela, Neoplasta))#Laccobius, Thremmatidae

names(data)
names(data[c(1:238, 252:254, 239:251)])
data<-data[c(1:238, 252:254, 239:251)]### make sure all the taxa count data is before the metadata


KC_taxa<-data

# KC_taxa[5:270]<-lapply(KC_taxa[5:270], scale)
# KC_taxa$Year<-as.numeric(format(as.Date(KC_taxa$CollectionDate, '%m/%d/%Y'), "%Y"))
ggplot(KC_taxa, aes(x=Year, y=Pteronarcys, group=Year))+geom_violin()
ggplot(KC_taxa, aes(x=Year, y=Simuliidae, group=Year))+geom_violin()+geom_point()
l=unique(c(as.character(KC_taxa$Site.Code)))
KC_taxa$Site.Number<-as.numeric(factor(KC_taxa$Site.Code, levels=l))

library(reshape2)
KC_taxa_long<-melt(KC_taxa, id = c("Site.Code", "ID","Visit.ID", "Visit.Date", "Site.Number", "Project", "Sample.Code", "Stream.or.River", "Basin", "Year", "Subbasin", "WRIA.Number", "month", "year", "por"), value.name="Density")
unique(KC_taxa_long$variable)
KC_taxa_long$Density<-as.numeric(paste0(KC_taxa_long$Density))

TotDensities<-ddply(KC_taxa_long, .(Site.Code, ID,Visit.ID, Visit.Date, Site.Number, Project, Sample.Code, Stream.or.River, Basin, Year, Subbasin, WRIA.Number, month, year, por), summarize, TotalDensity=sum(Density))
l=unique(c(as.character(TotDensities$Site.Code)))
TotDensities$Site.Number<-as.numeric(factor(TotDensities$Site.Code, levels=l))
library(rkt)
with(TotDensities, rkt(date = as.numeric(Year), y = TotalDensity, block = Site.Number, correct = FALSE, rep="a"))
# ggplot(envdata, aes(y=per_Forest_x_RB, x=Year, group=Site.Code, color=Site.Code))+geom_line()
ggplot(TotDensities, aes(x=Year, y=TotalDensity, group=Year))+geom_boxplot()
TotDensities_wideyr<-reshape2::dcast(TotDensities, Site.Code~Year, value.var = "TotalDensity")
TotDensities_wideyr$perDiff<-TotDensities_wideyr$`2021`/TotDensities_wideyr$`2002`
1-mean(TotDensities_wideyr$perDiff, na.rm=T)


meanorgs<-ddply(KC_taxa_long, .(variable), summarize, meanall=median(Density))

setwd(here::here())
setwd("./outputs")
write.csv(meanorgs[order(meanorgs$meanall,decreasing=T), ], "median_densities.csv")


# KC_taxa_long<-subset(KC_taxa_long, variable!="X")
# KC_taxa_long$Year<-as.numeric(format(as.Date(KC_taxa_long$Visit.Date, '%m/%d/%Y'), "%Y"))
# KC_taxa_long<-ddply(KC_taxa_long, .(variable), mutate, scaled=scale(Density))
ggplot(subset(KC_taxa_long, (variable=="Baetis.rhodani.group"|variable=="Chironomidae"|variable=="Oligochaeta"|variable=="Simuliidae"|variable=="Zapada.cinctipes")), aes(x=Year, y=Density, color=variable))+geom_smooth(se=F)+labs(color="Taxo")
# scale_x_continuous(breaks = seq(min(KC_taxa_long$Year), max(KC_taxa_long$Year), by = 1))+
# facet_wrap_paginate(~variable, scales="free", ncol=8, nrow=9,page=1)#+
# facet_wrap(~variable, scales="free")
# facet_wrap(~STAID)+
# geom_vline(aes(xintercept=2013), color="black")
ggsave("trending_tolerant_taxa_densities.png", width=6, height=4)

taxa_trends_regional<-read.csv("regionwide_abundance_trends_RKT_density_03132023_2002-2021.csv")
KC_taxa_long2<-merge(KC_taxa_long, taxa_trends_regional, by.x = "variable", by.y="taxon", all.x=T)
unique(subset(KC_taxa_long, !variable %in% KC_taxa_long2$variable)$variable)
freq_taxa_trends_regional<-read.csv("freq_of_occurrence_density_10122023_2002-2021.csv")
KC_taxa_long3<-merge(KC_taxa_long2, freq_taxa_trends_regional, by.x = "variable", by.y="taxon", all.x=T)
unique(subset(KC_taxa_long, !variable %in% KC_taxa_long3$variable)$variable)
pres_taxa_trends_regional<-read.csv("region_pres-abs_density_03132023_2002-2021.csv")
KC_taxa_long4<-merge(KC_taxa_long3, pres_taxa_trends_regional, by.x = "variable", by.y="taxon", all.x=T)
unique(subset(KC_taxa_long, !variable %in% KC_taxa_long4$variable)$variable)
# 
# subset(KC_taxa_long4, RKTtrend="positive"|mk.trend=="positive"|trend=="positive")

# ggplot(subset(KC_taxa_long, variable=="Acari"), aes(x=variable, y=scaled, group=variable))+geom_point(position="jitter")
# ggplot(KC_taxa_long, aes(x=variable, y=scaled, group=variable))+geom_boxplot()

library(ggforce)

excludetaxa<-c("Heptageniidae",
               "Margaritifera.falcata",
               "Matriella.teresa",
               "Corixidae",
               "Nemouridae",
               "Glossosomatidae",
               "Philopotamidae",
               "Psychomyia",
               "Uenoidae..s.l.",
               "Erioptera", 
               "Muscidae",
               "Hirudinea",
               "Crangonyctidae",
               "Holorusia.hespera",
               "Tricorythodes",
               "Coenagrionidae",
               "Amphipoda",
               "Allocosmoecus"
               
)

ggplot(subset(KC_taxa_long3, RKTtrend!="none" & !variable %in% excludetaxa ), aes(x=Year, y=Density))+#geom_smooth(se=F)+
  geom_point( inherit.aes=F, data=subset(KC_taxa_long3, RKTtrend!="none" & !variable %in% excludetaxa), mapping=aes(x=Year, y=Density))+
  theme(legend.position = "none")+
  # scale_x_continuous(breaks = seq(min(KC_taxa_long$Year), max(KC_taxa_long$Year), by = 1))+
  # facet_wrap_paginate(~variable, scales="free", ncol=8, nrow=9,page=1)#+

facet_wrap(~variable, scales="free")+labs(y="Density (#/foot^2)")+theme(strip.text.x = element_text(size = 6))
  # facet_wrap(~STAID)+
  # geom_vline(aes(xintercept=2013), color="black")
ggsave("trending_taxa_densities.png", width=16, height=9)

# ggplot(subset(KC_taxa_long4, RKTtrend!="none" & !variable %in% excludetaxa & Density!= 0), aes(x=Year, y=Density))+geom_point()

ggplot(KC_taxa_long, aes(x=Year, y=Density, color=variable))+geom_smooth(se=F)+theme(legend.position = "none")+
  # scale_x_continuous(breaks = seq(min(KC_taxa_long$Year), max(KC_taxa_long$Year), by = 1))+
  facet_wrap_paginate(~variable, scales="free", ncol=8, nrow=9,page=1)

ggplot(KC_taxa_long, aes(x=Year, y=Density, color=variable))+geom_smooth(se=F)+theme(legend.position = "none")+
  # scale_x_continuous(breaks = seq(min(KC_taxa_long$Year), max(KC_taxa_long$Year), by = 1))+
  facet_wrap_paginate(~variable, scales="free", ncol=8, nrow=9,page=2)
ggplot(KC_taxa_long, aes(x=Year, y=Density, color=variable))+geom_smooth(se=F)+theme(legend.position = "none")+geom_point()+
  # scale_x_continuous(breaks = seq(min(KC_taxa_long$Year), max(KC_taxa_long$Year), by = 1))+
  facet_wrap_paginate(~variable, scales="free", ncol=8, nrow=9,page=3)
ggplot(KC_taxa_long, aes(x=Year, y=Density, color=variable))+geom_smooth(se=F)+theme(legend.position = "none")+geom_point()+
  # scale_x_continuous(breaks = seq(min(KC_taxa_long$Year), max(KC_taxa_long$Year), by = 1))+
  facet_wrap_paginate(~variable, scales="free", ncol=8, nrow=9,page=4)


ggplot(subset(KC_taxa_long, (variable=="Baetis.rhodani.group"|variable=="Chironomidae"|variable=="Oligochaeta"|variable=="Simuliidae"|variable=="Zapada.cinctipes")), aes(x=Year, y=Density, color=variable))+geom_smooth(se=F)+labs(color="Taxon")
  # scale_x_continuous(breaks = seq(min(KC_taxa_long$Year), max(KC_taxa_long$Year), by = 1))+
  # facet_wrap_paginate(~variable, scales="free", ncol=8, nrow=9,page=1)#+
  # facet_wrap(~variable, scales="free")
# facet_wrap(~STAID)+
# geom_vline(aes(xintercept=2013), color="black")
ggsave("trending_tolerant_taxa_densities.png", width=6, height=4)

setwd(here::here())
setwd("./inputs")
library(openxlsx)
ENSO<-openxlsx::read.xlsx("ENSO_MEI.xlsx")
ENSO2<-reshape2::melt(ENSO, id.vars=c("YEAR"))
levels(ENSO2$variable)
ENSO2$month<-as.numeric(factor(ENSO2$variable))
ENSO2$date<-paste0(ENSO2$month, "-", ENSO2$YEAR)
library(lubridate)
ENSO2$date<-my(ENSO2$date)
library(ggplot2)
as.Date(ENSO2$date, "%m-%Y")
ggplot(ENSO2, aes(x=date, y=value))+geom_point()
ENSO3<-subset(ENSO2, date>"2002-01-01")
ggplot(ENSO3, aes(x=date, y=value))+geom_point()
KC_taxa_long$Visit.Date<-as.Date(KC_taxa_long$Visit.Date, "%Y-%m-%d")
names(ENSO3)[2]<-"bimonth"
names(KC_taxa_long)

ggplot(subset(KC_taxa_long, (variable=="Baetis.rhodani.group"|variable=="Chironomidae"|variable=="Oligochaeta"|variable=="Simuliidae"|variable=="Zapada.cinctipes")))+geom_smooth(aes(x=Visit.Date, y=Density, color=variable),se=F)+labs(color="Taxon")+
  geom_point(data=ENSO3, mapping=aes(x=date, y=value*20))
ggsave("trending_tolerant_taxa_densities_ENSO.png", width=6, height=4)




##########
### convert to frequency data##
source('C:/Users/esosik/OneDrive - King County/Documents/biostats.R', encoding = 'UTF-8')
data_freq<-KC_taxa
rownames(data_freq)<-data_freq$ID
names(data_freq)
# names(data_freq[5:(ncol(data_freq)-3)])
data_freq<-data_freq[2:(ncol(data_freq)-14)]
data_freq[data_freq!=0]<-1
data_freq<-drop.var(data_freq, var='',min.fo=1)
data_freq$ID<-rownames(data_freq)
data_freq<-merge(KC_taxa[c(1, (ncol(KC_taxa)-13):ncol(KC_taxa))], data_freq, by="ID")
# data_freq$Year<-as.numeric(format(as.Date(data_freq$CollectionDate, '%m/%d/%Y'), "%Y"))

data_freq3<-data_freq %>% group_by(Year) %>% summarise_if(is.numeric, sum)
ggplot(data_freq3, aes(x=Year, y=Pteronarcys))+geom_point()+geom_smooth(se=F)+labs(y="Number of sites containing Pteronarcys")
ggplot(data_freq3, aes(x=Year, y=Yoraperla))+geom_point()+geom_smooth(se=F)+labs(y="Number of sites containing Yoraperla")
nsamp<-ddply(KC_taxa, .(year), summarize, nsamp=length(unique(ID)))
data_freq3<-merge(data_freq3, nsamp, by.x="Year", by.y="year")
detach("package:plyr", unload = TRUE)
data_freq3<-data_freq3 %>% mutate(across(Acari:Uenoidae..s.l., function(x) x/nsamp*100))
data_freq3<-subset(data_freq3, select=-nsamp)

data_freq3_long<-melt(data_freq3, id = c("Year"), value.name="FOO")
unique(data_freq3_long$variable)
data_freq3_long<-subset(data_freq3_long, variable!="Visit.ID"&variable!="WRIA.Number"&variable!="por"&variable!="Site.Number")

library(plyr)
meanorgs<-ddply(data_freq3_long, .(variable), summarize, meanall=mean(FOO))
write.csv(meanorgs[order(meanorgs$meanall,decreasing=T), ], "mean_FOO.csv")

data_freq3_long2<-merge(data_freq3_long, freq_taxa_trends_regional, by.x = "variable", by.y="taxon", all.x=T)
unique(subset(data_freq3_long, !variable %in% data_freq3_long2$variable)$variable)




  
ggplot(subset(data_freq3_long2, mk.trend!="none"& !variable %in% excludetaxa), aes(x=Year, y=FOO))+geom_smooth(se=F)+
  geom_point()+
  theme(legend.position = "none")+
  # scale_x_continuous(breaks = seq(min(KC_taxa_long$Year), max(KC_taxa_long$Year), by = 1))+
  facet_wrap(~variable, scales="free")+labs(y="% of sites present")+theme(strip.text.x = element_text(size = 9))
  # facet_wrap_paginate(~variable, scales = "free", ncol=8, nrow=9, page=1)#+
  # geom_vline(aes(xintercept=2013), color="black")
ggsave("trending_taxa_spread_10132023.png", width=16, height=9)
ggplot(data_freq3_long, aes(x=Year, y=FOO, color=variable))+geom_smooth(se=F)+theme(legend.position = "none")+
  facet_wrap_paginate(~variable, scales = "free", ncol=8, nrow=9, page=2)
ggplot(data_freq3_long, aes(x=Year, y=FOO, color=variable))+geom_smooth(se=F)+theme(legend.position = "none")+
  facet_wrap_paginate(~variable, scales = "free", ncol=8, nrow=9, page=3)
ggplot(data_freq3_long, aes(x=Year, y=FOO, color=variable))+geom_smooth(se=F)+theme(legend.position = "none")+
  facet_wrap_paginate(~variable, scales = "free", ncol=8, nrow=9, page=4)


names(data_freq)
data_freq2<-melt(data_freq, id = c("Site.Code", "ID", "Visit.ID", "Project", "WRIA.Number","Visit.Date", "month", "year", "por", "Sample.Code","Site.Number","Stream.or.River", "Basin", "Subbasin", "Year"), value.name="Presence")
# data_freq2<-subset(data_freq2, variable!="X")
richness<-ddply(data_freq2, .(Year, Site.Code), summarize, richness=sum(Presence))

l=unique(c(as.character(richness$Site.Code)))
richness$Site.Number<-as.numeric(factor(richness$Site.Code, levels=l))
library(rkt)
with(richness, rkt(date = as.numeric(Year), y = richness, block = Site.Number, correct = FALSE, rep="a"))

ggplot(richness, aes(x=Year, y=richness, group=Year))+geom_boxplot()+theme(legend.position = "none")

ggplot(richness, aes(x=Year, y=richness, color=Site.Code))+geom_point()+geom_smooth(se=F)+theme(legend.position = "none")+
  facet_wrap(~Site.Code)+
  geom_vline(aes(xintercept=2011), color="black")

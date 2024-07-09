setwd(here::here())
setwd("./inputs")
# merged_OTU<-read.csv("Merged_data_OTUs.csv", header=T)
raw<-read.csv("Raw_taxa_data_2002-2022.csv")


missinggridinfo<-read.csv("missing grid info.csv")###missing data obtained from Sean. One ABR sample with count <500, assumed fully counted

setwd(here::here())
setwd("./outputs")
rawcounts<-read.csv("Collapsed_Coarse_Taxa_LR_exclude.csv")
##########Convert to Density####
# missinggrids<-raw[raw$Sample.Code %in% missinggridinfo$Sample.Code,c("Sample.Code","Surface.Area", "Sampling.Grid.Squares.Counted", "Total.Sampling.Grid.Squares")]
# missinggrids[,c("Sample.Code","Surface.Area", "Sampling.Grid.Squares.Counted", "Total.Sampling.Grid.Squares")]<-missinggridinfo[match(missinggrids$Sample.Code, missinggridinfo$Sample.Code),c("Sample.Code","Surface.Area", "Sampling.Grid.Squares.Counted", "Total.Sampling.Grid.Squares")]



raw[raw$Sample.Code %in% missinggridinfo$Sample.Code,c("Sample.Code","Surface.Area", "Sampling.Grid.Squares.Counted", "Total.Sampling.Grid.Squares")]<-missinggridinfo[match(raw[raw$Sample.Code %in% missinggridinfo$Sample.Code,"Sample.Code"], missinggridinfo$Sample.Code),c("Sample.Code","Surface.Area", "Sampling.Grid.Squares.Counted", "Total.Sampling.Grid.Squares")]
raw[raw$Sample.Code %in% missinggridinfo$Sample.Code,c("Sample.Code","Surface.Area", "Sampling.Grid.Squares.Counted", "Total.Sampling.Grid.Squares")]


prop_counted<-subset(raw, select=c(Sample.Code, Sampling.Grid.Squares.Counted, Total.Sampling.Grid.Squares, Surface.Area))
aggreg<-aggregate(Quantity~Sample.Code+Sampling.Grid.Squares.Counted+Total.Sampling.Grid.Squares+Surface.Area+Surface.Area.Units+Visit.Date, raw, FUN=sum)
# write.csv(aggreg, "sample_density.csv")
prop_counted$prop_counted<-prop_counted$Sampling.Grid.Squares.Counted/prop_counted$Total.Sampling.Grid.Squares
prop_counted<-unique(prop_counted)
library(dplyr)
counts<-left_join(rawcounts, prop_counted, by="Sample.Code")

counts$total_sample_count<-counts$Quantity_OTU/counts$prop_counted
subset(counts, select=c(total_sample_count, Quantity_OTU, prop_counted, Surface.Area))
counts$density<-counts$total_sample_count/counts$Surface.Area
subset(counts, select=c(total_sample_count, Quantity_OTU, prop_counted, Surface.Area, density))
sample_id<-unique(subset(counts, select=Sample.Code))
sample_id$Sample.ID<-seq(1, length(sample_id$Sample.Code), 1)
counts<-left_join(counts, sample_id, by="Sample.Code")
unique(counts[which(is.na(counts$density)),]$Sample.Code)
write.csv(counts, "Collapsed_Coarse_Taxa_density_LRexclude.csv")
counts<-read.csv("Collapsed_Coarse_Taxa_density_LRexclude.csv")
counts<-subset(counts, select=-X.1)
##Append _UNIQUE to unique coarse OTUs
counts$OTU_COARSE_Unique<-ifelse(counts$Unique_OTU, paste0(counts$OTU_COARSE, "_UNIQUE"), counts$OTU_COARSE)


##################DPAC Preparation####

names(counts)
counts2<-counts[, c(2, 44,3, 52,14,50,16)] ##get just minimum sample data needed: Visit.ID, Taxon Name, density, Unique, and Taxon Name _Unique
counts2[which(duplicated(counts2)),] #make sure no data is duplicated

cast<- reshape2::dcast(counts2, OTU_COARSE+OTU_COARSE_Unique+Unique_OTU~Sample.Code, sum, value.var="density") ##reshape the data to wide format for DPAC

##add on hierarchical info

hier<-counts[, c(3, 17:38)]
hier<-unique(hier)
names(hier)
hier<-hier[,c(1:16,18, 17,19, 21, 20, 22,23)] ##re-order hierarchy to be in correct cascading order
cast<-merge(cast, hier, by="OTU_COARSE") ##append hierarchies
names(cast)

cast<-cast[cast$OTU_COARSE_Unique!="NA_UNIQUE",]##These are Vashon sites with taxa that were not in the lookup table...need to figure out what to do with these guys
names(cast)

####Append _UNIQUE to hierarchies that match each unique coarse OTU, so that the code doesn't distribute uniquely identified taxa
### Previously I've done this step in excel using complex formula involving match function, but the loop below does the same thing. 
### BUT it will end in error if there's no match in the hierarchy for what is listed in the OTU_COARSE column.
### Errors should be investigated in OTU_collapse.R and the lookup table used in that code.
# cast[1,(ncol(cast)-21):ncol(cast)]##chec that these columns are the taxonomic hierarchy columns
for ( i in 1:nrow(cast)){
  if(cast[i,"Unique_OTU"]==T){
    idx<-match(cast$OTU_COARSE[i], cast[i,(ncol(cast)-21):ncol(cast)])## match OTU_COARSE to position in hierarchical columns
    cast[i,(ncol(cast)-21):ncol(cast)][,idx]<-paste0(cast[i,(ncol(cast)-21):ncol(cast)][,idx], "_UNIQUE") ##matching hierarchical position will get _UNIQUE added to it
    # print( cast[i,524:545][,idx])
  }
}

# cast[i,] ##if error in above loop, this will tell you where the error occurred

cast[cast==""]<-NA ##Any blank hierarchies convert to NA

comm=cast[, c(2,4:(ncol(cast)-22))] ##select columns OTU_COARSE_Unique, and all Visit.IDs
hier=cast[,c(2,(ncol(cast)-21):ncol(cast))] ##select columns OTU_COARSE_Unique and all hierarchical data
taxa.var="OTU_COARSE_Unique" ##identify column used for identification (OTU_COARSE_Unique)
out<-list(comm=comm, hier=hier, taxa.var=taxa.var)
class(out) <- "wide_class"

mod_dpac_s<-function (x, value.var.cols = NULL) 
{
  
  if (class(x) != "wide_class") 
    stop("Need an object of class 'wide_class'!")
  if (is.null(value.var.cols)) 
    stop("Must specify value.var!")
  
  comm <- x[["comm"]]
  hier <- x[["hier"]]
  taxa.var <- x[["taxa.var"]]
  temp<-data.frame("ID"=character(), taxa.var=character(), Quantity_old=numeric(), Quantity_new=numeric(), ambp=logical(), assigned=character())
  names(temp)[names(temp)=="taxa.var"]<-taxa.var
  for(value.var in names(comm[value.var.cols])){
    print(value.var)
    if (!value.var %in% names(comm)) 
      stop("value.var not found in data")
    if (any(is.na(comm[, value.var]))) 
      stop("No NAs in value.var allowed!")
    
    keep <- apply(hier, 2, function(x) any(is.na(x)))
    keep[rle(keep)$lengths[1]] <- TRUE
    keep[taxa.var] <- TRUE
    hier <- hier[, keep]
    lev <- rev(names(hier))
    lev <- lev[!lev %in% taxa.var]
    foo <- function(y, value.var) {
      # print(y[,which(names(y)==i)])
      if(all(is.na(y[,which(names(y)==i)]))){
        return(y)
      }
      else{
        if((which(names(y) == i) + 1)==(ncol(hier)-1)){
          childs <- !is.na(y[, which(names(y) == i) + 1])
        }
        else{
          childs <- apply(!is.na(y[, c((which(names(y) == i) + 1):(ncol(hier)-1))]), 1, any)
        }
        y[childs, taxa.var]
        parent <- !childs
        y[parent, taxa.var]
        if (sum(y[childs, value.var]) == 0 | all(childs)) {
          return(y)
        }
        else {
          w <- y[childs, value.var]/sum(y[childs, value.var])
          y[childs, "Quantity_new"] <- y[childs, value.var] + y[parent, value.var] * w
          y[parent, "Quantity_new"] <- 0
          y$ambp[parent] <- TRUE
          y$assigned[parent]<-paste(c(y[childs, taxa.var]), collapse=", ")
          return(y)
        }
      }}
    wdf <- cbind(hier, comm)
    wdf$ambp <- FALSE
    wdf$Quantity_new<-wdf[,value.var]
    wdf$ID<-value.var
    wdf$assigned<-""
    wdf<-subset(wdf, get(value.var)>0)
    for (i in lev[-1]) {
      wdf <- ddply(wdf, i, foo, value.var)
    }
    
    names(wdf)[which(names(wdf)==value.var)]<-"Quantity_old"
    commout <- wdf[, c("ID", taxa.var, "Quantity_old", "Quantity_new",  "ambp", "assigned")]
    
    temp<-rbind(temp, commout)
  }
  return(temp)
  
}

####DPAC#####
##This will take a several hours to run for a large dataset
dpac_out<-mod_dpac_s(x=out, value.var.cols=c(2:ncol(comm))) ##value.var.cols are the Visit.ID columns
dpac_out
counts2[!paste0(counts2$Sample.Code, counts2$OTU_COARSE_Unique) %in% paste0(dpac_out$ID, dpac_out$OTU_COARSE_Unique),]###these are missing taxa OTUs in regulatory effectiveness project. Don't worry about for now, not part of trends.

write.csv(dpac_out, "dpac_out_03092023_LRexclude.csv") #02012023 version, fixed Tipulidae s.l. taxonomy issue

dpac_out<-read.csv("dpac_out_03092023_LRexclude.csv")

#####################taxa tremds

library(rkt)
library(EnvStats)
library(tidyverse)
library(stringr)


# source('C:/Users/esosik/OneDrive - King County/Documents/biostats.R', encoding = 'UTF-8')
counts<-read.csv("Collapsed_Coarse_Taxa_density_LRexclude.csv")
lookup<-subset(counts, select=c(Site.Code, Sample.Code,Visit.ID, Project, Stream.or.River, Subbasin, WRIA.Number, Visit.Date, Year, Basin))
lookup<-unique(lookup)

# data<-read.csv("dpac_out.csv", header=T)
data<-dpac_out
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




write.csv(data, "KC_noAmbig_noReps_Rolledup_density_03092023_LRexclude.csv")
data<-read.csv("KC_noAmbig_noReps_Rolledup_density_03092023_LRexclude.csv")
##### end 9/21/2021 update####

################### Regional trends  ####
### convert to frequency data##

data_freq<-data
names(data_freq)
rownames(data_freq)<-data_freq$ID
data_freq<-data_freq[3:(ncol(data_freq)-13)]###select only taxa count data
data_freq[data_freq!=0]<-1 ##if present, mark as 1

###get rid of taxa that don't appear at least once in the dataset##
y1<-data_freq
min.cv=0
min.po=0
min.fo=1
max.po=100
max.fo=nrow(data_freq)
pct.missing=100
#statistical functions
cova<<-function(x,na.rm) sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE)*100
freq.occur<<-function(x,na.rm) sum(!x==0,na.rm=TRUE)
pct.occur<<-function(x,na.rm) sum(!x==0,na.rm=TRUE)/length(x)*100
fpct.missing<<-function(x,na.rm) sum(is.na(x))/length(x)*100

#delete offending variables
z<-as.matrix(apply(y1,2,cova)<min.cv |
               apply(y1,2,pct.occur)>max.po |
               apply(y1,2,pct.occur)<min.po |
               apply(y1,2,freq.occur)>max.fo |
               apply(y1,2,freq.occur)<min.fo |
               apply(y1,2,fpct.missing)>pct.missing)
z<-y1[,z[,1]==FALSE]


data_freq<-z

data_freq$ID<-rownames(data_freq)
data_freq<-merge(data[c(2, (ncol(data)-12):ncol(data))], data_freq, by="ID") ###add metadata back in

data_freq2<-data_freq %>% group_by(year) %>% summarise_if(is.numeric, max) #region-wide presence/absence, year to year (i.e. 1 if detected anywhere, 0 if not)
data_freq3<-data_freq %>% group_by(year) %>% summarise_if(is.numeric, sum) #frequency of occurence across sites, year to year (i.e. 23 if detected at 23 sites within a year)
names(data_freq3)
nsamp<-ddply(data, .(year), summarize, nsamp=length(unique(ID)))
data_freq3<-merge(data_freq3, nsamp, by.x="year", by.y="year")
data_freq3<-data_freq3 %>% mutate(across(Acari:Uenoidae..s.l., function(x) x/nsamp*100))
data_freq3<-subset(data_freq3, select=-nsamp)

# test<-reshape2::melt(data_freq3, id.vars=c("year", "Visit.ID", "WRIA.Number", "Year", "month", "por"))
# library(plyr)
# ties<-subset(ddply(test, .(variable, value), summarize, ties=length(value)), ties>1)
# FOQ<-read.csv("freq_of_occurrence_density_03132023_2002-2021.csv")
# FOQ_none<-subset(FOQ, mk.trend=="none")
# ties[ties$variable %in% FOQ_none$taxon,]
# ddply(test, .(variable), summarize, nyears=length(value))
# no_trends<-test[test$variable %in% FOQ_none$taxon,]
# ggplot(no_trends, aes(x=year, y=value))+geom_point()+facet_wrap(.~variable, scales="free")

#### region-wide pres/abs using logistic regression
### A positive trend indicates the taxa is occurring more frequently over time.

data.freq_trend<-data.frame(taxon=character(), slope=double(), inter=double(),pval=double(),trend=character(), samples=double(), sum.taxon=double(),stringsAsFactors = F)
names(data_freq2)
for (i in 7:ncol(data_freq2)) {
  data.lr<-NULL
  data.lr$taxon<-as.character(colnames(data_freq2[i]))
  data.lr$slope<-glm(as.formula(paste0(colnames(data_freq2[i]), " ~ ", "as.numeric(year)")), data=data_freq2, family="binomial")$coefficients[2]
  data.lr$inter<-glm(as.formula(paste0(colnames(data_freq2[i]), " ~ ", "as.numeric(year)")), data=data_freq2,family="binomial")$coefficients[1]
  data.lr$pval<-summary(glm(as.formula(paste0(colnames(data_freq2[i]), " ~ ", "as.numeric(year)")), data=data_freq2, family="binomial"))$coefficients[8]
  data.lr$trend <- as.character(ifelse(data.lr$pval < 0.05 & data.lr$slope < 0, "negative", ifelse(data.lr$pval < 0.05 & data.lr$slope > 0, "positive", "none"))) # add trend col
  data.lr$samples<-nrow(subset(data_freq2, data_freq2[i]!=0))
  data.lr$sum.taxon<-sum(subset(data_freq2, data_freq2[i]!=0)[i])
  
  data.lr<- as.data.frame(data.lr)
  data.freq_trend<- rbind(data.lr, data.freq_trend)
  data.freq_trend %>% mutate_if(is.factor, as.character) -> data.freq_trend
  
}
nrow(subset(data.freq_trend, trend!="none"))
subset(data.freq_trend, trend!="none")
subset(data.freq_trend, pval<.1)
unique(data.freq_trend$taxon)
write.csv(data.freq_trend, "region_pres-abs_density_03132023_2002-2021.csv")


####Look for trends in frequency of occurrence across streams, year to year. 
### A positive trend indicates the taxa is occurring at more streams over time.

data.freq_trend2<-data.frame(taxon=character(), mk.tau=double(), mk.Sen=double(),mk.pval=double(), mk.inter=double(), mk.trend=character(),samples=double(), sum.taxon=double(), stringsAsFactors = F)
names(data_freq3)
for (i in 7:ncol(data_freq3)) {
  data.lr<-NULL
  data.lr$taxon<-as.character(colnames(data_freq3[i]))
  data.lr$mk.tau<-kendallTrendTest(as.formula(paste0(colnames(data_freq3[i]), " ~ ", "as.numeric(year)")), data=data_freq3)$estimate[1]
  data.lr$mk.Sen<-kendallTrendTest(as.formula(paste0(colnames(data_freq3[i]), " ~ ", "as.numeric(year)")), data=data_freq3)$estimate[2]
  data.lr$mk.pval<-kendallTrendTest(as.formula(paste0(colnames(data_freq3[i]), " ~ ", "as.numeric(year)")), data=data_freq3)$p.value
  data.lr$mk.inter<-kendallTrendTest(as.formula(paste0(colnames(data_freq3[i]), " ~ ", "as.numeric(year)")), data=data_freq3)$estimate[3]
  data.lr$mk.trend <- as.character(ifelse(data.lr$mk.pval < 0.05 & data.lr$mk.tau < 0, "negative", ifelse(data.lr$mk.pval < 0.05 & data.lr$mk.tau > 0, "positive", "none")))
  data.lr$samples<-nrow(subset(data_freq3, data_freq3[i]!=0))
  data.lr$sum.taxon<-sum(subset(data_freq3, data_freq3[i]!=0)[i])
  data.freq_trend2<- rbind(data.lr, data.freq_trend2)
  data.freq_trend2 %>% mutate_if(is.factor, as.character) -> data.freq_trend2
  
}

# ggplot(data_freq3, aes(y=Cinygmula, x=year))+geom_point()
# ggplot(data_freq3, aes(y=Glutops, x=year))+geom_point()
# ggplot(data_freq3, aes(y=Psychodidae, x=year))+geom_point()
# ggplot(data_freq3, aes(y=Heterlimnius.corpulentus, x=year))+geom_point()
# ggplot(data_freq3, aes(y=Narpus, x=year))+geom_point()

nrow(subset(data.freq_trend2, mk.trend!="none"))
subset(data.freq_trend2, mk.trend!="none")
subset(data.freq_trend2, mk.pval<.1)
write.csv(data.freq_trend2, "freq_of_occurrence_density_10122023_2002-2021.csv")

test<-reshape2::melt(data_freq3, id.vars=c("year", "Visit.ID", "WRIA.Number", "Year", "month", "por"))
library(plyr)
ties<-subset(ddply(test, .(variable, value), summarize, ties=length(value)), ties>1)
FOQ<-read.csv("freq_of_occurrence_density_10122023_2002-2021.csv")
FOQ_none<-subset(FOQ, mk.trend=="none")
ties[ties$variable %in% FOQ_none$taxon,]
ddply(test, .(variable), summarize, nyears=length(value))
no_trends<-test[test$variable %in% FOQ_none$taxon,]
ggplot(no_trends, aes(x=year, y=value))+geom_point()+facet_wrap(.~variable, scales="free")


##Look for regional trends in taxa abundance, year to year.
### note: there's an extremely slight difference in rounding that causes this R project to have extremely slightly different abundance trend (region, subbasin and site) results for Skweltsa, Skwala, Parapsyche, Optioservus, Neophylax, Hemerodromiinae, Glossosoma, Dixa, Dicranota and Cinygma the **extremely** slight difference in densities (out to the 5th decimal place) is likely because of a different version of R being used.

names(data)
l=unique(c(as.character(data$Site.Code)))
# data <- data[complete.cases(data[, 1]), ]
str(data)
data$Site.Number<-as.numeric(factor(data$Site.Code, levels=l))
data$year<-as.numeric(data$year)

metrics<-colnames(data)[3:(ncol(data)-14)]
data.lr1<-data.frame(taxon=character(), rkt.pval=double(),rkt.Sen=double(), rkt.tau=double(), RKTtrend=character(),  samples=double(), sum.taxon=double(), stringsAsFactors = F)
# each<-metrics[248]
# each<-"Heterlimnius.corpulentus"
for (each in metrics) {
  data.lr<-NULL
  data.lr$taxon<-paste0(each)
  data.lr$rkt.pval <- with(data, rkt(date = year, y = get(each), block = Site.Number, correct = FALSE)[1])
  data.lr$rkt.Sen <- with(data, rkt(date = year, y = get(each), block = Site.Number, correct = FALSE)[3])
  data.lr$rkt.tau <- with(data, rkt(date = year, y = get(each), block = Site.Number, correct = FALSE)[12])
  data.lr$RKTtrend <- with(data.lr, ifelse(rkt.pval < 0.05 & rkt.tau < 0, "negative", ifelse(rkt.pval < 0.05 & rkt.tau > 0, "positive", "none")))
  data.lr$samples<-nrow(subset(data, get(each)!=0))
  data.lr$sum.taxon<-sum(subset(data, select=get(each)))
  data.lr<-unlist(data.lr)
  data.lr<-as.data.frame(t(data.lr))
  colnames(data.lr)[ncol(data.lr)-5]<-paste0("rkt.pval")
  colnames(data.lr)[ncol(data.lr)-4]<-paste0("rkt.Sen")
  colnames(data.lr)[ncol(data.lr)-3]<-paste0("rkt.tau")
  colnames(data.lr)[ncol(data.lr)-2]<-paste0("RKTtrend")
  data.lr1<- rbind(data.lr, data.lr1)
  print(each)
}

# ggplot(data, aes(y=Cinygmula, x=year, group=year))+geom_point()+geom_boxplot()
# ggplot(data, aes(y=Glutops, x=year, group=year))+geom_point()+geom_boxplot()
# ggplot(data, aes(y=Psychodidae, x=year, group=year))+geom_point()+geom_boxplot()
# ggplot(data, aes(y=Heterlimnius.corpulentus, x=year, group=year))+geom_point()+geom_boxplot()
# ggplot(data, aes(y=Narpus, x=year, group=year))+geom_point()+geom_boxplot()

nrow(subset(data.lr1, RKTtrend!="none"))
subset(data.lr1, RKTtrend!="none")
subset(data.lr1, rkt.pval<.1)
data.lr1$rkt.pval
str(data.lr)
write.csv(data.lr1, "regionwide_abundance_trends_RKT_density_03132023_2002-2021.csv")


#### Site/Basin trends ####

####look for trends in presence/absence by site using logistic regression
### A positive trend indicates the taxa is occurring more frequently over time within a given site.

scal<-"Site.Code" # "Site.Code" or "Subbasin"

data_freq_scal<-data_freq %>% group_by(get(scal), year) %>% summarise_if(is.numeric, max) #presence/absence, year to year (i.e. 1 if detected anywhere, 0 if not)
names(data_freq_scal)[1]<-paste0(scal)

data.lr3<-data.frame(taxon=character(), site=character(), slope=double(), inter=double(),pval=double(), trend=character(), samples=double(), sum.taxon=double(),stringsAsFactors = F)
names(data_freq_scal)
form<-names(data_freq_scal)[8:(ncol(data_freq_scal))]
# var<-"Zaitzevia"
for (var in form) {
  print(var)
  data.lr<-NULL
  data.lr$taxon<-ddply(data_freq_scal, .(get(scal)), summarize, var)[2]
  data.lr$site<-ddply(data_freq_scal, .(get(scal)), summarize, var)[1]
  data.lr$slope<-ddply(data_freq_scal, .(get(scal)), summarize, slope= glm(get(paste0(var)) ~ as.numeric(year), family="binomial")$coefficients[2])
  data.lr$inter<-ddply(data_freq_scal, .(get(scal)), summarize, inter= glm(get(paste0(var)) ~ as.numeric(year), family="binomial")$coefficients[1])
  data.lr$pval<-ddply(data_freq_scal, .(get(scal)), summarize, pval= summary(glm(get(paste0(var)) ~ as.numeric(year), family="binomial"))$coefficients[8])
  data.lr$trend <- as.character(ifelse(data.lr$pval$pval < 0.05 & data.lr$slope$slope < 0, "negative", ifelse(data.lr$pval$pval < 0.05 & data.lr$slope$slope > 0, "positive", "none"))) # add trend col
  data.lr$samples<-ddply(data_freq_scal, .(get(scal)), summarize, samples=length(which(get(var)!=0)))
  data.lr$sum.taxon<-ddply(data_freq_scal, .(get(scal)), summarize, sum.taxon=sum(get(var)))
  data.lr<- as.data.frame(data.lr)
  data.lr<-subset(data.lr, select=-c(slope.get.scal., inter.get.scal., pval.get.scal., samples.get.scal., sum.taxon.get.scal.))
  colnames(data.lr)<-c("taxon","site", "slope", "inter", "pval", "trend", "samples", "sum.taxon")
  data.lr3<- rbind(data.lr, data.lr3)
  data.lr3 %>% mutate_if(is.factor, as.character) -> data.lr3
  print(var)
}

nrow(subset(data.lr3, trend!="none"))
nrow(subset(data.lr3, pval<.1))
subset(data.lr3, pval<.1)
write.csv(data.lr3, paste0(scal,"_pres-abs_density_03132023_2002-2021.csv"))


##### Look for Abundance trends by site ### 
data.lr2<-data.frame(taxon=character(), site=character(),mk.tau=double(),mk.Sen=double(),mk.pval=double(), mk.inter=double(),  samples=double(), sum.taxon=double(),mk.trend=character(), stringsAsFactors = F)

# ddply(data,.(Site.Code),summarize, summ=(length(get(paste0(var)) )))

form<-names(data)[3:(ncol(data)-14)]
for (var in form){
  data.lr<-NULL
  data.lr$taxon<-ddply(data, .(Site.Code), summarize, var)[2]
  data.lr$site<-ddply(data,.(Site.Code),summarize, mk.Sen=kendallTrendTest(get(paste0(var)) ~ as.numeric(year))$estimate[2])[1]
  data.lr$mk.tau<-ddply(data,.(Site.Code),summarize, mk.tau=kendallTrendTest(get(paste0(var)) ~ as.numeric(year))$estimate[1])[2]
  data.lr$mk.Sen<-ddply(data,.(Site.Code),summarize, mk.Sen=kendallTrendTest(get(paste0(var)) ~ as.numeric(year))$estimate[2])[2]
  data.lr$mk.pval<-ddply(data,.(Site.Code),summarize, mk.pval=kendallTrendTest(get(paste0(var)) ~ as.numeric(year))$p.value)[2]
  data.lr$mk.inter<-ddply(data,.(Site.Code),summarize, mk.inter=kendallTrendTest(get(paste0(var)) ~ as.numeric(year))$estimate[3])[2]
  data.lr$mk.trend <- ifelse(data.lr$mk.pval < 0.05 & data.lr$mk.tau < 0, "negative", ifelse(data.lr$mk.pval < 0.05 & data.lr$mk.tau > 0, "positive", "none"))
  data.lr$samples<-ddply(data, .(Site.Code), summarize, samples=length(which(get(var)!=0)))
  data.lr$sum.taxon<-ddply(data, .(Site.Code), summarize, sum.taxon=sum(get(var)))
  data.lr<- as.data.frame(data.lr)
  data.lr<-subset(data.lr, select=-c(samples.Site.Code, sum.taxon.Site.Code))
  colnames(data.lr)<-c("taxon","site", "mk.tau", "mk.Sen", "mk.pval", "mk.inter", "trend","samples", "sum.taxon")
  data.lr2<- rbind(data.lr, data.lr2)
  
  data.lr2 %>% mutate_if(is.factor, as.character) -> data.lr2
  
}

subset(data.lr2, trend!="none")
subset(data.lr2, mk.pval<.1)
write.csv(data.lr2, "Site.Code_abundance_trends_density_03132023_2002-2021.csv")

##### Look for Spread trends by Basin ### 

data_freq_scal<-data_freq %>% group_by(Subbasin, year) %>% summarise_if(is.numeric, sum) #frequency of occurence across sites, year to year (i.e. 23 if detected at 23 sites within a year)

data.lr2<-data.frame(taxon=character(), site=character(),mk.tau=double(),mk.Sen=double(),mk.pval=double(), mk.inter=double(),  samples=double(), sum.taxon=double(),mk.trend=character(), stringsAsFactors = F)

form<-names(data_freq_scal)[8:ncol(data_freq_scal)]
for (var in form){
  data.lr<-NULL
  data.lr$taxon<-ddply(data_freq_scal, .(Subbasin), summarize, var)[2]
  data.lr$site<-ddply(data_freq_scal,.(Subbasin),summarize, mk.Sen=kendallTrendTest(get(paste0(var)) ~ as.numeric(year))$estimate[2])[1]
  data.lr$mk.tau<-ddply(data_freq_scal,.(Subbasin),summarize, mk.tau=kendallTrendTest(get(paste0(var)) ~ as.numeric(year))$estimate[1])[2]
  data.lr$mk.Sen<-ddply(data_freq_scal,.(Subbasin),summarize, mk.Sen=kendallTrendTest(get(paste0(var)) ~ as.numeric(year))$estimate[2])[2]
  data.lr$mk.pval<-ddply(data_freq_scal,.(Subbasin),summarize, mk.pval=kendallTrendTest(get(paste0(var)) ~ as.numeric(year))$p.value)[2]
  data.lr$mk.inter<-ddply(data_freq_scal,.(Subbasin),summarize, mk.inter=kendallTrendTest(get(paste0(var)) ~ as.numeric(year))$estimate[3])[2]
  data.lr$mk.trend <- ifelse(data.lr$mk.pval < 0.05 & data.lr$mk.tau < 0, "negative", ifelse(data.lr$mk.pval < 0.05 & data.lr$mk.tau > 0, "positive", "none"))
  data.lr$samples<-ddply(data_freq_scal, .(Subbasin), summarize, samples=length(which(get(var)!=0)))
  data.lr$sum.taxon<-ddply(data_freq_scal, .(Subbasin), summarize, sum.taxon=sum(get(var)))
  data.lr<- as.data.frame(data.lr)
  data.lr<-subset(data.lr, select=-c(samples.Subbasin, sum.taxon.Subbasin))
  colnames(data.lr)<-c("taxon","site", "mk.tau", "mk.Sen", "mk.pval", "mk.inter", "trend","samples", "sum.taxon")
  data.lr2<- rbind(data.lr, data.lr2)
  
  data.lr2 %>% mutate_if(is.factor, as.character) -> data.lr2
  
}


subset(data.lr2, trend!="none")
subset(data.lr2, mk.pval<.1)
write.csv(data.lr2,  "Subbasin_Spread_trends_density_03132023_2002-2021.csv")


#Look for RKT abundance trends by basin ####
names(data)
l=unique(c(as.character(data$Site.Code)))
# data <- data[complete.cases(data[, 1]), ]
str(data)
data$Site.Number<-as.numeric(factor(data$Site.Code, levels=l))
data$year<-as.numeric(data$year)

data.lr2<-data.frame(taxon=character(), site=character(), rkt.pval=double(),rkt.Sen=double(), rkt.tau=double(), RKTtrend=character(),  samples=double(), sum.taxon=double(), stringsAsFactors = F)

form<-colnames(data)[3:(ncol(data)-14)]
# each<-"Cleptelmis.addenda"
# unique(data$Basin  )
# data.frame(Basin=unique(data$Basin), Taxon=each)

detach(package:dplyr, unload=T) ##sometimes dplyr and plyr packages don't play nicely and we get errors with get() in the loop below. Toggle dply off and on to fix
library(dplyr)
paste0(each)
for (each in form){
  data.lr<-NULL
  data.lr_taxon<-NULL
  data.lr_rkt.pval<-NULL
  data.lr_rkt.Sen<-NULL
  data.lr_rkt.tau<-NULL
  data.lr_taxon<-ddply(data, .(Subbasin), summarize, each)[c(1,2)]
  data.lr_rkt.pval<-as.data.frame(ddply(data,.(Subbasin),summarize, rkt.pval=rkt(date = year, y = get(each), block=Site.Number, correct=FALSE)[1]))
  data.lr_rkt.pval$rkt.pval<-unlist(data.lr_rkt.pval[,2])
  data.lr_rkt.Sen<-as.data.frame(ddply(data,.(Subbasin),summarize, rkt.Sen=rkt(date = year, y = get(each), block=Site.Number, correct=FALSE)[3]))
  data.lr_rkt.Sen$rkt.Sen<-unlist(data.lr_rkt.Sen[,2])
  data.lr_rkt.tau<-as.data.frame(ddply(data,.(Subbasin),summarize, rkt.tau=rkt(date = year, y = get(each), block=Site.Number, correct=FALSE)[12]))
  data.lr_rkt.tau$rkt.tau<-unlist(data.lr_rkt.tau[,2])
  data.lr<-Reduce(function(x, y) merge(x, y), list(data.lr_taxon, data.lr_rkt.pval, data.lr_rkt.Sen, data.lr_rkt.tau))
  data.lr$RKTtrend <- ifelse(data.lr$rkt.pval < 0.05 & data.lr$rkt.tau < 0, "negative", ifelse(data.lr$rkt.pval < 0.05 & data.lr$rkt.tau > 0, "positive", "none"))
  data.lr$samples<-ddply(data, .(Subbasin), summarize, samples=length(which(get(each)!=0)))$samples
  data.lr$sum.taxon<-ddply(data, .(Subbasin), summarize, sum.taxon=sum(get(each)))$sum.taxon
  
  data.lr2<- rbind(data.lr, data.lr2)
  
  
  
}


nrow(subset(data.lr2, RKTtrend!="none"))
subset(data.lr2, rkt.pval<.05)
write.csv(data.lr2, "Subbasin_abundance_trends_RKT_density_03132023_2002-2021.csv")


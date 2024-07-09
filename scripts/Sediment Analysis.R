setwd(here::here())

library(bio.infer)
library(openxlsx)
library(lubridate)
setwd("./outputs")

coarse<-read.csv("Collapsed_Coarse_Taxa_LR_exclude.csv")

# coarse<-OTU_collapsed3


names(coarse)
# raw3<-raw2[, c(2,29,30, 48:69)]
raw3<-coarse[, c(2,3,15)]
names(raw3)<-c("SVN", "Taxon", "CountValue")
raw3tax<-get.taxonomic(raw3)
##Anafroptilum to CENTROPTILUM
##Afghanurus to HEPTAGENIA
## Nemata to NEMATODA

raw3tax$SVN<-as.factor(raw3tax$SVN)
raw3tax$Taxon<-as.factor(raw3tax$Taxon)
data("coef.west.wt")
OTU<-get.otu(raw3tax, coef.west.wt, outputFile = TRUE)
unique(subset(OTU, is.na(OTU))$Taxon)
unique(subset(OTU, is.na(OTU))$TNAME)
coefnames<-coef.west.wt$tnames
coefnames<-as.data.frame(coefnames)
###Need to run get.otu for each sample, right now it drops "parent" taxa that occur less frequently in the dataset, but doesn't assign those parents to children--data loss!
ss<-makess(OTU)
inference <- mlsolve(ss, coef.west.wt, bruteforce = F)
write.csv(inference, "bio.infer.predictions_trends_031423.csv")
inference<-read.csv("bio.infer.predictions_trends_031423.csv")
samples<-unique(subset(coarse, select=c(Site.Code, Sample.Code, Visit.Date)))
samples<-merge(samples, inference, by.x="Sample.Code", by.y="SVN")

write.csv(samples, "samples_w_bioinfer_trends_031423.csv")
# data(itis.ttable)
# write.csv(itis.ttable, "itis.csv")
samples<-read.csv("samples_w_bioinfer_trends_031423.csv")


samples$Visit.Date<-as.Date(samples$Visit.Date, "%Y-%m-%d")
samples$Year<-year(samples$Visit.Date)
library(plyr)
samples <- droplevels(samples[samples$Year > 2001,]) # subset to sites since 2002
samples <- ddply(samples, 'Site.Code', mutate, por = length(unique(Year))) # add on period of record col
samples<-droplevels(samples[samples$por > 9,])# look only at sites with >9 years of data

samples <- ddply(samples, 'Site.Code', mutate, timerange = (as.numeric(max(Year))-as.numeric(min(Year))))
samples<-droplevels(samples[samples$timerange > 9,])# look only at streams with >9 years of data
library(ggplot2)
ggplot(samples, aes(x=Year, y=sed))+geom_smooth(se=F)+theme(legend.position = "none")+
  geom_point()
ggplot(samples, aes(x=Year, y=sed, group=Year))+geom_smooth(se=F)+theme(legend.position = "none")+
  geom_boxplot()
meansed<-ddply(samples, .(Year), summarize, meansed=mean(sed), nyears=length(unique(Site.Code)), sd=sd(sed), min=min(sed), max=max(sed), median=median(sed))
library(ggpmisc)
meanscore<-ggplot(meansed, aes(x=Year, y=meansed))+geom_smooth(method="lm",se=F)+theme(legend.position = "none")+
  geom_point()+ylim(c(0,100))+labs(y="Sediment Score")+
  stat_poly_eq(formula = y ~ x, rr.digits = 2, coef.digits = 3, size = 4, color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~")), parse = TRUE) 

Sediment<-ggplot(samples )+
    geom_boxplot(aes(y=sed, x=as.numeric(paste0(Year)), group=as.factor(Year)), fill="white")+
  geom_point(data=meansed, mapping=aes(x=as.numeric(paste0(Year)), y=meansed))+
  geom_smooth(data=meansed, mapping=aes(x=as.numeric(paste0(Year)), y=meansed), method="loess",se=F)+
  labs(y="% Sand/Fine Sediment", x="Year")+ theme(text = element_text(size = 28))+
  geom_quantile(data=samples, mapping=aes(x=as.numeric(paste0(Year)), y=sed), quantiles=.5, color="red", size=1)


##############



##############FSBI#########

library(odbc)
library(data.table)
library(stringr)
library(bio.infer)
library(ggplot2)
library(sf)
library(dplyr)
library(plyr)
library(tidyverse)
library("readxl")
library(stringr)

setwd(here::here())
setwd("./inputs")
FSBI_Index <- read_excel("FSBI_Index.xlsx") ##Diane's FSBI index table from her internship

#executive decision to take out the subgroups of Rhyacophila -> Keep rhyacophila sibirica = 0 

FSBI_Index<-subset(FSBI_Index, !(Taxon=="Rhyacophila Sibirica Group"&FSBI_Score>0|Order_Nsize_FFG_Habit_O.V.=="- blarina T 42 P Clinger 8.0"))





# CoarseTaxon<-coarse %>%
#   distinct(OTU_COARSE, .keep_all = TRUE)#remove duplicates 

FSBI_Index$Taxon<-str_trim(FSBI_Index$Taxon,side=c("both"))#trim spaces from both sides
names(coarse)
# str_to_upper(coarse[,c(3,17:38)])
coarse<-coarse %>% mutate(across(c(3,17:38), str_to_upper))
coarse<-coarse %>% #don't use for taxa analysis
  mutate_at(
    .vars = vars(names(coarse[,c(3,17:38)])),
    .funs = ~ str_replace(., pattern = " S.L.", replacement = "")
  )

FSBI_Index$Taxon<-str_to_upper(FSBI_Index$Taxon)


coarse$mergefield<-ifelse(coarse$OTU_COARSE %in% FSBI_Index$Taxon, coarse$OTU_COARSE, 
                            ifelse(coarse$Subspecies %in% FSBI_Index$Taxon, coarse$Subspecies,
                                   ifelse(coarse$Species %in% FSBI_Index$Taxon, coarse$Species,
                                          ifelse(coarse$Subgenus %in% FSBI_Index$Taxon, coarse$Subgenus, 
                                                 ifelse(coarse$Genus %in% FSBI_Index$Taxon, coarse$Genus, 
                                                        ifelse(coarse$Genus.Group %in% FSBI_Index$Taxon, coarse$Genus.Group, 
                                                               ifelse(coarse$Subtribe %in% FSBI_Index$Taxon, coarse$Subtribe, 
                                                                      ifelse(coarse$Tribe %in% FSBI_Index$Taxon, coarse$Tribe, 
                                                                             ifelse(coarse$Custom.Subfamily %in% FSBI_Index$Taxon, coarse$Custom.Subfamily, 
                                                                                    ifelse(coarse$Subfamily %in% FSBI_Index$Taxon, coarse$Subfamily, 
                                                                                           ifelse(coarse$Family %in% FSBI_Index$Taxon, coarse$Family, 
                                                                                                  ifelse(coarse$Superfamily %in% FSBI_Index$Taxon, coarse$Superfamily,
                                                                                                         ifelse(coarse$Infraorder %in% FSBI_Index$Taxon,coarse$Infraorder,
                                                                                                                ifelse(coarse$Suborder %in% FSBI_Index$Taxon, coarse$Suborder, 
                                                                                                                       ifelse(coarse$Order %in% FSBI_Index$Taxon, coarse$Order, 
                                                                                                                              ifelse(coarse$Superorder %in% FSBI_Index$Taxon, coarse$Superorder,
                                                                                                                                     ifelse(coarse$Infraclass %in% FSBI_Index$Taxon, coarse$Infraclass,
                                                                                                                                            ifelse(coarse$Subclass %in% FSBI_Index$Taxon, coarse$Subclass, 
                                                                                                                                                   ifelse(coarse$Class %in% FSBI_Index$Taxon, coarse$Class,
                                                                                                                                                          ifelse(coarse$Superclass %in% FSBI_Index$Taxon, coarse$Superclass,
                                                                                                                                                                 ifelse(coarse$Subphylum %in% FSBI_Index$Taxon, coarse$Subphylum,
                                                                                                                                                                        ifelse(coarse$Phylum %in% FSBI_Index$Taxon, coarse$Phylum,
                                                                                                                                                                               ""))))))))))))))))))))))


unique(subset(coarse, mergefield=="", select=OTU_COARSE))
coarse<-subset(coarse, Project!="Regulatory Effectiveness")

coarse[coarse$OTU_COARSE=="CHELIFERA/METACHELA","mergefield"]<-"CHELIFERA"
coarse[coarse$OTU_COARSE=="KOGOTUS/RICKERA","mergefield"]<-"KOGOTUS"
coarse[coarse$OTU_COARSE=="NEAVIPERLA/ SUWALLIA","mergefield"]<-"NEAVIPERLA"
coarse[coarse$OTU_COARSE=="PARALEPTOPHLEBIA/ NEOLEPTOPHLEBIA","mergefield"]<-"PARALEPTOPHLEBIA"

# m3<-merge(PSSBTaxon, FSBI_Index, by.x="Taxon", all.x=TRUE) #change name of score, then create a new vector - if species na grab family etc. 
# m3<-dplyr::rename(m3, FSBI_Taxon=FSBI_Score)
# 
# 
# ####Merge Genus to FSBI####
# m4<-merge(x=m3,FSBI_Index, by.x="Genus", by.y="Taxon", all.x=TRUE)
# m4<-dplyr::rename(m4, FSBI_Genus=FSBI_Score)
# 
# 
# ####Merge Family to FSBI####
# m5 <- merge (x=m4,FSBI_Index, by.x="Family", by.y="Taxon", all.x=TRUE)
# 
# 
# m5<-dplyr::rename(m5, FSBI_Family=FSBI_Score)
# m5<-select(m5, -"Order_Nsize_FFG_Habit_O.V.", -"Order_Nsize_FFG_Habit_O.V..x", -"Order_Nsize_FFG_Habit_O.V..y")
# 
# ####Merge Subamily to FSBI####
# m6 <- merge(x=m5,FSBI_Index, by.x="Subfamily", by.y="Taxon", all.x=TRUE)
# m6<-dplyr::rename(m6, FSBI_Subfamily=FSBI_Score)
# 
# ####Write if else statement loop to get finalfsbi column (combined)####
# finalfsbi<-mutate(m6, finalfsbi= ifelse(!is.na(FSBI_Taxon), FSBI_Taxon, 
#                                         ifelse(!is.na(FSBI_Genus), FSBI_Genus, 
#                                                ifelse(!is.na(FSBI_Subfamily), FSBI_Subfamily,
#                                                       ifelse(!is.na(FSBI_Family), FSBI_Family, NA)))))  
# raw_unique_samples <- raw2  # Duplicate data frame
# raw_unique_samples$QC.Replicate.Of.Sample.Code[raw_unique_samples$QC.Replicate.Of.Sample.Code == ""] <- NA  # Replace all blanks in dataset by NA
# 
# raw_unique_samples<-subset(raw_unique_samples, is.na(QC.Replicate.Of.Sample.Code))
# selectfinalfsbi<-select(finalfsbi, Taxon, Taxon.Serial.Number, finalfsbi)
# 

# masterwfsbi<-merge(raw_unique_samples, selectfinalfsbi, by.x="Taxon", by.y="Taxon", all.x=TRUE) 
masterwfsbi<-merge(coarse, FSBI_Index, by.x="mergefield", by.y="Taxon", all.x=TRUE)

length(unique(masterwfsbi$Sample.Code))
length(unique(masterwfsbi$Visit.ID))

masterwfsbi[which(masterwfsbi[which(duplicated(masterwfsbi[,c("Sample.Code", "mergefield")])),"mergefield"]!=""),]

FSBIcollapse<-ddply(masterwfsbi, .(Sample.Code, OTU_COARSE, Site.Code, Visit.Date, Year), summarize, finalfsbi=mean(FSBI_Score))

FSBI<-ddply(FSBIcollapse, .(Sample.Code, Site.Code, Visit.Date, Year), summarize, FSBI=sum(finalfsbi, na.rm=T))
setwd(here::here())
setwd("./outputs")
write.csv(FSBI, "FSBI_trends_031423.csv")
FSBI<-read.csv("FSBI_trends_031423.csv")

FSBI <- droplevels(FSBI[FSBI$Year > 2001,]) # subset to sites since 2002
FSBI <- ddply(FSBI, 'Site.Code', mutate, por = length(unique(Year))) # add on period of record col
FSBI<-droplevels(FSBI[FSBI$por > 9,])# look only at sites with >9 years of data

FSBI <- ddply(FSBI, 'Site.Code', mutate, timerange = (as.numeric(max(Year))-as.numeric(min(Year))))
FSBI<-droplevels(FSBI[FSBI$timerange > 9,])# look only at streams with >9 years of data

ggplot(FSBI, aes(x=Year, y=FSBI))+geom_smooth(se=F)+theme(legend.position = "none")+
  geom_point()
ggplot(FSBI, aes(x=Year, y=FSBI, group=Year))+geom_smooth(se=F)+theme(legend.position = "none")+
  geom_boxplot()
meansed<-ddply(FSBI, .(Year), summarize, meansed=mean(FSBI), nyears=length(unique(Site.Code)), sd=sd(FSBI), min=min(FSBI), max=max(FSBI), median=median(FSBI))
library(ggpmisc)
meanscore<-ggplot(meansed, aes(x=Year, y=meansed))+geom_smooth(method="lm",se=F)+theme(legend.position = "none")+
  geom_point()+ylim(c(0,100))+labs(y="FSBI Sediment Score")+
  stat_poly_eq(formula = y ~ x, rr.digits = 2, coef.digits = 3, size = 4, color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~")), parse = TRUE) 

FSBISediment<-ggplot(FSBI )+
  geom_boxplot(aes(y=FSBI, x=as.numeric(paste0(Year)), group=as.factor(Year)), fill="white")+
  geom_point(data=meansed, mapping=aes(x=as.numeric(paste0(Year)), y=meansed))+
  geom_smooth(data=meansed, mapping=aes(x=as.numeric(paste0(Year)), y=meansed), method="loess",se=F)+
  labs(y="FSBI Score", x="Year")+ theme(text = element_text(size = 28))+
  geom_quantile(data=FSBI, mapping=aes(x=as.numeric(paste0(Year)), y=FSBI), quantiles=.5, color="red", size=1)


################# BSTI ############

library(bio.infer)
library(stringr)
setwd(here::here())
setwd("./inputs")
OR_OTU<-read.csv("Cal_Sed_bugs.csv") ## this is the bug data that Shannon HUbler's model is based on. We just need the bug names to make sure our names match.

taxa<-names(OR_OTU) ## we just want the names of the taxa
taxa<-taxa[4:243] ## remove non-taxa names
taxa<-as.data.frame(x=taxa) ##convert to data frame
taxa$tnames<-taxa$taxa ##create new column where we can edit the names to make a lookup table
taxa$tnames<-str_replace_all(taxa$tnames, "Gr_", "Group")
taxa$tnames<-str_replace_all(taxa$tnames, "_", ".")
taxa$tnames<-str_replace_all(taxa$tnames, "Gr$", "Group")
taxa$tnames<-str_trim(taxa$tnames)
taxa$tnames<-str_to_upper(taxa$tnames)
# taxa2<-as.data.frame(x=taxa)
# taxa2$tnames<-taxa2$taxa
# taxa2<-subset(taxa2, select=c(tnames))
# 
# OR_OTU<-reshape2::melt(OR_OTU, id.vars=c("CodeNum", "CodeName", "FullName"), variable.name="Taxon", value.name = "CountValue")
# OR_OTU$Taxa<-OR_OTU$Taxon
# OR_OTU$Taxa<-str_replace_all(OR_OTU$Taxa, "Gr_", "Group")
# OR_OTU$Taxa<-str_replace_all(OR_OTU$Taxa, "_", ".")
# OR_OTU$Taxa<-str_replace_all(OR_OTU$Taxa, "Gr$", "Group")
# OR_OTU$Taxa<-str_trim(OR_OTU$Taxa)
# OR_OTU$Taxa<-str_to_upper(OR_OTU$Taxa)
# OR_OTU$TNAME<-OR_OTU$Taxa


library(openxlsx)

# raw<-read.xlsx("PSSB_Taxa_List.xlsx", detectDates = T)
# 
# ## load in your taxa data
# taxaBind <- function(file.path) {
#   
#   path.files <- list.files(file.path)
#   # read in files
#   list.with.each.file <- lapply(paste(file.path, list.files(file.path), sep = ''), function(y) read.delim(y, header=TRUE))
#   taxa<-do.call("rbind.data.frame", list.with.each.file) 
#   return(taxa)
#   
#   
# }
# file.path="./taxa_data/"
# raw<-taxaBind(file.path)

## lines 49-64 I'm just subsetting the taxa data to only the samples I want. Replace this with whatever code you need to get your desired set of samples
# pts<-read.csv("basin_pts.csv")
# pts[pts$Site_Code=="16","Site_Code"]<-"016"
# sites<-unique(pts$Site_Code)
# sites[!sites %in% raw$Site.Code]
# raw2<-subset(raw, Site.Code %in% sites)
# selections<-read.xlsx("Updated_basins_scores2.xlsx", sheet=2)
# 
# VisitIDs<-c(selections$GroupAVisitID, selections$GroupBVisitID)
# sum(!is.na(selections$GroupAVisitID))+sum(!is.na(selections$GroupBVisitID))
# VisitIDs<-VisitIDs[!is.na(VisitIDs)]
# unique(raw2$Visit.ID)
# unique(raw2$Site.Code)
# 
# raw2<-subset(raw2, Visit.ID %in% VisitIDs)
# names(raw)
# unique(raw2$Taxon[!raw2$Taxon %in% taxa])

##going back to my original work directory, skip this or replace with whatever directory you want your output files going to.
# setwd("C:/Users/esosik/King County/DNRP - Science TCA Unit - Bug Monitoring/Sediment Index Exploration/C2 model files_SedTemp.2005-20220120T205138Z-001/C2 model files_SedTemp.2005")

##I am using functions in Bio.Infer to ease the translation.
##BIG NOTE: when the edit window pops up, before editing the taxa look to see if any match Shannon's taxa (in the taxa object)
##Specifically, Turbellaria will be one that bio.infer doesn't recognize, but is on Shannon's list.
##In the edit window, leave Turbellaria (and any other taxa that are also in Shannon's list) as is.
##Use whatever other translations you used when you ran bio.infer
# raw3<-raw[, c(4,30,31)]
# names(raw3)<-c("SVN", "Taxon", "CountValue")
# raw3tax<-get.taxonomic(raw3)
raw3tax
raw3tax$SPECIES<-str_replace(raw3tax$SPECIES, "/COMPLEX", "") ##I noticed that Shannon's taxa didn't have "Complex" in the taxa names, and that tripped things up a bit.
## here I'm using bio.infer's get.otu function to automatically translate/rollup my taxa to match Shannon's where possible
test<-get.otu(raw3tax, taxa, outputFile = F) 

## I looked through the resulting list to see which taxa couldn't be matched to Shannon's. 
##I compared these to Shannon's taxa names to see if there are any taxa that should match but did not
unique(subset(test, is.na(OTU))$TNAME)##(compare to the "taxa" data object)
unique(subset(test, is.na(OTU))$Taxon) ##(compare to the "taxa" data object)

##Drunella grandis/spinifera didn't match, but both D. grandis and D. spinifera are in Shannon's list
##I looked to see how common each species is in my dataset,
# test[(test$Taxon=="Drunella grandis/spinifera"),]
# test[(test$Taxon=="Drunella grandis"),]
# test[(test$Taxon=="Drunella spinifera"),]


##we're constructing a lookup table to cross walk between the OTUs that match Shannon's modified names, and the taxa names in our raw data. 
lookup<-unique(subset(test, select=c(Taxon, TNAME, OTU)))

## I  assigned the D grandis/spinifera to D spinifera
# lookup[(lookup$Taxon=="Drunella grandis/spinifera"),]$OTU<-"DRUNELLA.SPINIFERA"
##fixed Chelifera to match Shannon's list
lookup[(lookup$Taxon=="Chelifera/Metachela"),]$OTU<-"CHELIFERA.METACHELA"
# lookup[(lookup$Taxon=="Pericoma/Telmatoscopus"),]$OTU<-"PERICOMA.TELMATOSCOPUS"
lookup[(lookup$Taxon=="Neaviperla/ Suwallia"),]$OTU<-"SUWALLIA"
lookup[(lookup$Taxon=="Kogotus/Rickera"),]$OTU<-"KOGOTUS.RICKERA"
lookup[(lookup$Taxon=="Drunella doddsii"),]$OTU<-"DRUNELLA.DODDSI"
lookup[(lookup$Taxon=="Matriella teresa"),]$OTU<-"SERRATELLA.TERESA"
##checking again to make sure those changes worked
unique(subset(lookup, is.na(OTU))$Taxon)##(compare to the "taxa" data object)
unique(subset(lookup, is.na(OTU))$TNAME)##(compare to the "taxa" data object)

### now we're adding to our lookup to crosswalk between the taxa names in our data, the names we modified in Shannon's list,and Shannon's original taxa names
Sed_OTUs<-merge(taxa, lookup, by.x="tnames", by.y="OTU")
# Sed_OTUs<-unique(subset(Sed_OTUs, select=c(TNAME, Taxon.x, Taxa, Taxon.y, TNAME.y)))

rawdata<-subset(coarse, select=c(Visit.ID, Sample.Code, Site.Code, Visit.Date, OTU_COARSE, Quantity_OTU)) ##just getting my raw taxa data formatted for Shannon's model.
Sed_OTUs$Taxon<-str_to_upper(Sed_OTUs$Taxon)### we made OTU_COARSE be all upper case in the FSBI segment, need to convert Taxon to match
rawdata<-merge(rawdata, Sed_OTUs[, c("taxa", "Taxon")], by.x="OTU_COARSE", by.y="Taxon", all.x=T) ### we want to convert our taxa names to match Shannon's original list where ever possible
missing<-unique(rawdata[is.na(rawdata$taxa),"OTU_COARSE"])##one last look to see if anything isn't getting matched to Shannon's list that should (compare to the "taxa" data object)
unique(coarse[coarse$OTU_COARSE %in% missing,"Family"])## see if there are any coarse OTUs that should get rolled up but did not
str_subset(taxa$taxa, "dae")
unique(coarse[coarse$OTU_COARSE %in% missing,c("Family", "OTU_COARSE")])

unique(coarse[coarse$OTU_COARSE %in% missing,"Subfamily"])
str_subset(taxa$taxa, "nae")
unique(coarse[coarse$OTU_COARSE %in% missing,c("Family", "OTU_COARSE")])

rawdata[rawdata$OTU_COARSE=="POLYCENTROPUS","taxa"]<-"Polycentropodidae"
rawdata[rawdata$OTU_COARSE=="SPHAERIUM","taxa"]<-"Sphaeriidae"
##whereever there is an OTU to match Shannon's list, make that the new taxon name
rawdata$OTU_COARSE<-ifelse(!is.na(rawdata$taxa), as.character(rawdata$taxa), paste0(as.character(rawdata$OTU_COARSE)))


##Now just final formatting to get it into shape for C2
library(lubridate)
rawdata$Visit.Date<-as.Date(rawdata$Visit.Date, "%Y-%m-%d")
rawdata$Year<-year(rawdata$Visit.Date)
# rawdata$Site.Code<-paste0(rawdata$Site.Code, "_", rawdata$Visit.ID, "_", rawdata$Year)
rawdata$Site.Code<-rawdata$Sample.Code
data.out<-reshape2::dcast(rawdata, Visit.ID+Site.Code~OTU_COARSE, value.var="Quantity_OTU", fun.aggregate=sum)
data.out$longname<-data.out$Site.Code
names(data.out)
# write.csv(data.out[,c("longname", "Visit.ID", "Site.Code")], "BSTI_lookup.csv")
data.out<-data.out[, c(1,2, 253, 3:252)] ## this is re-arranging the columns to be Visit.ID, Site.Code, and then longname. You'll need to adjust based on the number of columns you have.
setwd(here::here())
setwd("./outputs")
write.csv(data.out, "all_KC_C2_data_out_trends_031423.csv", row.names = F)
write.csv(rawdata, "BSTI_rawdata_031423.csv")
#####################################
### at this point you need to use C2 software and Shannon Hubler's model to calculate BSTI outside of R. You'll need to download C2 software from https://www.staff.ncl.ac.uk/stephen.juggins/software/C2Home.htm and use the model files in C2 model ./files_SedTemp.2005, along with the guide from Shannon -- "Running stressor ID models.docx"
############################################################
setwd(here::here())
setwd("./inputs")
BSTI<-read.xlsx("BSTI_trends_031423.xlsx")
# BSTI_lookup<-read.csv("BSTI_lookup.csv")
# BSTI_rawdata<-read.csv("BSTI_rawdata_031423.csv")

# BSTI<-merge(BSTI, unique(BSTI_rawdata[,c("Site.Code", "Visit.ID", "Year")]), by.x="Code", by.y="Site.Code")
BSTI<-merge(BSTI, unique(subset(coarse, select=c(Sample.Code, Year, Site.Code))), by.x="Code", by.y="Sample.Code")

# str_split(BSTI$Code, "_", simplify = T)[,c(1,2)][str_split(BSTI$Code, "_", simplify = T)[,4]!=""]
# BSTI[str_split(BSTI$Code, "_", simplify = T)[,4]!="","Site.Code"]<-paste0(str_split(BSTI$Code, "_", simplify = T)[,c(1)][str_split(BSTI$Code, "_", simplify = T)[,4]!=""], "_",
#                                                                     paste0(str_split(BSTI$Code, "_", simplify = T)[,c(2)][str_split(BSTI$Code, "_", simplify = T)[,4]!=""]))
# BSTI[str_split(BSTI$Code, "_", simplify = T)[,4]=="","Site.Code"]<-paste0(str_split(BSTI$Code, "_", simplify = T)[,1][str_split(BSTI$Code, "_", simplify = T)[,4]==""])

# str_split(BSTI$Code, "_", simplify = T)[,c(4)][str_split(BSTI$Code, "_", simplify = T)[,4]!=""]
# BSTI[str_split(BSTI$Code, "_", simplify = T)[,4]!="","Year"]<-paste0(str_split(BSTI$Code, "_", simplify = T)[,c(4)][str_split(BSTI$Code, "_", simplify = T)[,4]!=""])
# BSTI[str_split(BSTI$Code, "_", simplify = T)[,4]=="","Year"]<-paste0(str_split(BSTI$Code, "_", simplify = T)[,3][str_split(BSTI$Code, "_", simplify = T)[,4]==""])




BSTI <- droplevels(BSTI[BSTI$Year > 2001,]) # subset to sites since 2002
# BSTI$Site.Code<-str_split(BSTI$Code, "_", simplify = T)[,1]

BSTI <- ddply(BSTI, 'Site.Code', mutate, por = length(unique(Year))) # add on period of record col
BSTI<-droplevels(BSTI[BSTI$por > 9,])# look only at sites with >9 years of data

BSTI <- ddply(BSTI, 'Site.Code', mutate, timerange = (as.numeric(max(Year))-as.numeric(min(Year))))
BSTI<-droplevels(BSTI[BSTI$timerange > 9,])# look only at streams with >9 years of data


ggplot(BSTI, aes(x=Year, y=BSTI_Sed))+geom_smooth(se=F)+theme(legend.position = "none")+
  geom_point()
ggplot(BSTI, aes(x=Year, y=BSTI_Sed, group=Year))+geom_smooth(se=F)+theme(legend.position = "none")+
  geom_boxplot()
meansed<-ddply(BSTI, .(Year), summarize, meansed=mean(BSTI_Sed), nyears=length(unique(Code)), sd=sd(BSTI_Sed), min=min(BSTI_Sed), max=max(BSTI_Sed), median=median(BSTI_Sed))
library(ggpmisc)
meanscore<-ggplot(meansed, aes(x=Year, y=meansed))+geom_smooth(method="lm",se=F)+theme(legend.position = "none")+
  geom_point()+ylim(c(0,100))+labs(y="BSTI Sediment Score")+
  stat_poly_eq(formula = y ~ x, rr.digits = 2, coef.digits = 3, size = 4, color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~")), parse = TRUE) 

BSTISediment<-ggplot(BSTI )+
  geom_boxplot(aes(y=BSTI_Sed, x=as.numeric(paste0(Year)), group=as.factor(Year)), fill="white")+
  geom_point(data=meansed, mapping=aes(x=as.numeric(paste0(Year)), y=meansed))+
  geom_smooth(data=meansed, mapping=aes(x=as.numeric(paste0(Year)), y=meansed), method="loess",se=F)+
  labs(y="BSTI % Fines", x="Year")+ theme(text = element_text(size = 28))+
  geom_quantile(data=BSTI, mapping=aes(x=as.numeric(paste0(Year)), y=BSTI_Sed), quantiles=.5, color="red", size=1)




library(rkt)
names(samples)
l=unique(c(as.character(samples$Site.Code)))
samples$Site.Number<-as.numeric(factor(samples$Site.Code, levels=l))
with(samples, rkt(date = Year, y = sed, block = Site.Number, correct = FALSE, rep="a"))

names(FSBI)
l=unique(c(as.character(FSBI$Site.Code)))
FSBI$Site.Number<-as.numeric(factor(FSBI$Site.Code, levels=l))
with(FSBI, rkt(date = Year, y = FSBI, block = Site.Number, correct = FALSE, rep="a"))

names(BSTI)
l=unique(c(as.character(BSTI$Site.Code)))
BSTI$Site.Number<-as.numeric(factor(BSTI$Site.Code, levels=l))
with(BSTI, rkt(date = as.numeric(paste0(Year)), y = BSTI_Sed, block = Site.Number, correct = FALSE, rep="a"))


unique(BSTI$Site.Code)

merged<-merge(samples, FSBI, by=c("Sample.Code", "Site.Code", "Year"), all.x=T, all.y = T)
merged<-merge(merged, BSTI, by.x=c("Sample.Code", "Year", "Site.Code"), by.y=c("Code", "Year", "Site.Code"), all=T)

merged[which(!complete.cases(merged[,c("sed", "FSBI", "BSTI_Sed")])), ]
length(unique(samples$Sample.Code))

setwd(here::here())
setwd("./outputs")
KC_scores<-read.csv("B-IBI_results_03082023_2002-2021_NoUnique_subsamp_450_LR_exclude.csv")
KC_scores<-subset(KC_scores, Tot_Abund>=450)

KC_trends<-read.csv("Site_Trends_coarse_LR_03092023_2002-2021_LRexclude.csv")
KC_trends<-subset(KC_trends, !(Site.Code=="09DUW0277"&Stream.or.River=="Riverton Creek (003D)"))
KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code))
KC_scores<-subset(KC_scores, Site.Code %in% KC_trends$Site.Code)
samples<-subset(KC_scores, select=c(Sample.Code,Site.Code, Visit.Date, Overall.Score)) ### make sure to fix 09DUW0277/05, incorrectly given site as 08BEA3312 in PSSB and B-IBI results
samples$Visit.Date<-as.Date(samples$Visit.Date, "%Y-%m-%d")
samples<-inner_join(samples, merged, by=c("Site.Code"="Site.Code", "Visit.Date"="Visit.Date.x", "Sample.Code"="Sample.Code"))
# samples$Sample.Code<-str_replace(samples$Sample.Code, "/", "_")
# samples$Sample.Code<-str_replace(samples$Sample.Code, "-", "_")
samples<-samples[!str_detect(samples$Sample.Code, "_R"),]
samples<-samples[!str_detect(samples$Sample.Code, "[0-9]R[0-9]"),]
samples<-samples[!str_detect(samples$Sample.Code, "[0-9]R$"),]
samples<-samples[!str_detect(samples$Sample.Code, "QC"),]
samples<-samples[!str_detect(samples$Sample.Code, "_r"),]
samples<-samples[!str_detect(samples$Sample.Code, "Duplicate"),]


cor.test(samples$Overall.Score, samples$BSTI_Sed, method="spearman")
cor.test(samples$Overall.Score, samples$sed, method="spearman")
cor.test(samples$Overall.Score, samples$FSBI, method="spearman")

library(EnvStats)
samples<-samples[which(complete.cases(samples)),]
# data2<- ddply(samples, .(Site.Code), mutate, slope =lm(BSTI_Sed ~ as.numeric(Year))[[1]][2])
data2<-ddply(samples,.(Site.Code),mutate, mk.tau=kendallTrendTest( BSTI_Sed~ as.numeric(Year))$estimate[1])
data2<-ddply(data2,.(Site.Code),mutate, mk.slope=kendallTrendTest( BSTI_Sed~ as.numeric(Year))$estimate[2])

data2_trends<-unique(subset(data2, select=c(Site.Code, mk.slope, mk.tau))) 
# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))
data2_score_trends<-merge(KC_trends, data2_trends,  by.x="Site.Code", by.y="Site.Code")
summary(data2_score_trends$mk.slope)
# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$mk.tau)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$mk.slope, method="spearman")
# ggplot(data2_score_trends, aes(y=MKSlope_Overall_Score, x=slope))+geom_point()


data2<-ddply(samples,.(Site.Code),mutate, mk.tau=kendallTrendTest( sed~ as.numeric(Year))$estimate[1])
data2<-ddply(data2,.(Site.Code),mutate, mk.slope=kendallTrendTest( sed~ as.numeric(Year))$estimate[2])
data2_trends<-unique(subset(data2, select=c(Site.Code, mk.slope, mk.tau))) 
# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))
data2_score_trends<-merge(KC_trends, data2_trends,  by.x="Site.Code", by.y="Site.Code")
summary(data2_score_trends$mk.slope)
# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$mk.tau)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$mk.slope, method="spearman")
ggplot(data2_score_trends, aes(y=MKSlope_Overall_Score, x=slope))+geom_point()


data2<-ddply(samples,.(Site.Code),mutate, mk.tau=kendallTrendTest( FSBI~ as.numeric(Year))$estimate[1])
data2<-ddply(data2,.(Site.Code),mutate, mk.slope=kendallTrendTest( FSBI~ as.numeric(Year))$estimate[2])
data2_trends<-unique(subset(data2, select=c(Site.Code, mk.slope, mk.tau))) 
# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))
data2_score_trends<-merge(KC_trends, data2_trends,  by.x="Site.Code", by.y="Site.Code")
summary(data2_score_trends$mk.slope)
# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$mk.tau)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$mk.slope, method="spearman")
ggplot(data2_score_trends, aes(y=MKSlope_Overall.Score, x=mk.slope))+geom_point()


ggsave("Bioinfer_trends_031423.png", Sediment, width=16, height=10, dpi=300)
ggsave("FSBI_trends_031423.png", FSBISediment, width=16, height=10, dpi=300)
ggsave("BSTI_trends_031423.png", BSTISediment, width=16, height=10, dpi=300)

library(lubridate)
# jdate(KC_scores)
KC_scores$jdate<-yday(as.Date(KC_scores$Visit.Date, '%Y-%m-%d'))


l=unique(c(as.character(KC_scores$Site.Code)))
KC_scores$Site.Number<-as.numeric(factor(KC_scores$Site.Code, levels=l))
with(KC_scores, rkt(date = Year, y = jdate, block = Site.Number, correct = FALSE, rep="a"))
cor.test(KC_scores$Overall.Score, KC_scores$jdate, method="spearman")
# data2<- ddply(KC_scores, .(Site.Code), mutate, slope =lm(jdate ~ as.numeric(Year))[[1]][2])
data2<-ddply(KC_scores,.(Site.Code),mutate, mk.tau=kendallTrendTest( jdate~ as.numeric(Year))$estimate[1])
data2<-ddply(data2,.(Site.Code),mutate, mk.slope=kendallTrendTest( jdate~ as.numeric(Year))$estimate[2])
data2_trends<-unique(subset(data2, select=c(Site.Code, mk.slope, mk.tau))) 
# KC_trends<-read.csv("Site_Trends_coarse_LR_01182023_2002-2021.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))
data2_score_trends<-merge(KC_trends, data2_trends,  by.x="Site.Code", by.y="Site.Code")
summary(data2_score_trends$mk.slope)
# cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$mk.tau)
cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$mk.slope, method="spearman")
ggplot(data2_score_trends, aes(y=MKSlope_Overall.Score, x=mk.slope))+geom_point()



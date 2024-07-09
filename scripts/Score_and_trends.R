# library(devtools) 
# install_github("leppott/BioMonTools", force=TRUE, build_vignettes=FALSE)


library(plyr)
library(openxlsx)
library(lubridate)
library(stringr)
library(tidyverse)
library(dplyr)
library(here)
getwd()
# setwd("G:/GreenWQA/Biota/BenthosProjects_KC/trends analysis/2021 trends analysis")
setwd("./inputs")

raw<-read.csv("Raw_taxa_data_2002-2022.csv")
LRexclude<-openxlsx::read.xlsx("Large_Rare_to_exclude_2023_03_09.xlsx", sheet=2, detectDates=T)

LRexclude[LRexclude$YEAR<=2015&LRexclude$YEAR>=2014,"Sample_PSSB_Site_ID"]<-LRexclude[LRexclude$YEAR<=2015&LRexclude$YEAR>=2014,"Sample_Station_Name"]
LRexclude[LRexclude$YEAR<=2013,"Sample_PSSB_Site_ID"]<-LRexclude[LRexclude$YEAR<=2013,"Sample_Client_ID"]

raw$helper<-paste0(raw$Sample.Code, raw$Taxon)
LRexclude$helper<-paste0(LRexclude$Sample_PSSB_Site_ID, LRexclude$Taxon)

raw<-merge(raw, subset(LRexclude, select=c(helper, Count)), by="helper", all.x = T)
missing<-LRexclude[!LRexclude$helper %in% raw$helper, "YEAR"]#these missing are all from 2022

length(raw[!is.na(raw$Count),"Quantity"])

raw[!is.na(raw$Count),"Quantity"]<-raw[!is.na(raw$Count),"Quantity"]-raw[!is.na(raw$Count),"Count"]
nrow(subset(raw, Quantity==0))
raw<-subset(raw, Quantity!=0)


lookup<-openxlsx::read.xlsx("Official_Taxon_OTU_Conversion_Table_2021_12_14.xlsx", detectDates = T)
OTU<-merge(raw, subset(lookup, select=c(Taxon, Recommended_OTU)), by="Taxon", all.x=T)
OTU[which(is.na(OTU$Recommended_OTU)),]
colnames(OTU)[ncol(OTU)]<-"OTU"
unique(OTU[which(is.na(OTU$OTU)), "Taxon"])

##Update Serratella based on email communication with Sean Sullivan of Rhithron, September 9, 2020  
OTU[OTU$Taxon=="Serratella"&(OTU$Sample.Code=="08ISS3962_18"),"OTU"]<-c("Serratella micheneri")
OTU[OTU$Taxon=="Serratella"&(OTU$Sample.Code=="08LIT2585/08"),"OTU"]<-c("Ephemerella tibialis")
OTU[OTU$Taxon=="Ephemerella"&(OTU$Sample.Code=="08LIT2585/08"),"OTU"]<-c("Ephemerella excrucians group")
OTU[OTU$Taxon=="Serratella"&(OTU$Sample.Code=="09NEW1657/08"),"OTU"]<-c("Ephemerella tibialis")

OTU[OTU$Taxon=="Serratella"&(OTU$Sample.Code=="08ISS3962_18"|OTU$Sample.Code=="08LIT2585/08"|OTU$Sample.Code=="09NEW1657/08"),"Site.Code"]
OTU[OTU$Taxon=="Serratella"&(OTU$Sample.Code=="08ISS3962_18"|OTU$Sample.Code=="08LIT2585/08"|OTU$Sample.Code=="09NEW1657/08"),"OTU"]



OTU$Visit.Date<-format(as.Date(OTU$Visit.Date, "%Y-%m-%d"))
OTU$Year<-year(OTU$Visit.Date)
duration<-ddply(OTU, .(Site.Code), summarize, nyears=length(unique(Year)))
selectsites<-subset(duration, nyears>=10)$Site.Code
# write.csv(selectsites, "trendsites.csv")
OTU<-subset(OTU, Site.Code %in% selectsites)

# missing<-OTU[which(is.na(OTU$OTU)),c("Site.Code", "Taxon", "Year")]
# write.csv(missing, "missing_OTU.csv")

##psychodini to Psychodidae
##Nectoporus, Ilybius will get rolled to dysticidae
#Pediciidae to Tipulidae (S.l.)
OTU[OTU$Taxon=="Psychodini","OTU"]<-"Psychodidae"
OTU[OTU$Taxon=="Nectoporus","OTU"]<-"Dytiscidae"
OTU[OTU$Taxon=="Ilybius","OTU"]<-"Dytiscidae"
OTU[OTU$Taxon=="Pediciidae","OTU"]<-"Tipulidae s.l."

OTU$Unique<-as.logical(OTU$Unique)

OTU_collapsed<-ddply(OTU, .(Sample.Code, OTU, WRIA.Number, Basin, Subbasin, Stream.or.River, Project, Visit.Date, Year, Latitude, Longitude, Lab.Name, Site.Code), summarize, Quantity_OTU = sum(Quantity), Unique_OTU=any(Unique))
OTU_collapsed$Visit.Date<-as.Date(OTU_collapsed$Visit.Date, "%Y-%m-%d")

# missing<-OTU[which(is.na(OTU$OTU)),c("Site.Code", "Taxon", "Year")]
# OTU_collapsed[which(is.na(OTU_collapsed$OTU)),c("Site.Code", "Year")]
# write.csv(missing, "missing_OTU.csv")

##psychodini to Psychodidae
##Nectoporus, Ilybius will get rolled to dysticidae
#Pediciidae to Tipulidae (S.l.)



##Append correct hierarchy by matching OTU to Taxon in the lookup table. Separate out and remove taxa that don't have matching hierarchy, these will get added back further down
OTU_collapsed3<-merge(OTU_collapsed, unique(lookup[,c(1, 13:34)]), by.x="OTU", by.y="Taxon", all.x=T)
OTU_collapsed4<-OTU_collapsed3[which(is.na(OTU_collapsed3$Phylum)),][,1:15]
OTU_collapsed3<-subset(OTU_collapsed3, !is.na(Phylum))

##Where there was no matching Taxon for an OTU, append the mostly correct hierarchy by matching OTU to OTU in the lookup table.
##The hierarchies in the lookup table reflect the Taxon, not the recommended OTUs. So these will need manual correcting further in the code.
lookup$OTU<-lookup$Recommended_OTU
OTU_collapsed4<-join(OTU_collapsed4, unique(lookup[,c(13:35)]), by="OTU", type="left", match="first")


fix<-unique(OTU_collapsed4$OTU)##These are the taxa that need to be checked and corrected
subset(OTU_collapsed4, OTU==fix[1])[,16:37]
subset(OTU_collapsed4, OTU==fix[2])[,16:37]
subset(OTU_collapsed4, OTU==fix[3])[,16:37]##fix
OTU_collapsed4[which(OTU_collapsed4$OTU==fix[3]),"Species"]<-"Baetis alius group"
subset(OTU_collapsed4, OTU==fix[4])[,16:37]##add species group
OTU_collapsed4[which(OTU_collapsed4$OTU==fix[4]),"Species"]<-"Baetis fuscatus group"
subset(OTU_collapsed4, OTU==fix[5])[,16:37]##fix
OTU_collapsed4[which(OTU_collapsed4$OTU==fix[5]),"Genus"]<-"Dasyhelea"
subset(OTU_collapsed4, OTU==fix[6])[,16:37]##fix
OTU_collapsed4[which(OTU_collapsed4$OTU==fix[6]),"Species"]<-"Drunella grandis group"
subset(OTU_collapsed4, OTU==fix[7])[,16:37]##fix
OTU_collapsed4[which(OTU_collapsed4$OTU==fix[7]),"Genus"]<-"Neaviperla/ Suwallia"
subset(OTU_collapsed4, OTU==fix[8])[,16:37]##fix
OTU_collapsed4[which(OTU_collapsed4$OTU==fix[8]),"Genus"]<-"Paraleptophlebia/ Neoleptophlebia"
subset(OTU_collapsed4, OTU==fix[9])[,16:37]##fix
OTU_collapsed4[which(OTU_collapsed4$OTU==fix[9]),"Genus"]<-"Polycentropus s.l."
subset(OTU_collapsed4, OTU==fix[10])[,16:37]##fix
OTU_collapsed4[which(OTU_collapsed4$OTU==fix[10]),c("Subclass", "Order", "Suborder", "Family")]<-c("NA", "NA", "NA", "NA")
subset(OTU_collapsed4, OTU==fix[11])[,16:37]##fix
OTU_collapsed4[which(OTU_collapsed4$OTU==fix[11]),c("Subfamily", "Genus")]<-c("NA", "NA")
subset(OTU_collapsed4, OTU==fix[12])[,16:37]
OTU_collapsed4[which(OTU_collapsed4$OTU==fix[12]),"Family"]
OTU_collapsed4[which(OTU_collapsed4$OTU==fix[12]),c(28:37)]<-c("", "","","","","","","","","")
subset(OTU_collapsed4, OTU==fix[13])[,16:37]## 2021 taxa

OTU_collapsed<-rbind(OTU_collapsed3, OTU_collapsed4)
# write.csv(OTU_collapsed, "OTU_collapsed_2002-2020.csv")##used to verify that output matches that of original manuscript effort OTU_collapsed.xlsx. Only differences found were old output did not include 2020 and 2021 data, and new output doesn't incude data from 09SOO1209 prior to 2010 (older samples flagged in PSSB to not display?)

##changes to hierarchy per conversation with Sean Sullivan of Rithron 12/14/2021
OTU_collapsed[which(OTU_collapsed$Family=="Pisidiidae"),"Family"]<-"Sphaeriidae"
# OTU_collapsed[which(OTU_collapsed$OTU=="Uenoidae"), "Family"]<-"Uenoidae s.l."
# OTU_collapsed[which(OTU_collapsed$OTU=="Thremmatidae"), "Family"]<-"Uenoidae s.l."
OTU_collapsed[which(OTU_collapsed$Family=="Uenoidae"), "Family"]<-"Uenoidae s.l."
OTU_collapsed[which(OTU_collapsed$Family=="Thremmatidae"), "Family"]<-"Uenoidae s.l."
OTU_collapsed[which(OTU_collapsed$OTU=="Uenoidae"), "OTU"]<-"Uenoidae s.l."
OTU_collapsed[which(OTU_collapsed$OTU=="Thremmatidae"), "OTU"]<-"Uenoidae s.l."
OTU_collapsed[which(OTU_collapsed$Family=="Uenoidae s.l."), ]
##Anywhere Para/Neo is flagged as not unique because of child taxa bicornuta also present in sample, change flag to unique, per S. Sullivan 12/14/2021
ParaNeo_sites<-OTU_collapsed[which(OTU_collapsed$OTU=="Paraleptophlebia/ Neoleptophlebia"& OTU_collapsed$Unique_OTU==F),]$Sample.Code
ParaNeo<-OTU_collapsed[which(OTU_collapsed$Sample.Code %in% ParaNeo_sites & (OTU_collapsed$Family=="Leptophlebiidae"|OTU_collapsed$Family=="Paraleptophlebia")),c("Sample.Code","OTU", "Family")]
OTU_collapsed[which(OTU_collapsed$Sample.Code == "08BEA3650/06"),][,c(1, 2, 15:37)]
OTU_collapsed[which(OTU_collapsed$OTU=="Paraleptophlebia/ Neoleptophlebia"& OTU_collapsed$Unique_OTU==F),"Unique_OTU"]<-TRUE


##Add attributes from Lookup table. Sean reviewed most of these according to OTU.
OTU_collapsed<-join(OTU_collapsed, unique(lookup[,c(7:11,35)]), by="OTU", type="left", match="first")
unique(OTU_collapsed[which(is.na(OTU_collapsed$`2012.Clinger`)),][,1:15]$OTU)##These make sense, are all NA in the lookup table too

#assign unique
OTU_collapsed$OTU_Unique2<-paste0(OTU_collapsed$OTU, ifelse(OTU_collapsed$Unique_OTU, "_UNIQUE", ""))

##Add Sean Suggested Edits-- manual edits are consistent with the excel document listed below
# SSedits<-read.xlsx("G:/GreenWQA/Biota/BenthosProjects_KC/trends analysis/Data_for_Trends_FWS_manuscript/Copy of Copy of OTU_collpased_for_Sean_to_review_SS_EDIT.xlsx", detectDates = T)
OTU_collapsed$SSedits<-OTU_collapsed$OTU_Unique2
OTU_collapsed[which(OTU_collapsed$OTU_Unique2=="Baetidae_UNIQUE"), "SSedits"]<-"Anafroptilum_UNIQUE"
OTU_collapsed[which(OTU_collapsed$OTU_Unique2=="Baetis_UNIQUE"), "SSedits"]<-"Baetis rhodani group_UNIQUE"
OTU_collapsed[which(OTU_collapsed$OTU_Unique2=="Drunella_UNIQUE"), "SSedits"]<-"Drunella grandis group_UNIQUE"
OTU_collapsed[which(OTU_collapsed$OTU_Unique2=="Drunella_UNIQUE"&OTU_collapsed$Sample.Code=="08CED4192_18"), "SSedits"]<-"Drunella flavilinea/coloradensis_UNIQUE"
OTU_collapsed[which(OTU_collapsed$OTU_Unique2=="Ephemera_UNIQUE"), "SSedits"]<-"Ephemerella excrucians group_UNIQUE"
OTU_collapsed[which(OTU_collapsed$OTU_Unique2=="Ephemerella_UNIQUE"), "SSedits"]<-"Ephemerella excrucians group_UNIQUE"
OTU_collapsed[which(OTU_collapsed$OTU_Unique2=="Ephemerellidae_UNIQUE"&(OTU_collapsed$Sample.Code=="09BLA0675_19_R"|OTU_collapsed$Sample.Code=="09BLA0675_14")), "SSedits"]<-"Caudatella_UNIQUE"
OTU_collapsed[which(OTU_collapsed$OTU_Unique2=="Glossiphoniidae_UNIQUE"&OTU_collapsed$Sample.Code=="08WES1178/10"), "SSedits"]<-"Helobdella stagnalis_UNIQUE"
OTU_collapsed[which(OTU_collapsed$OTU_Unique2=="Peltoperlidae_UNIQUE"), "SSedits"]<-"Yoraperla_UNIQUE"
OTU_collapsed[which(OTU_collapsed$OTU_Unique2=="Serratella levis_UNIQUE"), "SSedits"]<-"Serratella levis_micheneri_UNIQUE"
OTU_collapsed[which(OTU_collapsed$OTU_Unique2=="Serratella micheneri_UNIQUE"), "SSedits"]<-"Serratella levis_micheneri_UNIQUE"

OTU_collapsed[which(OTU_collapsed$SSedits=="Anafroptilum_UNIQUE"),]$Family<-"Baetidae"
OTU_collapsed[which(OTU_collapsed$SSedits=="Anafroptilum_UNIQUE"),]$Genus<-"Anafroptilum_UNIQUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Anafroptilum_UNIQUE"),]$`2012.Clinger`<-"TRUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Caudatella_UNIQUE"),]$Family<-"Ephemerellidae"
OTU_collapsed[which(OTU_collapsed$SSedits=="Caudatella_UNIQUE"),]$Genus<-"Caudatella_UNIQUE"

OTU_collapsed[which(OTU_collapsed$SSedits=="Drunella flavilinea/coloradensis_UNIQUE"),]$Genus<-"Drunella"
OTU_collapsed[which(OTU_collapsed$SSedits=="Drunella flavilinea/coloradensis_UNIQUE"),]$Species.Group<-"Drunella flavilinea/coloradensis_UNIQUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Drunella flavilinea/coloradensis_UNIQUE"),]$Species<-"Drunella flavilinea/coloradensis_UNIQUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Drunella flavilinea/coloradensis_UNIQUE"),]$`2012.Clinger`<-"TRUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Drunella flavilinea/coloradensis_UNIQUE"),]$`2012.Intolerant`<-"FALSE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Ephemerella excrucians group_UNIQUE"),]$Infraorder<-"Pannota"
OTU_collapsed[which(OTU_collapsed$SSedits=="Ephemerella excrucians group_UNIQUE"),]$Superfamily<-"Ephemerelloidea"
OTU_collapsed[which(OTU_collapsed$SSedits=="Ephemerella excrucians group_UNIQUE"),]$Family<-"Ephemerellidae"
OTU_collapsed[which(OTU_collapsed$SSedits=="Ephemerella excrucians group_UNIQUE"),]$Genus<-"Ephemerella"
OTU_collapsed[which(OTU_collapsed$SSedits=="Ephemerella excrucians group_UNIQUE"),]$Species.Group<-"Ephemerella excrucians group_UNIQUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Ephemerella excrucians group_UNIQUE"),]$Species<-"Ephemerella excrucians group_UNIQUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Ephemerella excrucians group_UNIQUE"),]$`2012.Clinger`<-"TRUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Helobdella stagnalis_UNIQUE"),]$Family<-"Glossiphoniidae"
OTU_collapsed[which(OTU_collapsed$SSedits=="Helobdella stagnalis_UNIQUE"),]$Subfamily<-"Haementeriinae"
OTU_collapsed[which(OTU_collapsed$SSedits=="Helobdella stagnalis_UNIQUE"),]$Genus<-"Helobdella"
OTU_collapsed[which(OTU_collapsed$SSedits=="Helobdella stagnalis_UNIQUE"),]$Species<-"Helobdella stagnalis_UNIQUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Helobdella stagnalis_UNIQUE"),]$`2012.Long.Lived`<-"FALSE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Helobdella stagnalis_UNIQUE"),]$`2012.Predator`<-"TRUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Oligochaeta_UNIQUE"),]$Subclass<-"Oligochaeta_UNIQUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Serratella levis_micheneri_UNIQUE"),]$Species<-"Serratella levis_micheneri_UNIQUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Serratella levis_micheneri_UNIQUE"),]$`2012.Clinger`<-"TRUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Yoraperla_UNIQUE"),]$Family<-"Peltoperlidae"
OTU_collapsed[which(OTU_collapsed$SSedits=="Yoraperla_UNIQUE"),]$Subfamily<-"Peltoperlinae"
OTU_collapsed[which(OTU_collapsed$SSedits=="Yoraperla_UNIQUE"),]$Genus<-"Yoraperla_UNIQUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Yoraperla_UNIQUE"),]$`2012.Intolerant`<-"TRUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Yoraperla_UNIQUE"),]$`2012.Long.Lived`<-"TRUE"

OTU_collapsed[which(OTU_collapsed$SSedits=="Baetis rhodani group_UNIQUE"),]$Species<-"Baetis rhodani group_UNIQUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Baetis rhodani group_UNIQUE"),]$Species.Group<-"Baetis rhodani group_UNIQUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Drunella grandis group_UNIQUE"),]$Species<-"Drunella grandis group_UNIQUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Drunella grandis group_UNIQUE"),]$Species.Group<-"Drunella grandis group_UNIQUE"
OTU_collapsed[which(OTU_collapsed$SSedits=="Drunella grandis group_UNIQUE"),]$`2012.Predator`<-"TRUE"



# write.csv(OTU_collapsed, "OTU_collapsed_corrected_2002-2020.csv")##matches.##check that this matches SSedits of original output. Should match all except Uenoidae, Tipulidae, Drunella grandis group (spelling change), Baetis rhodani group (added UNIQUE) and Neoleptophlebia/paraleptophlebia.
# nrow(unique((OTU_collapsed[, c("Sample.Code", "SSedits")])))
# OTU_collapsed[which(duplicated(OTU_collapsed[, c("Sample.Code", "SSedits")])),]
# 
# OTU_collapsed[OTU_collapsed$Sample.Code=="09LOW0753_13", ]




###################################### Roll-up by broad rules ##########
KC_OTU<-OTU_collapsed

# names(KC_OTU)[4]<-"OTU_Unique"
# names(KC_OTU)[19]<-"Unique_OTU2"
exclude<-read.csv("exclusions.csv")
exclude<-subset(exclude, Excluded==TRUE)
KC_OTU$Exclude<-"FALSE" ##do not run for taxa trends
KC_OTU[which(str_replace(KC_OTU$SSedits, "_UNIQUE", "") %in% exclude$Taxon.Name ),]$Exclude<-"TRUE" # don't run for taxa trends 
KC_taxa_coarse<-KC_OTU
KC_taxa_coarse$OTU_COARSE<-""

names(KC_taxa_coarse)
KC_taxa_coarse[which(KC_taxa_coarse$Subclass=="Oligochaeta"),]$OTU_COARSE<-"Oligochaeta"
KC_taxa_coarse[which(KC_taxa_coarse$Subclass=="Oligochaeta_UNIQUE"),]$OTU_COARSE<-"Oligochaeta"
KC_taxa_coarse[which(KC_taxa_coarse$Subclass=="Oligochaeta"),][21:37]<-"" 
KC_taxa_coarse[which(KC_taxa_coarse$Subclass=="Oligochaeta_UNIQUE"),][21:37]<-""

KC_taxa_coarse[which(KC_taxa_coarse$Class=="Gastropoda"),][28:37]<-""
KC_taxa_coarse[which(KC_taxa_coarse$Class=="Gastropoda_UNIQUE"),][28:37]<-""
KC_taxa_coarse[which(KC_taxa_coarse$Class=="Gastropoda"),]$OTU_COARSE<-paste0(KC_taxa_coarse[which(KC_taxa_coarse$Class=="Gastropoda"),]$Family)
KC_taxa_coarse[which(KC_taxa_coarse$Class=="Gastropoda_UNIQUE"),]$OTU_COARSE<-paste0(KC_taxa_coarse[which(KC_taxa_coarse$Class=="Gastropoda_UNIQUE"),]$Class)
KC_taxa_coarse[which(KC_taxa_coarse$OTU_COARSE=="Hydrobiidae s.l."),]$Order<-"Neotaenioglossa"
KC_taxa_coarse[which(KC_taxa_coarse$Class=="Gastropoda"&KC_taxa_coarse$OTU=="Gastropoda"),]$OTU_COARSE<-"Gastropoda"

# KC_taxa_coarse[which(KC_taxa_coarse$Order=="Trichoptera_UNIQUE") ,]
KC_taxa_coarse[which(KC_taxa_coarse$Order=="Trichoptera" & KC_taxa_coarse$Genus!="0") ,]$OTU_COARSE<-  paste0(KC_taxa_coarse[which(KC_taxa_coarse$Order=="Trichoptera" & KC_taxa_coarse$Genus!="0"),]$Genus )
KC_taxa_coarse[which(KC_taxa_coarse$Order=="Trichoptera" & KC_taxa_coarse$Genus=="0"),]$OTU_COARSE<-  paste0(KC_taxa_coarse[which(KC_taxa_coarse$Order=="Trichoptera" & KC_taxa_coarse$Genus=="0"),]$Family )
KC_taxa_coarse[which(KC_taxa_coarse$Order=="Trichoptera" & KC_taxa_coarse$Family=="0") ,]$OTU_COARSE<-  paste0(KC_taxa_coarse[which(KC_taxa_coarse$Order=="Trichoptera" & KC_taxa_coarse$Family=="0"),]$Superfamily)
KC_taxa_coarse[which(KC_taxa_coarse$Order=="Trichoptera" & KC_taxa_coarse$OTU_COARSE=="0"),]$OTU_COARSE<-  paste0(KC_taxa_coarse[which(KC_taxa_coarse$Order=="Trichoptera" & KC_taxa_coarse$OTU_COARSE=="0"),]$Order)
KC_taxa_coarse[which(KC_taxa_coarse$Order=="Trichoptera_UNIQUE"),]$OTU_COARSE<-  paste0(KC_taxa_coarse[which(KC_taxa_coarse$Order=="Trichoptera_UNIQUE"),]$Order )
KC_taxa_coarse[which(KC_taxa_coarse$Order=="Trichoptera"),][34:37]<-""
KC_taxa_coarse[which(KC_taxa_coarse$Order=="Trichoptera_UNIQUE"),][34:37]<-""

KC_taxa_coarse$SSedits<-str_replace(KC_taxa_coarse$SSedits, "_UNIQUE", "")
KC_taxa_coarse[KC_taxa_coarse$OTU_COARSE=="",]$OTU_COARSE<-  paste0(KC_taxa_coarse[KC_taxa_coarse$OTU_COARSE=="",]$SSedits)
KC_taxa_coarse$OTU_COARSE<-str_replace(KC_taxa_coarse$OTU_COARSE, "_UNIQUE", "")
KC_taxa_coarse[is.na(KC_taxa_coarse)]<-""

unique(KC_taxa_coarse[which(KC_taxa_coarse$Family=="Dytiscidae") ,]$OTU_COARSE)
unique(KC_taxa_coarse[which(KC_taxa_coarse$Family=="Simuliidae") ,]$OTU_COARSE)
unique(KC_taxa_coarse[which(KC_taxa_coarse$Family=="Chironomidae") ,]$OTU_COARSE)
unique(KC_taxa_coarse[which(KC_taxa_coarse$Subclass=="Acari") ,]$OTU_COARSE)

KC_taxa_coarse[which(KC_taxa_coarse$OTU_COARSE=="NA"),] ###Vashon sites, not important for trends but need to figure out what to do with these guys at some point.

KC_taxa_coarse_2<-KC_taxa_coarse %>% #don't use for taxa analysis
  mutate_at(
    .vars = vars(names(KC_taxa_coarse[,c(16:37)])),
    .funs = ~ str_replace(., pattern = "_UNIQUE", replacement = "")
  )

KC_taxa_coarse_2<-KC_taxa_coarse_2 %>%
  mutate_at(
    .vars = vars(names(KC_taxa_coarse[,c(16:37)])), #20:46 use for taxa analysis
    .funs = ~ str_replace(., pattern = "0", replacement = "")
  )



hier<-subset(KC_taxa_coarse_2, OTU_COARSE==SSedits|OTU_COARSE=="Physidae"|OTU_COARSE=="Goera"|OTU_COARSE=="Ecclisocosmoecus"|OTU_COARSE=="Hydrobiidae s.l."|OTU_COARSE=="Ancylidae"|OTU_COARSE=="Pleuroceridae s.l."|OTU_COARSE=="Onocosmoecus"|OTU_COARSE=="Heteroplectron"|OTU_COARSE=="Allocosmoecus"|OTU_COARSE=="Amphicosmoecus")#these exceptions are all either gastropods or trichopterans that get rolled up more coarsely than Sean's suggestions.
hier<-unique(hier[,c(16:42,46)])
missing<-unique(KC_taxa_coarse_2$OTU_COARSE)[!unique(KC_taxa_coarse_2$OTU_COARSE) %in% hier$OTU_COARSE]
dup_hier<-hier[duplicated(hier[,28]),]
dup_hier2<-hier[duplicated(hier[,28], fromLast = TRUE),]
dup_hier<-rbind(dup_hier, dup_hier2)##these duplicates  result from Sean's bench notes renaming OTUs but not updating the hierarchy
KC_taxa_coarse_2[KC_taxa_coarse_2$OTU_COARSE=="Ephemerella excrucians group"&KC_taxa_coarse_2$`2012.Intolerant`=="",c("2012.Intolerant", "2012.Long.Lived", "2012.Predator", "2012.Tolerant")]<-c(FALSE, FALSE, FALSE, FALSE)
KC_taxa_coarse_2[KC_taxa_coarse_2$OTU_COARSE=="Serratella levis_micheneri"&KC_taxa_coarse_2$`2012.Intolerant`=="",c("2012.Intolerant", "2012.Long.Lived", "2012.Predator", "2012.Tolerant")]<-c(FALSE, FALSE, FALSE, FALSE)
hier<-subset(KC_taxa_coarse_2, OTU_COARSE==SSedits|OTU_COARSE=="Physidae"|OTU_COARSE=="Goera"|OTU_COARSE=="Ecclisocosmoecus"|OTU_COARSE=="Hydrobiidae s.l."|OTU_COARSE=="Ancylidae"|OTU_COARSE=="Pleuroceridae s.l."|OTU_COARSE=="Onocosmoecus"|OTU_COARSE=="Heteroplectron"|OTU_COARSE=="Allocosmoecus"|OTU_COARSE=="Amphicosmoecus")#these exceptions are all either gastropods or trichopterans that get rolled up more coarsely than Sean's suggestions.
hier<-unique(hier[,c(16:42,46)])


library(plyr)
OTU_collapsed2<-ddply(KC_taxa_coarse_2, .(Sample.Code, OTU_COARSE,WRIA.Number, Basin, 
                                          Subbasin, Stream.or.River, Project, Visit.Date, 
                                          Year, Latitude, Longitude, Lab.Name, Site.Code
), summarize, Quantity_OTU = sum(Quantity_OTU), Unique_OTU=any(Unique_OTU))

OTU_collapsed3<-left_join(OTU_collapsed2, hier, by=c("OTU_COARSE"="OTU_COARSE"))



OTU_collapsed4<-left_join(OTU_collapsed3, unique(raw[,c("Sample.Code", "Visit.ID")]), by="Sample.Code")
setwd(here::here())
setwd("./outputs/")
write.csv(OTU_collapsed4, "Collapsed_Coarse_Taxa_LR_exclude.csv")
OTU_collapsed4<-read.csv( "Collapsed_Coarse_Taxa_LR_exclude.csv")

OTU_collapsed5<-ddply(OTU_collapsed4, .(OTU_COARSE,WRIA.Number, Basin, 
                                          Subbasin, Stream.or.River, Project, Visit.Date, 
                                          Year, Latitude, Longitude, Lab.Name, Site.Code, Visit.ID
), summarize, Quantity_OTU = sum(Quantity_OTU), Unique_OTU=any(Unique_OTU))

OTU_collapsed5<-left_join(OTU_collapsed5, hier, by=c("OTU_COARSE"="OTU_COARSE"))
write.csv(OTU_collapsed5,"Collapsed_Coarse_Taxa_reps_wrapped_in_LR_exclude.csv")

reps<-ddply(OTU_collapsed4, .(Visit.ID, Site.Code, Year, Project), summarize, reps=length(unique(Sample.Code)))
reps<-subset(reps, reps>1&Project!="Regulatory Effectiveness")
write.csv(reps, "replicates_for_sqft_analysis.csv")

library(rkt)
ambig<-ddply(OTU_collapsed4, .(Sample.Code, Year, Site.Code), summarize, ambig_taxa=sum(!Unique_OTU))
ambig <- droplevels(ambig[ambig$Year > 2001,]) # subset to sites since 2002
ambig <- ddply(ambig, 'Site.Code', mutate, por = length(ambig_taxa)) # add on period of record col
ambig<-droplevels(ambig[ambig$por > 9,])# look only at sites with >9 years of data
ambig<-subset(ambig, ambig$Site.Code!="09SOO1209")
ambig <- ddply(ambig, 'Site.Code', mutate, timerange = (max(Year)-min(Year))) # add on period of record col
ambig<-droplevels(ambig[ambig$timerange > 9,])# look only at streams with >9 years of data
ggplot(ambig, aes(x=Year, group=Year, y=ambig_taxa))+geom_boxplot()
l=unique(c(as.character(ambig$Site.Code)))
ambig$Site.Number<-as.numeric(factor(ambig$Site.Code, levels=l))
with(ambig, rkt(date = as.numeric(Year), y = ambig_taxa, block = Site.Number, correct = FALSE, rep="a"))

samplecounts<-ddply(OTU_collapsed4, .(Sample.Code, Year, Site.Code), summarize, sumOrgs=sum(Quantity_OTU))
samplecounts$above500<-ifelse(samplecounts$sumOrgs>=500, TRUE, FALSE)
samplecounts <- droplevels(samplecounts[samplecounts$Year > 2001,]) # subset to sites since 2002
samplecounts <- ddply(samplecounts, 'Site.Code', mutate, por = length(sumOrgs)) # add on period of record col
samplecounts<-droplevels(samplecounts[samplecounts$por > 9,])# look only at sites with >9 years of data
samplecounts<-subset(samplecounts, samplecounts$Site.Code!="09SOO1209")
samplecounts <- ddply(samplecounts, 'Site.Code', mutate, timerange = (max(Year)-min(Year))) # add on period of record col
samplecounts<-droplevels(samplecounts[samplecounts$timerange > 9,])# look only at streams with >9 years of data
l=unique(c(as.character(samplecounts$Site.Code)))
samplecounts$Site.Number<-as.numeric(factor(samplecounts$Site.Code, levels=l))
with(samplecounts, rkt(date = as.numeric(Year), y = sumOrgs, block = Site.Number, correct = FALSE, rep="a"))
library(EnvStats)
samplecounts<-ddply(samplecounts, .(Year), summarize, sum_abov500=sum(above500))
kendallTrendTest(sum_abov500~Year, data=samplecounts)




# setwd("G:/GreenWQA/Biota/BenthosProjects_KC/trends analysis/2021 trends analysis")
OTU_collapsed4<-read.csv( "Collapsed_Coarse_Taxa_LR_exclude.csv")
getwd()

KC_rarified<-OTU_collapsed4
KC_rarified<-KC_rarified[0,]
set.seed(17760704, "Mersenne-Twister",normal.kind = "Inversion",  sample.kind="Rounding")
for(visit in unique(OTU_collapsed4$Sample.Code)){
  print(visit)
  test<-subset(OTU_collapsed4, Sample.Code==visit)
  testsample<-rep(test$OTU_COARSE, test$Quantity_OTU)
  if(sum(test$Quantity_OTU)>=450){
    subsamp<-sample(x = testsample, size = 450,replace=F)
  }
  else {subsamp<-sample(x = testsample, size = sum(test$Quantity_OTU),replace=F)}
  subsamp<-as.data.frame(table(subsamp))
  names(subsamp)<-c("OTU_COARSE","Quantity_OTU")
  subsamp_meta<-test[c(match(subsamp$OTU_COARSE, test$OTU_COARSE)),]
  subsamp_meta<-subset(subsamp_meta, select=-c(Quantity_OTU))
  subsamp_comp<-merge(subsamp_meta, subsamp, by="OTU_COARSE")
  KC_rarified<-rbind(subsamp_comp, KC_rarified)
}


# detach("package:reshape2", unload = TRUE)
detach("package:plyr", unload = TRUE)
# library(dplyr)

Tot_Richness<-KC_rarified %>% dplyr::filter(Unique_OTU==TRUE)  %>% dplyr::group_by(Sample.Code) %>% dplyr::summarise(Total_Richness=length(OTU_COARSE))
E_Richness<-KC_rarified %>% dplyr::filter(Order=="Ephemeroptera", Unique_OTU==TRUE)  %>% dplyr::group_by(Sample.Code) %>% dplyr::summarise(E_Richness=length(OTU_COARSE))
P_Richness<-KC_rarified %>% dplyr::filter(Order=="Plecoptera", Unique_OTU==TRUE)  %>% dplyr::group_by(Sample.Code) %>% dplyr::summarise(P_Richness=length(OTU_COARSE))
T_Richness<-KC_rarified %>% dplyr::filter(Order=="Trichoptera", Unique_OTU==TRUE)  %>% dplyr::group_by(Sample.Code) %>% dplyr::summarise(T_Richness=length(OTU_COARSE))
Cling_Richness<-KC_rarified %>% dplyr::filter(`X2012.Clinger`==TRUE, Unique_OTU==TRUE)  %>% dplyr::group_by(Sample.Code) %>% dplyr::summarise(Clin_Richness=length(OTU_COARSE))
LL_Richness<-KC_rarified %>% dplyr::filter(`X2012.Long.Lived`==TRUE, Unique_OTU==TRUE)  %>% dplyr::group_by(Sample.Code) %>% dplyr::summarise(LL_Richness=length(OTU_COARSE))
Intol_Richness<-KC_rarified %>% dplyr::filter(`X2012.Intolerant`==TRUE, Unique_OTU==TRUE)  %>% dplyr::group_by(Sample.Code) %>% dplyr::summarise(Intol_Richness=length(OTU_COARSE))
Tot_Abund<-KC_rarified   %>% dplyr::group_by(Sample.Code) %>% dplyr::summarise(Tot_Abund=sum(Quantity_OTU)) 
Tol_Abund<-KC_rarified   %>% dplyr::group_by(Sample.Code)%>% dplyr::filter(`X2012.Tolerant`==TRUE) %>% dplyr::summarise(Tol_Abund=sum(Quantity_OTU))
Pred_Abund<-KC_rarified   %>% dplyr::group_by(Sample.Code)%>% dplyr::filter(`X2012.Predator`==TRUE) %>% dplyr::summarise(Pred_Abund=sum(Quantity_OTU))
Dom_abund<-KC_rarified   %>% dplyr::group_by(Sample.Code) %>% dplyr::arrange(desc(Quantity_OTU)) %>% dplyr::slice(1:3) %>% dplyr::summarise(Dom_Abund=sum(Quantity_OTU))

KC_results<-Reduce(function(x, y, ...) merge(x,y, all=TRUE, ...), list(Tot_Richness, E_Richness, P_Richness, T_Richness, Cling_Richness, LL_Richness, Intol_Richness, Dom_abund,
                                                                       Tol_Abund, Pred_Abund,Tot_Abund))
KC_results<-KC_results %>% mutate(Tol_Percent=Tol_Abund/Tot_Abund*100, Pred_Percent= Pred_Abund/Tot_Abund*100, Dom_Percent= Dom_Abund/Tot_Abund*100)
KC_results[is.na(KC_results)]<-0

KC_results<-KC_results %>% mutate(Tot_Richness_Score= 10 * (Total_Richness-16)/(37-16))
KC_results<-KC_results %>% mutate(E_Richness_Score= 10 * (E_Richness-1)/(8-1))
KC_results<-KC_results %>% mutate(P_Richness_Score= 10 * (P_Richness-1)/(8-1))
KC_results<-KC_results %>% mutate(T_Richness_Score= 10 * (T_Richness-1)/(9-1))
KC_results<-KC_results %>% mutate(Cling_Richness_Score= 10 * (Clin_Richness-5)/(22-5))
KC_results<-KC_results %>% mutate(LL_Richness_Score= 10 * (LL_Richness-2)/(10-2))
KC_results<-KC_results %>% mutate(Intol_Richness_Score= 10 * (Intol_Richness-0)/(7-0))
KC_results<-KC_results %>% mutate(Dom_Percent_Score= 10-(10 * (Dom_Percent-44)/(82-44)))
KC_results<-KC_results %>% mutate(Pred_Percent_Score= 10 * (Pred_Percent-1)/(21-1))
KC_results<-KC_results %>% mutate(Tol_Percent_Score= 10-(10 * (Tol_Percent-0)/(43-0)))

KC_results[which(KC_results$Tot_Richness_Score>10),"Tot_Richness_Score"]<-10
KC_results[which(KC_results$Tot_Richness_Score<0),"Tot_Richness_Score"]<-0
KC_results[which(KC_results$E_Richness_Score>10),"E_Richness_Score"]<-10
KC_results[which(KC_results$E_Richness_Score<0),"E_Richness_Score"]<-0
KC_results[which(KC_results$P_Richness_Score>10),"P_Richness_Score"]<-10
KC_results[which(KC_results$P_Richness_Score<0),"P_Richness_Score"]<-0
KC_results[which(KC_results$T_Richness_Score>10),"T_Richness_Score"]<-10
KC_results[which(KC_results$T_Richness_Score<0),"T_Richness_Score"]<-0
KC_results[which(KC_results$Cling_Richness_Score>10),"Cling_Richness_Score"]<-10
KC_results[which(KC_results$Cling_Richness_Score<0),"Cling_Richness_Score"]<-0
KC_results[which(KC_results$LL_Richness_Score>10),"LL_Richness_Score"]<-10
KC_results[which(KC_results$LL_Richness_Score<0),"LL_Richness_Score"]<-0
KC_results[which(KC_results$Intol_Richness_Score>10),"Intol_Richness_Score"]<-10
KC_results[which(KC_results$Intol_Richness_Score<0),"Intol_Richness_Score"]<-0
KC_results[which(KC_results$Dom_Percent_Score>10),"Dom_Percent_Score"]<-10
KC_results[which(KC_results$Dom_Percent_Score<0),"Dom_Percent_Score"]<-0
KC_results[which(KC_results$Pred_Percent_Score>10),"Pred_Percent_Score"]<-10
KC_results[which(KC_results$Pred_Percent_Score<0),"Pred_Percent_Score"]<-0
KC_results[which(KC_results$Tol_Percent_Score>10),"Tol_Percent_Score"]<-10
KC_results[which(KC_results$Tol_Percent_Score<0),"Tol_Percent_Score"]<-0

KC_results<-KC_results %>% mutate(Overall.Score=Tot_Richness_Score+ E_Richness_Score+P_Richness_Score+T_Richness_Score+ Cling_Richness_Score+ LL_Richness_Score+ Intol_Richness_Score+ Dom_Percent_Score+ Pred_Percent_Score+ Tol_Percent_Score)

KC_results<-left_join(KC_results, unique(subset(OTU_collapsed4, select=c("Sample.Code","WRIA.Number", "Basin", 
                                                                         "Subbasin", "Stream.or.River", "Project", "Visit.Date", 
                                                                         "Year", "Lab.Name", "Site.Code"))), by="Sample.Code")

KC_results<-left_join(KC_results, unique(subset(raw, select=c(Site.Code, Latitude, Longitude))), by="Site.Code")
KC_results[KC_results$Sample.Code=="09DUW0277/05",]$Site.Code<-"09DUW0277"
write.csv(KC_results, "B-IBI_results_03082023_2002-2021_NoUnique_subsamp_450_LR_exclude.csv")


###############


library(stringr)
library(plyr)
library(reshape2)
library(stats)
library(ggplot2)
library(ggpmisc)
library(EnvStats)
library(rkt)
library(plyr)
library(tidyverse)
####note: we've changed subsampling number to 450 and removed samples with total abundance <450 for greater statistical consistency in richness between samples over time
###need to update all stats in the report based on this
##need to check in with Sean about "large and rare" search, and if that explains our trends over time.
bibi <-read.csv("B-IBI_results_03082023_2002-2021_NoUnique_subsamp_450_LR_exclude.csv")
bibi<-subset(bibi, Project!="Regulatory Effectiveness")
bibi <- droplevels(bibi[bibi$Year > 2001,]) # subset to sites since 2002
bibi <- ddply(bibi, 'Site.Code', mutate, por = length(Overall.Score)) # add on period of record col
bibi<-droplevels(bibi[bibi$por > 9,])# look only at sites with >9 years of data
exclude<-c("09SOO1209") 
bibi<-subset(bibi, bibi$Site.Code!=exclude)
bibi <- ddply(bibi, 'Site.Code', mutate, timerange = (max(Year)-min(Year))) # add on period of record col
bibi<-droplevels(bibi[bibi$timerange > 9,])# look only at streams with >9 years of data

exlcudedsamps<-subset(bibi, Tot_Abund<450)
nrow(subset(exlcudedsamps, Project!="Regulatory Effectiveness"))
unique(exlcudedsamps$Site.Code)
bibi<-subset(bibi, Tot_Abund>=450)

# ############diagnostics for sampling conditions changing over time, skip if doing trends#######
# OTU_collapsed4<-read.csv( "Collapsed_Coarse_Taxa_LR_exclude.csv")
# samplecounts<-ddply(OTU_collapsed4, .(Sample.Code, Year, Site.Code, Visit.ID), summarize, sumOrgs=sum(Quantity_OTU))
# samplecounts$bin<-cut(samplecounts$sumOrgs, c(1, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800))
# samplecounts$above500<-ifelse(samplecounts$sumOrgs>=450, TRUE, FALSE)
# samplecounts <- droplevels(samplecounts[samplecounts$Year > 2001,]) # subset to sites since 2002
# samplecounts <- ddply(samplecounts, 'Site.Code', mutate, por = length(unique(Year))) # add on period of record col
# samplecounts<-droplevels(samplecounts[samplecounts$por > 9,])# look only at sites with >9 years of data
# exclude<-c("09SOO1209") #Site to remove from trends analysis
# samplecounts<-subset(samplecounts, samplecounts$Site.Code!=exclude)
# samplecounts <- ddply(samplecounts, 'Site.Code', mutate, timerange = (max(Year)-min(Year))) # add on period of record col
# samplecounts<-droplevels(samplecounts[samplecounts$timerange > 9,])# look only at streams with >9 years of data
# 
# counts_score<-merge(bibi, samplecounts, by=c("Sample.Code", "Site.Code", "Year"))
# counts_score <- ddply(counts_score, 'Site.Code', mutate, timerange = (max(Year)-min(Year))) # add on period of record col
# counts_score<-droplevels(counts_score[counts_score$timerange > 9,])# look only at streams with >9 years of data
# counts_score <- ddply(counts_score, 'Site.Code', mutate, por = length(unique(Year))) # add on period of record col
# counts_score<-droplevels(counts_score[counts_score$por > 9,])# look only at sites with >9 years of data
# ggplot(counts_score, aes(y=Overall.Score, x=sumOrgs))+geom_point()
# ggplot(counts_score, aes(y=Overall.Score, x=bin, group=bin))+geom_boxplot()
# ggplot(counts_score, aes(y=sumOrgs, x=Year))+geom_point()+geom_smooth()
# l=unique(c(as.character(counts_score$Site.Code)))
# counts_score$Site.Number<-as.numeric(factor(counts_score$Site.Code, levels=l))
# with(counts_score, rkt(date = as.numeric(Year), y = sumOrgs, block = Site.Number, correct = FALSE, rep="a"))
# 
# 
# samplecounts2<-ddply(counts_score, .(Year), summarize, sum_abov500=sum(above500))
# kendallTrendTest(sum_abov500~Year, data=samplecounts2)
# 
# cor.test(counts_score$Overall.Score, counts_score$sumOrgs)
# # ddply(counts_score,.(Site.Code),summarize, summ=length(sumOrgs))
# data2<-ddply(counts_score,.(Site.Code),mutate, mk.tau=kendallTrendTest( sumOrgs~ as.numeric(Year))$estimate[1])
# data2<-ddply(data2,.(Site.Code),mutate, mk.slope=kendallTrendTest( sumOrgs~ as.numeric(Year))$estimate[2])
# data2<-ddply(data2,.(Site.Code),mutate, pval=kendallTrendTest( sumOrgs~ as.numeric(Year))$p.value)
# data2<-ddply(data2,.(Site.Code),mutate, meanScore=mean(Overall.Score))
# test<-unique(subset(data2, select=c(Site.Code, pval, mk.tau, meanScore)))
# test<-test[test$pval<0.05,]
# data2_trends<-unique(subset(data2, select=c(Site.Code, mk.slope, mk.tau))) 
# KC_trends<-read.csv("Site_Trends_coarse_LR_03092023_2002-2021_LRexclude.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))
# data2_score_trends<-merge(KC_trends, data2_trends,  by.x="Site.Code", by.y="Site.Code")
# # cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$mk.tau)
# cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$mk.slope)
# 
# ##ambiguous by taxa###
# ambig<-ddply(OTU_collapsed4, .(Sample.Code, Year, Site.Code), summarize, ambig_taxa=sum(!Unique_OTU), total_taxa=length(Unique_OTU))
# ambig$per_ambig<-ambig$ambig_taxa/ambig$total_taxa
# ambig <- droplevels(ambig[ambig$Year > 2001,]) # subset to sites since 2002
# ambig <- ddply(ambig, 'Site.Code', mutate, por = length(unique(Year))) # add on period of record col
# ambig<-droplevels(ambig[ambig$por > 9,])# look only at sites with >9 years of data
# ambig<-subset(ambig, ambig$Site.Code!=exclude)
# ambig <- ddply(ambig, 'Site.Code', mutate, timerange = (max(Year)-min(Year))) # add on period of record col
# ambig<-droplevels(ambig[ambig$timerange > 9,])# look only at streams with >9 years of data
# ambig_score<-merge(bibi, ambig, by=c("Sample.Code", "Site.Code", "Year"))
# ambig_score <- ddply(ambig_score, 'Site.Code', mutate, por = length(unique(Year))) # add on period of record col
# ambig_score<-droplevels(ambig_score[ambig_score$por > 9,])# look only at sites with >9 years of data
# ambig_score <- ddply(ambig_score, 'Site.Code', mutate, timerange = (max(Year)-min(Year))) # add on period of record col
# ambig_score<-droplevels(ambig_score[ambig_score$timerange > 9,])# look only at streams with >9 years of data
# ggplot(ambig_score, aes(y=Overall.Score, x=per_ambig))+geom_point()
# 
# ggplot(ambig_score, aes(x=Year, group=Year, y=per_ambig))+geom_boxplot()
# l=unique(c(as.character(ambig_score$Site.Code)))
# ambig_score$Site.Number<-as.numeric(factor(ambig_score$Site.Code, levels=l))
# with(ambig_score, rkt(date = as.numeric(Year), y = per_ambig, block = Site.Number, correct = FALSE, rep="a"))
# 
# cor.test(ambig_score$Overall.Score, ambig_score$per_ambig)
# data2<-ddply(ambig_score,.(Site.Code),mutate, mk.tau=kendallTrendTest( per_ambig~ as.numeric(Year))$estimate[1])
# data2<-ddply(data2,.(Site.Code),mutate, mk.slope=kendallTrendTest( per_ambig~ as.numeric(Year))$estimate[2])
# data2_trends<-unique(subset(data2, select=c(Site.Code, mk.slope, mk.tau))) 
# KC_trends<-read.csv("Site_Trends_coarse_LR_03092023_2002-2021_LRexclude.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))
# data2_score_trends<-merge(KC_trends, data2_trends,  by.x="Site.Code", by.y="Site.Code")
# # cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$mk.tau)
# cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$mk.slope)
# 
# 
# ##ambiguous by individuals###
# ambig<-ddply(OTU_collapsed4, .(Sample.Code, Year, Site.Code, Unique_OTU), summarize, ambig_taxa=sum(Quantity_OTU))
# ambig<-reshape2::dcast(ambig, Sample.Code+Year+Site.Code~Unique_OTU)
# ambig[is.na(ambig$`FALSE`),'FALSE']<-0
# ambig$per_ambig<-ambig$`FALSE`/ambig$`TRUE`
# ambig <- droplevels(ambig[ambig$Year > 2001,]) # subset to sites since 2002
# ambig <- ddply(ambig, 'Site.Code', mutate, por = length(per_ambig)) # add on period of record col
# ambig<-droplevels(ambig[ambig$por > 9,])# look only at sites with >9 years of data
# ambig<-subset(ambig, ambig$Site.Code!=exclude)
# ambig <- ddply(ambig, 'Site.Code', mutate, timerange = (max(Year)-min(Year))) # add on period of record col
# ambig<-droplevels(ambig[ambig$timerange > 9,])# look only at streams with >9 years of data
# ambig_score<-merge(bibi, ambig, by=c("Sample.Code", "Site.Code", "Year"))
# ambig_score <- ddply(ambig_score, 'Site.Code', mutate, por = length(unique(Year))) # add on period of record col
# ambig_score<-droplevels(ambig_score[ambig_score$por > 9,])# look only at sites with >9 years of data
# ambig_score <- ddply(ambig_score, 'Site.Code', mutate, timerange = (max(Year)-min(Year))) # add on period of record col
# ambig_score<-droplevels(ambig_score[ambig_score$timerange > 9,])# look only at streams with >9 years of data
# ggplot(ambig_score, aes(y=Overall.Score, x=per_ambig))+geom_point()
# 
# ggplot(ambig_score, aes(x=Year, group=Year, y=per_ambig))+geom_boxplot()
# l=unique(c(as.character(ambig_score$Site.Code)))
# ambig_score$Site.Number<-as.numeric(factor(ambig_score$Site.Code, levels=l))
# with(ambig_score, rkt(date = as.numeric(Year), y = per_ambig, block = Site.Number, correct = FALSE, rep="a"))
# 
# 
# cor.test(ambig_score$Overall.Score, ambig_score$per_ambig)
# data2<-ddply(ambig_score,.(Site.Code),mutate, mk.tau=kendallTrendTest( per_ambig~ as.numeric(Year))$estimate[1])
# data2<-ddply(data2,.(Site.Code),mutate, mk.slope=kendallTrendTest( per_ambig~ as.numeric(Year))$estimate[2])
# data2_trends<-unique(subset(data2, select=c(Site.Code, mk.slope, mk.tau))) 
# KC_trends<-read.csv("Site_Trends_coarse_LR_03092023_2002-2021_LRexclude.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))
# data2_score_trends<-merge(KC_trends, data2_trends,  by.x="Site.Code", by.y="Site.Code")
# # cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$mk.tau)
# cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$mk.slope)
# 
# 
# 
# density<-read.csv("Collapsed_Coarse_Taxa_density_LRexclude.csv")
# meanorgs<-ddply(density, .(OTU_COARSE), summarize, meanall=mean(total_sample_count))
# freq_taxa<-meanorgs[order(meanorgs$meanall,decreasing=T), ][1:25,1]
# density<-subset(density, !OTU_COARSE %in% freq_taxa)
# density<-ddply(density, .(Sample.Code, Site.Code, Year, Surface.Area ), summarize, sumOrgs=sum(total_sample_count, na.rm = T))
# density <- droplevels(density[density$Year > 2001,]) # subset to sites since 2002
# density <- ddply(density, 'Site.Code', mutate, por = length(unique(Year))) # add on period of record col
# density<-droplevels(density[density$por > 9,])# look only at sites with >9 years of data
# density<-subset(density, density$Site.Code!=exclude)
# density <- ddply(density, 'Site.Code', mutate, timerange = (max(Year)-min(Year))) # add on period of record col
# density<-droplevels(density[density$timerange > 9,])# look only at streams with >9 years of data
# density_score<-merge(bibi, density, by=c("Sample.Code", "Site.Code", "Year"))
# density_score <- ddply(density_score, 'Site.Code', mutate, por = length(unique(Year))) # add on period of record col
# density_score<-droplevels(density_score[density_score$por > 9,])# look only at sites with >9 years of data
# density_score <- ddply(density_score, 'Site.Code', mutate, timerange = (max(Year)-min(Year))) # add on period of record col
# density_score<-droplevels(density_score[density_score$timerange > 9,])# look only at streams with >9 years of data
# 
# ggplot(density_score, aes(y=Overall.Score, x=sumOrgs))+geom_point()
# ggplot(density_score, aes(y=Overall.Score, x=Surface.Area,group=Surface.Area))+geom_boxplot()
# 
# ggplot(density_score, aes(x=Year, group=Year, y=sumOrgs))+geom_boxplot()
# l=unique(c(as.character(density_score$Site.Code)))
# density_score$Site.Number<-as.numeric(factor(density_score$Site.Code, levels=l))
# with(density_score, rkt(date = as.numeric(Year), y = sumOrgs, block = Site.Number, correct = FALSE, rep="a"))
# 
# 
# cor.test(density_score$Overall.Score, density_score$sumOrgs)
# density_score[is.na(density_score$sumOrgs),]
# data2<-ddply(density_score,.(Site.Code),mutate, mk.tau=kendallTrendTest( sumOrgs~ as.numeric(Year))$estimate[1])
# data2<-ddply(data2,.(Site.Code),mutate, mk.slope=kendallTrendTest( sumOrgs~ as.numeric(Year))$estimate[2])
# data2_trends<-unique(subset(data2, select=c(Site.Code, mk.slope, mk.tau))) 
# KC_trends<-read.csv("Site_Trends_coarse_LR_03092023_2002-2021_LRexclude.csv")
# KC_trends<-subset(KC_trends, select=c(MKtau_Overall.Score, MKSlope_Overall.Score, Site.Code, slope_Overall.Score))
# data2_score_trends<-merge(KC_trends, data2_trends,  by.x="Site.Code", by.y="Site.Code")
# # cor.test(data2_score_trends$MKtau_Overall.Score, data2_score_trends$mk.tau)
# cor.test(data2_score_trends$MKSlope_Overall.Score, data2_score_trends$mk.slope)
# 
# 
# 
# 
# ###############End diagnostics##############



#########  Format, remove data from wrong months, average scores from samples with same site+year, and subset B-IBI data to data since 2001 with more than 9 years of data
bibi$month <- format(as.Date(bibi$Visit.Date, '%Y-%m-%d'), "%m") # add month column
# bibi$jdate<-yday(as.Date(bibi$Visit.Date, '%m/%d/%Y'))
# bibi$Year <- format(as.Date(bibi$Visit.Date, '%m/%d/%Y'), "%Y") # add year column
bibi$sampnum <- paste(bibi$Site.Code, format(as.Date(bibi$Visit.Date, '%Y-%m-%d'), "%y"), sep = "_") # add sample identifier
bibi <- droplevels(bibi[bibi$month %in% c('07', '08', '09', '10'),]) # exclude samples collected outside of July - Oct
dups<-bibi[duplicated(bibi[,c("Project" , "WRIA.Number"  , "Basin" , "Subbasin" , "Stream.or.River" , "Site.Code" , "Year" , "sampnum")]),]
# bibi <- aggregate(cbind(Latitude, Longitude, Visit.Date, Overall.Score, Tot_Richness_Score, E_Richness_Score, P_Richness_Score, T_Richness_Score, Cling_Richness_Score, LL_Richness_Score, Intol_Richness_Score, Dom_Percent_Score,  Pred_Percent_Score, Tol_Percent_Score) ~  Project + WRIA.Number  + Basin + Subbasin + Stream.or.River + Site.Code + Year + sampnum, data = bibi, FUN = "mean", na.action = na.exclude)
# bibi <-aggregate(cbind( Overall.Score, Taxa.Richness.Score, Ephemeroptera.Richness.Score, Plecoptera.Richness.Score, Trichoptera.Richness.Score, Clinger.Richness.Score, `Long-Lived.Richness.Score`, Intolerant.Richness.Score, Percent.Dominant.Score,  Predator.Percent.Score, Tolerant.Percent.Score) ~  Project + WRIA.Number  + Basin + Subbasin + Stream.or.River + Site.Code + Year + sampnum, data = bibi, FUN = "mean", na.action = na.exclude)#+jdate
bibi <-aggregate(cbind( Overall.Score, Tot_Richness_Score, E_Richness_Score, P_Richness_Score, T_Richness_Score, Cling_Richness_Score, LL_Richness_Score, Intol_Richness_Score, Dom_Percent_Score,  Pred_Percent_Score, Tol_Percent_Score) ~  Project + WRIA.Number  + Basin + Subbasin + Stream.or.River + Site.Code + Year + sampnum, data = bibi, FUN = "mean", na.action = na.exclude)#+jdate

## need to exclude CAO samples because sampled differently
bibi<-subset(bibi, Project!="Regulatory Effectiveness")
# bibi<-subset(bibi, Project!="KC Historical")
# bibi<-subset(bibi, Project!="Des Moines Creek Habitat Enhancements, Phase 3")
# bibi<-subset(bibi, Project!="Des Moines Creek Water Quality Monitoring Program")
# bibi<-subset(bibi, Project!="Restoration and Protection of Select B-IBI Basins - King County riffle method")
# bibi<-subset(bibi, Project!="Restoration and Protection of Select B-IBI Basins - Ecology transect method")
unique(bibi$Project)

# bibi <- aggregate(cbind(Latitude, Longitude, Event.Date, Overall.Score, Taxa.Richness.Quantity, Ephemeroptera.Richness.Quantity, Plecoptera.Richness.Quantity, Trichoptera.Richness.Quantity, EPT.Richness.Quantity, Clinger.Richness.Quantity, Long.Lived.Richness.Quantity, Intolerant.Richness.Quantity, Percent.Dominant.Quantity,  Predator.Percent.Quantity, Tolerant.Percent.Quantity) ~  Project + WRIA.Number + WRIA + Basin + Subbasin + Stream + Site.Code + year + sampnum + year, data = bibi, FUN = "mean", na.action = na.exclude)
bibi <- droplevels(bibi[bibi$Year > 2001,]) # subset to sites since 2002
bibi <- ddply(bibi, 'Site.Code', mutate, por = length(Overall.Score)) # add on period of record col
bibi<-droplevels(bibi[bibi$por > 9,])# look only at sites with >9 years of data
bibi<-subset(bibi, bibi$Site.Code!=exclude)
bibi <- ddply(bibi, 'Site.Code', mutate, timerange = (max(Year)-min(Year))) # add on period of record col
bibi<-droplevels(bibi[bibi$timerange > 9,])# look only at streams with >9 years of data

# test<-ddply(bibi, .(Site.Code), summarize, nyrs=length(Overall.Score))
# write.csv(test, "yrs_sampled.csv")

bibi.lr<-bibi
# bibi.lr[bibi.lr$Year<2012,"Overall.Score"]<-(bibi.lr[bibi.lr$Year<2012,"Overall.Score"]*.997)+2.51 ###subsamp to 500
bibi.lr1<-bibi.lr

ggplot(bibi, aes(x=Site.Code, y=Year))+geom_tile()

sum(  unique(subset(bibi, Year>2018)$Site.Code) %in% unique(subset(bibi, Year<2006)$Site.Code)) /120
unique(subset(bibi, Year>2005)$Site.Code)[( ! unique(subset(bibi, Year>2005)$Site.Code) %in% unique(subset(bibi, Year<2006)$Site.Code)) ]
unique(subset(bibi, Year<2006)$Site.Code)[( ! unique(subset(bibi, Year<2006)$Site.Code) %in% unique(subset(bibi, Year>2018)$Site.Code)) ]

# bibi.lr<-subset(bibi.lr, WRIA.Number==9)
# bibi.lr1<-subset(bibi.lr1, WRIA.Number==9)
# bibi<-subset(bibi, WRIA.Number==9)

sites<-unique(bibi.lr[, c("Subbasin","Stream.or.River", "Site.Code")] )
sites<-data.frame(site=sites)
# bibi.lr1$Year<-as.numeric(paste0(bibi.lr1$Year))

# ggplot(bibi.lr1, aes(x=Year, y=Tot_Richness_Score, color=Site.Code))+geom_smooth(se=F)+theme(legend.position = "none")+
#   facet_wrap(~Site.Code)+geom_point()+
#   geom_vline(aes(xintercept=2013), color="black")

# bibi.lr1[bibi.lr1$Year<2012,"Overall.Score"]<-(bibi.lr1[bibi.lr1$Year<2012,"Overall.Score"]*.997)+2.51 ###subsamp to 500
# bibi.lr1[bibi.lr1$Year<2012,"Overall.Score"]<-(bibi.lr1[bibi.lr1$Year<2012,"Overall.Score"]*.98)+3.53 ###subsamp to 450
ggplot(bibi.lr1, aes(x=Year, y=Overall.Score))+geom_smooth(se=F)+theme(legend.position = "none")+
  geom_point()



library(ggpmisc)

meanscores<-ddply(bibi.lr1, .(Year), summarize, meanScore=mean(Overall.Score), nyears=length(unique(Site.Code)), sd=sd(Overall.Score), min=min(Overall.Score), max=max(Overall.Score), median=median(Overall.Score))
meanscore<-ggplot(meanscores, aes(x=Year, y=meanScore))+geom_smooth(method="lm",se=F)+theme(legend.position = "none")+
  geom_point()+ylim(c(0,100))+labs(y="BIBI Score")+
  stat_poly_eq(formula = y ~ x, rr.digits = 2, coef.digits = 3, size = 4, color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~")), parse = TRUE) 

ggsave("2002-2021_meansscores_LR_exclude.png", plot=meanscore, width=20, height=10)

median(bibi.lr1$Overall.Score)
median(bibi.lr1$Year)

##calculating intercept for Sen-Theil line, from https://pubs.usgs.gov/tm/2006/tm4a7/pdf/USGSTM4A7.pdf pdf page 15, and https://rdrr.io/github/USGS-R/HASP/src/R/statistics.R
median(bibi.lr1$Overall.Score)-(0.48*median(bibi.lr1$Year))


scores<-ggplot(bibi.lr1 )+
  geom_rect(xmin=2001, xmax=2022, ymin=0, ymax=20, fill="firebrick1", color="black")+
  geom_rect(xmin=2001, xmax=2022, ymin=20, ymax=40, fill="tan1", color="black")+
  geom_rect(xmin=2001, xmax=2022, ymin=40, ymax=60, fill="lightgoldenrod", color="black")+
  geom_rect(xmin=2001, xmax=2022, ymin=60, ymax=80, fill="lightgreen", color="black")+
  geom_rect(xmin=2001, xmax=2022, ymin=80, ymax=100, fill="steelblue1", color="black")+
  geom_boxplot(aes(y=Overall.Score, x=as.numeric(paste0(Year)), group=as.factor(Year)), fill="white")+
  geom_point(data=meanscores, mapping=aes(x=as.numeric(paste0(Year)), y=meanScore))+
  # geom_smooth(data=meanscores, mapping=aes(x=as.numeric(paste0(Year)), y=meanScore), method="lm",se=F)+
  labs(y="Overall Score", x="Year")+ theme(text = element_text(size = 28))+
  # geom_quantile(data=bibi.lr1, mapping=aes(x=as.numeric(paste0(Year)), y=Overall.Score), quantiles=.5, color="red", size=1)+
  geom_abline(slope=0.48, intercept = -914.2032, color="red", lwd=1)##adding Sen-Theil line and intercept calculated above
ggsave("2002-2021_scores_pres_LR_exclude.png", plot=scores, width=20, height=10)


scores<-ggplot(bibi.lr1 )+
  # geom_rect(xmin=2001, xmax=2022, ymin=0, ymax=20,  color="black")+
  # geom_rect(xmin=2001, xmax=2022, ymin=20, ymax=40,  color="black")+
  # geom_rect(xmin=2001, xmax=2022, ymin=40, ymax=60, color="black")+
  # geom_rect(xmin=2001, xmax=2022, ymin=60, ymax=80,  color="black")+
  # geom_rect(xmin=2001, xmax=2022, ymin=80, ymax=100,  color="black")+
  scale_y_continuous(name= "Overall Score", breaks=c(0,20, 40, 60, 80, 100))+
  theme_bw()+
  geom_boxplot(aes(y=Overall.Score, x=as.numeric(paste0(Year)), group=as.factor(Year)), fill="white")+
  annotate("text", y= Inf, x=-Inf, parse=T, label="italic(y) == 0.48 * italic(x) -914.2", hjust=-.25, vjust=1)+
  # geom_point(data=meanscores, mapping=aes(x=as.numeric(paste0(Year)), y=meanScore))+
  theme(text=element_text(family="sans"), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(colour="black"))+
  # geom_smooth(data=meanscores, mapping=aes(x=as.numeric(paste0(Year)), y=meanScore), method="lm",se=F)+
  labs(y="Overall Score", x="Year")+
  # geom_quantile(data=bibi.lr1, mapping=aes(x=as.numeric(paste0(Year)), y=Overall.Score), quantiles=.5, color="red", size=1)+
  geom_abline(slope=0.48, intercept = -914.2032, color="black")##adding Sen-Theil line and intercept calculated above
  
ggsave("2002-2021_scores_pres_LR_exclude_FWS.tiff", plot=scores, width=8.4, height=6, units="cm", dpi=600)

# results<-summary(lm(jdate~Year+Site.Code,data=bibi.lr))
# results<-results$coefficients
# results<-as.data.frame(results)
# write.csv(results, "CSV/date_trends.csv")
# results$`Pr(>|t|)`
# results<-subset(results, `Pr(>|t|)`<.05)
# results$sitename<-row.names(results)
# results$sitename<-str_remove(results$sitename, "Site.Code")
# biobi.lr_sub<-bibi.lr[which(bibi.lr$Site.Code %in% results$sitename),]
# ggplot(bibi.lr, aes(Year, jdate, group=Site.Code, fill=Site.Code, colour=Site.Code))+geom_smooth(method="lm", se=FALSE)+geom_point(size=3)+theme(legend.position = "none")+geom_violin(inherit.aes = F, data=bibi.lr, aes(Year, jdate, group=Year))

names(bibi.lr1)

l=unique(c(as.character(bibi.lr1$Site.Code)))
bibi.lr1 <- bibi.lr1[complete.cases(bibi.lr1[, 1]), ]
str(bibi.lr1)
bibi.lr1$Site.Number<-as.numeric(factor(bibi.lr1$Site.Code, levels=l))

metrics<-colnames(bibi.lr1)[9:(ncol(bibi.lr1)-3)]

library(rkt)

for (each in metrics) {
  bibi.lr1$rkt.pval <- with(bibi.lr1, rkt(date = Year, y = get(each), block = Site.Number, correct = FALSE)[1])
  bibi.lr1$rkt.Sen <- with(bibi.lr1, rkt(date = Year, y = get(each), block = Site.Number, correct = FALSE)[3])
  bibi.lr1$rkt.tau <- with(bibi.lr1, rkt(date = Year, y = get(each), block = Site.Number, correct = FALSE)[12])
  bibi.lr1$RKTtrend <- with(bibi.lr1, ifelse(rkt.pval < 0.05 & rkt.tau < 0, "negative", ifelse(rkt.pval < 0.05 & rkt.tau > 0, "positive", "none")))
  colnames(bibi.lr1)[ncol(bibi.lr1)-3]<-paste0("rkt.pval_", each)
  colnames(bibi.lr1)[ncol(bibi.lr1)-2]<-paste0("rkt.Sen_", each)
  colnames(bibi.lr1)[ncol(bibi.lr1)-1]<-paste0("rkt.tau_", each)
  colnames(bibi.lr1)[ncol(bibi.lr1)]<-paste0("RKTtrend_", each)
}

overall_trends<-unique(bibi.lr1[(ncol(bibi.lr)+2):(ncol(bibi.lr1))])
overall_trends = as.matrix(overall_trends)
write.csv(overall_trends,"RKT_overall_trends_01182023_2002-2021_LR_exclude.csv")
trend<-subset(overall_trends, select=c(str_subset(colnames(overall_trends), "RKTtrend_")))

##plot the trends for each metric
for (i in 9:(ncol(bibi.lr)-3)) {
  ylabel<-names(bibi.lr1[i])
  # p<-ggplot(bibi.lr1, aes(as.factor(year), bibi.lr1[,i]))+geom_violin(draw_quantiles=c(.25, .5, .75))+ stat_summary(fun.y=median, geom="point", size=2, color="red")+labs(y=ylabel)
  o<-ggplot(bibi.lr1, aes(as.factor(Year), bibi.lr1[,i]))+geom_boxplot()+labs(y=ylabel)#+geom_abline(aes(intercept=get(paste0("MKinter_",names(bibi.lr1[i]))),slope=get(paste0("MKSlope_",names(bibi.lr1[i])))))
  # print(p)
  print(o)
  ggsave(paste0("metrics_boxplots_", ylabel, "03092023_2002-2021_LR_exclude.png"), plot = o, width=20, height=10)
}


# ##############
# 
# medvalues<-ddply(bibi.lr1, .(Subbasin, Year), summarize, Overall.Score=median(Overall.Score), Tot_Richness_Score=median(Tot_Richness_Score),
#                  E_Richness_Score=median(E_Richness_Score), P_Richness_Score=median(P_Richness_Score), T_Richness_Score=median(T_Richness_Score),
#                  Cling_Richness_Score=median(Cling_Richness_Score), LL_Richness_Score=median(LL_Richness_Score), Intol_Richness_Score=median(Intol_Richness_Score),
#                  Dom_Percent_Score=median(Dom_Percent_Score), Pred_Percent_Score=median(Pred_Percent_Score), Tol_Percent_Score=median(Tol_Percent_Score))
# 
# # medvalues<-ddply(bibi.lr1, .(Subbasin, Year), summarize, Overall.Score=median(Overall.Score), Tot_Richness_Score=median(Taxa.Richness.Score), 
# #                  E_Richness_Score=median(Ephemeroptera.Richness.Score), P_Richness_Score=median(Plecoptera.Richness.Score), T_Richness_Score=median(Trichoptera.Richness.Score), 
# #                  Cling_Richness_Score=median(Clinger.Richness.Score), LL_Richness_Score=median(`Long-Lived.Richness.Score`), Intol_Richness_Score=median(Intolerant.Richness.Score),
# #                  Dom_Percent_Score=median(Percent.Dominant.Score), Pred_Percent_Score=median(Predator.Percent.Score), Tol_Percent_Score=median(Tolerant.Percent.Score))
# 
# 
# l=unique(c(as.character(medvalues$Subbasin)))
# # medvalues <- medvalues[complete.cases(medvalues[, 1]), ]
# medvalues$Site.Number<-as.numeric(factor(medvalues$Subbasin, levels=l))
# 
# metrics<-colnames(medvalues)[3:(ncol(medvalues)-1)]
# 
# medvalues$Year<-as.numeric(paste0(medvalues$Year))
# for (each in metrics) {
#   medvalues$rkt.pval <- with(medvalues, rkt(date = Year, y = get(each), block = Site.Number, correct = FALSE)[1])
#   medvalues$rkt.Sen <- with(medvalues, rkt(date = Year, y = get(each), block = Site.Number, correct = FALSE)[3])
#   medvalues$rkt.tau <- with(medvalues, rkt(date = Year, y = get(each), block = Site.Number, correct = FALSE)[12])
#   medvalues$RKTtrend <- with(medvalues, ifelse(rkt.pval < 0.05 & rkt.tau < 0, "negative", ifelse(rkt.pval < 0.05 & rkt.tau > 0, "positive", "none")))
#   colnames(medvalues)[ncol(medvalues)-3]<-paste0("rkt.pval_", each)
#   colnames(medvalues)[ncol(medvalues)-2]<-paste0("rkt.Sen_", each)
#   colnames(medvalues)[ncol(medvalues)-1]<-paste0("rkt.tau_", each)
#   colnames(medvalues)[ncol(medvalues)]<-paste0("RKTtrend_", each)
# }
# names(medvalues)
# 
# overall_trends<-unique(medvalues[c(15:(ncol(medvalues)))])
# overall_trends = as.matrix(overall_trends)
# write.csv(overall_trends,"median_overall_trends_03092023_2002-2021_LRexclude.csv")
# trend<-subset(overall_trends, select=c(str_subset(colnames(overall_trends), "RKTtrend_")))

########## basin specific RKT regressions ####

# bibi <- droplevels(bibi[as.numeric(bibi$year)> 2006,])
bibi.lr2 <- ddply(bibi, 'Subbasin', mutate, timerange = (as.numeric(max(Year))-as.numeric(min(Year))))
bibi.lr2 <- ddply(bibi.lr2, 'Subbasin', mutate, por = length(Overall.Score)) # add on period of record col

bibi.lr2<-droplevels(bibi.lr2[bibi.lr2$por > 9,])
bibi.lr2<-droplevels(bibi.lr2[bibi.lr2$timerange > 9,])# look only at streams with >9 years of data
ddply(bibi.lr2, 'Subbasin', summarize, count=length(unique(Year)))

unique(bibi.lr2$Site.Code)

bibi.lr2<-ddply(bibi.lr2, 'Subbasin', mutate, max = max(Year))

scal<-"Subbasin" # "Site.Code" or "Stream.or.River"

form<-names(bibi.lr2)[9:(ncol(bibi.lr2)-3)]
bibi.lr2$Year<-as.numeric(paste0(bibi.lr2$Year))


l=unique(c(as.character(bibi.lr2$Site.Code)))
# bibi.lr2 <- bibi.lr2[complete.cases(bibi.lr2[, 1]), ]
bibi.lr2$Site.Number<-as.numeric(factor(bibi.lr2$Site.Code, levels=l))

# rm(bibi.lr2)
for (var in form) {
  bibi.lr2<-ddply(bibi.lr2,.(get(scal)),mutate, rkt.tau=rkt(date = Year, y= get(paste0(var)), block = Site.Number, correct = FALSE )$tau)
  bibi.lr2<-ddply(bibi.lr2,.(get(scal)),mutate, rkt.Sen=rkt(date = Year, y= get(paste0(var)), block = Site.Number, correct = FALSE )$B)
  bibi.lr2<-ddply(bibi.lr2,.(get(scal)),mutate, rkt.pval=rkt( date = Year, y= get(paste0(var)), block = Site.Number, correct = FALSE )$sl)
  bibi.lr2$MKtrend <- ifelse(bibi.lr2$rkt.pval < 0.05 & bibi.lr2$rkt.tau < 0, "negative", ifelse(bibi.lr2$rkt.pval < 0.05 & bibi.lr2$rkt.tau > 0, "positive", "none"))
  colnames(bibi.lr2)[ncol(bibi.lr2)-3]<-paste0("RKTtau_", var)
  colnames(bibi.lr2)[ncol(bibi.lr2)-2]<-paste0("RKTSlope_", var)
  colnames(bibi.lr2)[ncol(bibi.lr2)-1]<-paste0("RKTpval_", var)
  colnames(bibi.lr2)[ncol(bibi.lr2)]<-paste0("RKTtrend_", var)
}

names(bibi.lr2)
overall_trends<-unique(bibi.lr2[c(5,27:(ncol(bibi.lr2)))])
overall_trends<-unique(subset(overall_trends, select=c("Subbasin" ,str_subset(colnames(overall_trends), "RKTtau"), str_subset(colnames(overall_trends), "RKTpval"),str_subset(colnames(overall_trends), "RKTtrend"))))



write.csv(overall_trends, "Subbasin_RKTtrends_03092023_2002-2021_LR_exclude.csv")


#filename<-sprintf(paste0("./CSV/Trends_Stream_", Dat, "_",files, ".csv"))
#write.csv(overall_trends,filename)

bibi_sum<-ddply(bibi.lr2, "Subbasin", summarise, mean_Overall.Score=mean(Overall.Score), SD_overallscore=sd(Overall.Score), SE_overallscore=sd(Overall.Score)/sqrt(length(Overall.Score)), len=length(Overall.Score), RKT_trend=unique(RKTtrend_Overall.Score), Avg_RKTSlope_Overall.Score=mean(RKTSlope_Overall.Score), Avg_RKTtau_Overall.Score=mean(RKTtau_Overall.Score), Avg_RKTpval_Overall.Score=mean(RKTpval_Overall.Score), sites=length(unique(Site.Code)), min=min(Overall.Score), max=max(Overall.Score), med=median(Overall.Score))
write.csv(bibi_sum,"score_RKTtrend_summarized_by_subbasin_03092023_2002-2021_LRexclude.csv")

pp<-ggplot(bibi.lr2, aes(Year, Overall.Score, group=Year))+geom_boxplot(aes(fill=RKTtrend_Overall.Score))+facet_wrap(~Subbasin, ncol=4)+labs(y="B-IBI Score", x="Year")+ theme(axis.text.x = element_text(angle = 90))
ggsave(paste0("SubbasinRKTtrend_03092023_2002-2021_LRexclude.png"), plot = pp, width=20, height=10)


for(each in unique(bibi.lr2$Subbasin)){
pp<-ggplot(subset(bibi.lr2, Subbasin==each), aes(Year, Overall.Score, group=Year))+geom_boxplot(aes(fill=RKTtrend_Overall.Score))+facet_wrap(~Subbasin, ncol=4)+labs(y="B-IBI Score", x="Year")+ theme(axis.text.x = element_text(angle = 90))+theme(text = element_text(size = 28))+ guides(fill=guide_legend(title="Trend"))
ggsave(paste0("SubbasinRKTtrend_", each, "_03092023_2002-2021_LRexclude.png"), plot = pp, width=20, height=10)
# print(pp)
}


# #### Regional trends ####
# for (i in 9:(ncol(bibi.lr1)-1)) {
#   # bibi.lr1<- ddply(bibi.lr1, .variables ="Site.Code", mutate, slope =lm(as.formula(paste0(colnames(bibi.lr1[i]), " ~ ", "as.numeric(year)")))[[1]][2])
#   # bibi.lr1<-ddply(bibi.lr1, .variables ="Site.Code", mutate, pval = summary(lm(as.formula(paste0(colnames(bibi.lr1[i]), " ~ ", "as.numeric(year)"))))[[4]][2,4])
#   # bibi.lr1$trend <- ifelse(bibi.lr1$pval < 0.05 & bibi.lr1$slope < 0, "negative", ifelse(bibi.lr1$pval < 0.05 & bibi.lr1$slope > 0, "positive", "none")) # add trend col
#   bibi.lr1$mk.tau<-kendallTrendTest(as.formula(paste0(colnames(bibi.lr1[i]), " ~ ", "as.numeric(Year)")), data=bibi.lr1)$estimate[1]
#   bibi.lr1$mk.Sen<-kendallTrendTest(as.formula(paste0(colnames(bibi.lr1[i]), " ~ ", "as.numeric(Year)")), data=bibi.lr1)$estimate[2]
#   bibi.lr1$mk.pval<-kendallTrendTest(as.formula(paste0(colnames(bibi.lr1[i]), " ~ ", "as.numeric(Year)")), data=bibi.lr1)$p.value
#   bibi.lr1$mk.inter<-kendallTrendTest(as.formula(paste0(colnames(bibi.lr1[i]), " ~ ", "as.numeric(Year)")), data=bibi.lr1)$estimate[3]
#   bibi.lr1$MKtrend <- ifelse(bibi.lr1$mk.pval < 0.05 & bibi.lr1$mk.tau < 0, "negative", ifelse(bibi.lr1$mk.pval < 0.05 & bibi.lr1$mk.tau > 0, "positive", "none"))
#   colnames(bibi.lr1)[ncol(bibi.lr1)-4]<-paste0("MKtau_", paste0(colnames(bibi.lr1[i])))
#   colnames(bibi.lr1)[ncol(bibi.lr1)-3]<-paste0("MKSlope_", paste0(colnames(bibi.lr1[i])))
#   colnames(bibi.lr1)[ncol(bibi.lr1)-2]<-paste0("MKpval_", paste0(colnames(bibi.lr1[i])))
#   colnames(bibi.lr1)[ncol(bibi.lr1)-1]<-paste0("MKinter_", paste0(colnames(bibi.lr1[i])))
#   colnames(bibi.lr1)[ncol(bibi.lr1)]<-paste0("MKtrend_", paste0(colnames(bibi.lr1[i])))
# }
# 
# overall_trends<-unique(bibi.lr1[(ncol(bibi.lr)+1):(ncol(bibi.lr1))])
# write.csv(overall_trends,"overall_trends.csv")
# trend<-subset(overall_trends, select=c(str_subset(colnames(overall_trends), "MKtrend")))
# 


#####Site trends ####
library(EnvStats)
scal<-"Site.Code" # "Site.Code" or "Subbasin"
# form<-"Overall.Score"
form<-names(bibi.lr)[9:(ncol(bibi.lr)-2)]
# bibi.lr[bibi.lr$Year<2012,"Overall.Score"]<-(bibi.lr[bibi.lr$Year<2012,"Overall.Score"]*.972)+5.01
# for (i in 9:(ncol(bibi.lr))) {
for (var in form) {
  bibi.lr<- ddply(bibi.lr, .variables =scal, mutate, slope =lm(get(paste0(var)) ~ as.numeric(Year))[[1]][2])
  bibi.lr<-ddply(bibi.lr, .variables =scal, mutate, pval = summary(lm(get(paste0(var)) ~ as.numeric(Year)))[[4]][2,4])
  bibi.lr$trend <- ifelse(bibi.lr$pval < 0.05 & bibi.lr$slope < 0, "negative", ifelse(bibi.lr$pval < 0.05 & bibi.lr$slope > 0, "positive", "none")) # add trend col
  bibi.lr<-ddply(bibi.lr,.(get(scal)),mutate, mk.tau=kendallTrendTest( get(paste0(var))~ as.numeric(Year))$estimate[1])
  # bibi.lr<-ddply(bibi.lr,.(get(scal)),mutate, mk.tau=kendallTrendTest(as.formula(paste0(colnames(bibi.lr[i]), " ~ ", "as.numeric(Year)")))$estimate[1])
  bibi.lr<-ddply(bibi.lr,.(get(scal)),mutate, mk.Sen=kendallTrendTest( get(paste0(var))~ as.numeric(Year))$estimate[2])
  bibi.lr<-ddply(bibi.lr,.(get(scal)),mutate, mk.pval=kendallTrendTest( get(paste0(var))~ as.numeric(Year))$p.value)
  bibi.lr<-ddply(bibi.lr,.(get(scal)),mutate, mk.inter=kendallTrendTest( get(paste0(var))~ as.numeric(Year))$estimate[3])
  bibi.lr$MKtrend <- ifelse(bibi.lr$mk.pval < 0.05 & bibi.lr$mk.tau < 0, "negative", ifelse(bibi.lr$mk.pval < 0.05 & bibi.lr$mk.tau > 0, "positive", "none"))
  colnames(bibi.lr)[ncol(bibi.lr)-7]<-paste0("slope_",var)
  colnames(bibi.lr)[ncol(bibi.lr)-6]<-paste0("LRpval_", var)
  colnames(bibi.lr)[ncol(bibi.lr)-5]<-paste0("LRtrend_", var)
  colnames(bibi.lr)[ncol(bibi.lr)-4]<-paste0("MKtau_", var)
  colnames(bibi.lr)[ncol(bibi.lr)-3]<-paste0("MKSlope_", var)
  colnames(bibi.lr)[ncol(bibi.lr)-2]<-paste0("MKpval_", var)
  colnames(bibi.lr)[ncol(bibi.lr)-1]<-paste0("MKinter_", var)
  colnames(bibi.lr)[ncol(bibi.lr)]<-paste0("MKtrend_", var)
}

sampletimes<-ggplot(bibi.lr, aes(x=Site.Code, y=Year, fill=MKSlope_Overall.Score),color="black")+geom_tile(color="black")+scale_fill_gradient2(name="Sen-Theil Slope")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = "darkgray",
                                        colour = "darkgray"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave("sampletimes.png", plot=sampletimes, width=20, height=10)

names(bibi.lr)
overall_trends<-unique(bibi.lr[c(7,6,5,23:(ncol(bibi.lr)))])

# overall_trends<-unique(subset(overall_trends, select=c("Site.Code" , "Subbasin", "Stream.or.River", str_subset(colnames(overall_trends), "MKtau"), str_subset(colnames(overall_trends), "MKpval"),str_subset(colnames(overall_trends), "MKtrend"))))
write.csv(overall_trends,"Site_Trends_coarse_LR_03092023_2002-2021_LRexclude.csv")

bibi_sum_site<-ddply(bibi.lr, "Site.Code", summarise, mean_Overall.Score=mean(Overall.Score), SD_overallscore=sd(Overall.Score), SE_overallscore=sd(Overall.Score)/sqrt(length(Overall.Score)), len=length(Overall.Score), MK_trend=unique(MKtrend_Overall.Score), Avg_MKSlope_Overall.Score=mean(MKSlope_Overall.Score), Avg_MKtau_Overall.Score=mean(MKtau_Overall.Score), Avg_MKpval_Overall.Score=mean(MKpval_Overall.Score), sites=length(unique(Site.Code)), min=min(Overall.Score), max=max(Overall.Score), med=median(Overall.Score))
write.csv(bibi_sum_site,"score_RKTtrend_summarized_by_site_03092023_2002-2021_LRexclude.csv")

sum(bibi_sum_site$len)

########### Clean up names for plotting
bibi.lr$Stream<-gsub("tributary", "Trib", bibi.lr$Stream)# further shorten site names for plotting
bibi.lr$Stream<-gsub("Creek", "Ck.", bibi.lr$Stream)
bibi.lr$Stream<-gsub("Washington", "WA", bibi.lr$Stream)
bibi.lr$Stream<-gsub("Wash,", "WA", bibi.lr$Stream)
bibi.lr$Stream<-gsub("Green River-", "Green River -", bibi.lr$Stream) # clean river names for plotting
bibi.lr$Stream<-gsub("Lower Green River trib", "Green River - Lower trib", bibi.lr$Stream) # clean river names for plotting
bibi.lr$stream2 <- str_wrap(bibi.lr$Stream, width = 20)
bibi.lr$stream2 <-gsub("\\\n", "\\\r", bibi.lr$stream2) # make a carriage return rather than line break
bibi.lr$stream2<-as.factor(bibi.lr$stream2)

############  PLOTTING

############  Plot all positive and negative trends for overall and individual B-IBI scores
for (var in colnames(bibi.lr[str_subset(colnames(bibi.lr), "trend")])) {
  if(nrow(bibi.lr[bibi.lr[var]=='positive',])>=1) {
    pp<-ggplot(bibi.lr[bibi.lr[var]=='positive',], aes(as.numeric(Year), get(str_subset(colnames(bibi.lr[,c(1:20)]), str_split(var, "_", n=2)[[1]][2])), group = get(scal), fill = get(scal), colour = get(scal))) + ylim(c(0,100)) + geom_point(colour = 'black', size = 5, pch = 21) + theme(legend.position="none") + 
      facet_wrap(~ Subbasin +factor(stream2)+get(scal), ncol = 4) +  #factor(stream2) + 
      theme(axis.title=element_text(size=20,face="bold"), axis.text=element_text(size=15), axis.text.x = element_text(angle = 90), strip.text = element_text(size=12)) + labs(y = var, x = "Year")
    pp<-pp+geom_abline(aes(intercept=get(paste0("MKinter_",str_split(var, "_", n=2)[[1]][2])),slope=get(paste0("MKSlope_",str_split(var, "_", n=2)[[1]][2])),  colour=Site.Code))
    pp<-pp+geom_text(mapping= aes(x=2012, y=6, label=paste("Slope = ", round(get(paste0("MKSlope_",str_split(var, "_", n=2)[[1]][2])), 3))), colour="black")+theme(text=element_text(size=20), plot.title=element_text(hjust=.5, size=24, face="bold"))
    ggsave(paste0("BIBI~timeUp_", scal, "_", var,   "03092023_2002-2021_LRexclude.png"), plot = pp, width=20, height=10)
  }
  if(nrow(bibi.lr[bibi.lr[var]=='negative',])>=1) {
    pn<-ggplot(bibi.lr[bibi.lr[var]=='negative',], aes(as.numeric(Year), get(str_subset(colnames(bibi.lr[,c(1:20)]), str_split(var, "_", n=2)[[1]][2])), group = get(scal), fill = get(scal), colour = get(scal))) + 
      ylim(c(0,100)) + geom_point(colour = 'black', size = 5, pch = 21) + theme(legend.position="none") + 
      #stat_smooth(method = "lm", formula = y ~ x) + 
      facet_wrap(~ Subbasin+factor(stream2) + get(scal), ncol = 4) + #factor(stream2) +
      #stat_poly_eq(formula = y ~ x, label.y = 92, rr.digits = 2, coef.digits = 3, size = 4, color = "black", aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~")), parse = TRUE) + 
      theme(axis.title=element_text(size=20,face="bold"), axis.text=element_text(size=15), axis.text.x = element_text(angle = 90), strip.text = element_text(size=12)) + 
      labs(y = var, x = "Year")
    pn<-pn+geom_abline(aes(intercept=get(paste0("MKinter_",str_split(var, "_", n=2)[[1]][2])),slope=get(paste0("MKSlope_",str_split(var, "_", n=2)[[1]][2])),  colour=get(scal)))
    pn<-pn+geom_text(mapping= aes(x=2012, y=6, label=paste("Slope = ", round(get(paste0("MKSlope_",str_split(var, "_", n=2)[[1]][2])), 3))), colour="black")+theme(text=element_text(size=20), plot.title=element_text(hjust=.5, size=24, face="bold"))
    ggsave(paste0("BIBI~timeDown_", scal, "_",var, "03092023_2002-2021_LRexclude.png"), plot = pn, width=20, height=10)
  }
}
 library(RColorBrewer)

# for (var in colnames(bibi.lr[str_subset(colnames(bibi.lr), "trend")])) {
  # if(nrow(bibi.lr[bibi.lr[var]=='positive',])>=1) {
var="MKtrend_Overall.Score"

bibi.lr$pchOS<-as.factor(bibi.lr$MKtrend_Overall.Score)
shapes = c(6, 4, 2) 
shapes <- shapes[as.numeric(bibi.lr$pchOS)]


    pp<-ggplot(bibi.lr, aes(as.numeric(Year), get(str_subset(colnames(bibi.lr[,c(1:20)]), str_split(var, "_", n=2)[[1]][2])), group = get(scal), fill = get(scal), colour = get(scal))) + 
      ylim(c(0,100)) + 
      geom_point( aes(shape = c(shapes))) + scale_shape_identity()+ #theme(legend.position="none") +
      facet_wrap(~ Subbasin, ncol = 4) +  #factor(stream2) + 
      theme(axis.title=element_text(size=20,face="bold"), axis.text=element_text(size=15), axis.text.x = element_text(angle = 90), strip.text = element_text(size=12)) + labs(y = var, x = "Year")
    pp<-pp+geom_abline(aes(intercept=get(paste0("MKinter_",str_split(var, "_", n=2)[[1]][2])),slope=get(paste0("MKSlope_",str_split(var, "_", n=2)[[1]][2])),  colour=Site.Code))+
      scale_colour_manual(values=rep(brewer.pal(8,"Dark2"), times=20))
    # pp<-pp+geom_text(mapping= aes(x=2012, y=6, label=paste("Slope = ", round(get(paste0("MKSlope_",str_split(var, "_", n=2)[[1]][2])), 3))), colour="black")+theme(text=element_text(size=20), plot.title=element_text(hjust=.5, size=24, face="bold"))
    # ggsave(paste0("WRIA9_BIBI~timeUp_", scal, "_", var,   "-2.png"), plot = pp, width=20, height=10)
  # }
    print(pp)
    # }
    ggsave(paste0("OverallScores_03092023_2002-2021_LRexclude.png"), plot = pp, width=20, height=10)
    
    
ggplot(bibi.lr, aes(x=Year, y=Overall.Score))+  geom_rect(xmin=2001, xmax=2022, ymin=0, ymax=20, fill="firebrick1", color="black")+
      geom_rect(xmin=2001, xmax=2022, ymin=20, ymax=40, fill="tan1", color="black")+
      geom_rect(xmin=2001, xmax=2022, ymin=40, ymax=60, fill="lightgoldenrod", color="black")+
      geom_rect(xmin=2001, xmax=2022, ymin=60, ymax=80, fill="lightgreen", color="black")+
      geom_rect(xmin=2001, xmax=2022, ymin=80, ymax=100, fill="steelblue1", color="black")+
      # geom_smooth(se=F, method="lm")+
  geom_abline(aes(intercept=get(paste0("MKinter_",str_split(var, "_", n=2)[[1]][2])),slope=get(paste0("MKSlope_",str_split(var, "_", n=2)[[1]][2]))), color="blue", size=1)+
      theme(legend.position = "none")+
      facet_wrap(~Site.Code)+geom_point( aes(shape = c(shapes))) + scale_shape_identity()+labs(y="Overall B-IBI Score")

    
    ggsave("SiteScores_03092023_2002-2021_LRexclude.png", width=20, height=10)

    
    # for(each in unique(bibi.lr2$Site.Code)){
      # pchOS<-as.factor(subset(bibi.lr, Site.Code==each)$MKtrend_Overall.Score)
      # shapes = c(6, 4, 2) 
      # shapes <- shapes[as.numeric(pchOS)]
    #   pp<-ggplot(subset(bibi.lr, Site.Code==each), aes(x=Year, y=Overall.Score))+  geom_rect(xmin=2001, xmax=2022, ymin=0, ymax=20, fill="firebrick1", color="black")+
    #     geom_rect(xmin=2001, xmax=2022, ymin=20, ymax=40, fill="tan1", color="black")+
    #     geom_rect(xmin=2001, xmax=2022, ymin=40, ymax=60, fill="lightgoldenrod", color="black")+
    #     geom_rect(xmin=2001, xmax=2022, ymin=60, ymax=80, fill="lightgreen", color="black")+
    #     geom_rect(xmin=2001, xmax=2022, ymin=80, ymax=100, fill="steelblue1", color="black")+
    #     # geom_smooth(se=F, method="lm")+
    #     geom_abline(aes(intercept=get(paste0("MKinter_",str_split(var, "_", n=2)[[1]][2])),slope=get(paste0("MKSlope_",str_split(var, "_", n=2)[[1]][2]))), color="blue", size=1)+
    #     theme(legend.position = "none")+
    #     facet_wrap(~Site.Code)+geom_point(size=2) +labs(y="Overall B-IBI Score")+
    #     theme(text = element_text(size = 28))
    #   
    #   ggsave(paste0("Sitetrend_", each, ".png"), plot = pp, width=20, height=10)
    #   # print(pp)
    # }
    # 
    setwd(here::here())
    setwd("./inputs")
    lulc<-read.csv("NLCD_basin.csv", header = T)
    lulc<-subset(lulc, lulc$Name!="09SOO1209")
    BIBI<-merge(bibi.lr, lulc, by.x=c('Site.Code', "Year"), by.y=c('Name', "Year"))
    
    ggplot(BIBI, aes(y=Overall.Score, x=per_Urban_, color=Year))+geom_point(size=2, alpha=.9)+
      scale_color_distiller(palette = "PiYG")+ labs(x="Percent Urban", y="B-IBI Score")+
      theme_dark()+
      # geom_smooth(data=BIBI, mapping=aes(y=Overall.Score, x=per_Urban_x,color=Year), method="lm")
      geom_smooth(data=subset(BIBI, Year==2002), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2003), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2004), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2005), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2006), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2007), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2008), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2009), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2010), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2011), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2012), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2013), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2014), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2015), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2016), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2017), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2018), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2019), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2020), method="lm", se=F)+
      geom_smooth(data=subset(BIBI, Year==2021), method="lm", se=F)+
      theme(text = element_text(size = 28))
    setwd(here::here())
    
    setwd("./outputs")
    ggsave("BIBI_Urban_pres_03092023_2002-2021_LRexclude.png", width=20, height=10)
    
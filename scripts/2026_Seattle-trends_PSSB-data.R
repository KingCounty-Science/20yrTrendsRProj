#--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# Beka Stiling created this script 2026-02-04 
# Project: Seattle trends
# Goal: The goal of this script is to calculate any trends at the Seattle sites
#--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 

library(Microsoft365R) #for reading file from sharepoint
library(readxl) #for reading excel file
library(janitor) #clean up name
library(tidyverse) #for data tidying and wrangling
library(rkt) #for Mann Kendall

## read in Kate's list of Seattle Sites

##read in the dataset Kate sent for comparison (2026-01-2029)
#### Load ISP site pebble data from Microsoft Teams/Sharepoint ####
site <- get_sharepoint_site(site_url = "[saved in local environment]") #this project is public

# default is the document library
drv <- site$get_drive("LSSG Files")

# This downloads the file to the project folder (I had to know the pathway from Teams/Sharepoint)
drv$download_file("Freshwater/Streams/Freshwater Macroinvertebrate Program/Contracts/Seattle_bugs/2025_Report_Status_2020-2024/PSSB_download_fourways_2002_2024.xlsx")

# Review the sheet names in order to select the correct one.  
excel_sheets("PSSB_download_fourways_2002_2024.xlsx")

# Read in the data,Just pebble and canopy (may change in the future)
PSSB_4ways <-read_excel("PSSB_download_fourways_2002_2024.xlsx", 
                        sheet = "DATA_USE", 
                        .name_repair = make_clean_names) # skips the first line which does NOT contain data
# Removes the file from my working directory. 
file.remove("PSSB_download_fourways_2002_2024.xlsx")

#Determine which sites have more than 2 years of data.
enough.years<-PSSB_4ways |> group_by(download_specs, site_code) |> count(site_code) |> filter(n >2) |> select(site_code) |> pull()
bigdf <- PSSB_4ways |> filter(site_code %in% enough.years) #filter the data to include only sites with enough years

results<-  bigdf |> 
  group_by(download_specs, site_code) |> #group the data by download style and site
  mutate(mk.tau=kendallTrendTest(overall_score ~ year)$estimate[1]) |> #for each download style and site, calculate mk metrics (tau)
  mutate(mk.Sen=kendallTrendTest(overall_score ~ year)$estimate[2]) |>  #Sen
  mutate(mk.pval=kendallTrendTest(overall_score ~ year)$p.value) |> # pvalue
  select(site_code, download_specs, mk.tau,mk.Sen, mk.pval)  |> #then simplify so that each site and download has only one row of data
  unique() |> 
  mutate(mk.trend = if(mk.pval < 0.05 & mk.tau < 0){"negative"} #add a column that evaluates if it is negative
         else if(mk.pval < 0.05 & mk.tau > 0){"positive"} #positive
         else{"none"}) |> #or neither
  pivot_wider(names_from = download_specs, values_from = c(mk.tau, mk.Sen, mk.pval, mk.trend)) #then pivot the data to WIDE format for comparison

#write out the results. 
write.csv(results, "Site_Trends_4ways_2002-2021_20260205.csv")

####SCRAP PAPER IS BELOW. DELETE AFTER CONFIRMING ALL WORKS #####



PSSB_4way_names<-PSSB_4ways |> select(download_specs) |> distinct() |> pull()

PSSB_4ways$site_code_factor <- as.numeric(as_factor(PSSB_4ways$site_code))

PSSB_groups<-PSSB_4ways |> group_by(download_specs)

Coarse_Mapped_FullyRandom<-group_split(PSSB_groups)[[1]]
Coarse_NotMapped_SemiRan<-group_split(PSSB_groups)[[2]]
Meta_Mapped_FullyRandom  <-group_split(PSSB_groups)[[3]]
Meta_NotMapped_SemiRan <-group_split(PSSB_groups)[[4]]

mk_CMF<-rkt(date= Coarse_Mapped_FullyRandom$year, 
    y = Coarse_Mapped_FullyRandom$overall_score, 
    block = Coarse_Mapped_FullyRandom$site_code_factor,
    correct= F,
    rep = "a")
print(mk_CMF)

mk_CNS <-rkt(date= Coarse_NotMapped_SemiRan$year, 
             y = Coarse_NotMapped_SemiRan$overall_score, 
             block = Coarse_NotMapped_SemiRan$site_code_factor,
             correct= F,
             rep = "a")
print(mk_CNS)

mk_MMF<-rkt(date= Meta_Mapped_FullyRandom$year, 
            y = Meta_Mapped_FullyRandom$overall_score, 
            block = Meta_Mapped_FullyRandom$site_code_factor,
            correct= F,
            rep = "a")
print(mk_MMF)

mk_MNS <-rkt(date= Meta_NotMapped_SemiRan$year, 
             y = Meta_NotMapped_SemiRan$overall_score, 
             block = Meta_NotMapped_SemiRan$site_code_factor,
             correct= F,
             rep = "a")
print(mk_MNS)

singlesite <- Coarse_Mapped_FullyRandom |> filter(site_code_factor == 1)

library(EnvStats)
kendallTrendTest(singlesite$overall_score~singlesite$year)$estimate #gives me tau, Sen, and intercept
kendallTrendTest(singlesite$overall_score~singlesite$year)$p.value

#Beth's script starts with linear regressions, then she sores the slope, 

output <-ddply(Coarse_Mapped_FullyRandom,.(get(singlesite)),mutate, mk.tau=kendallTrendTest( overall_score ~ as.numeric(year))$estimate[1])

##get rid of data that there is not enough of
enough.years <- Coarse_Mapped_FullyRandom |> group_by(site_code) |> count(site_code) |> filter(n > 2) |> select(site_code) |> pull()

Coarse_Mapped_FullyRandom.enoughyears <- Coarse_Mapped_FullyRandom |> filter(site_code %in% enough.years)

df <- Coarse_Mapped_FullyRandom.enoughyears |> group_by(site_code) |> 
  mutate(mk.tau=kendallTrendTest(overall_score ~ year)$estimate[1]) |> 
  mutate(mk.Sen=kendallTrendTest(overall_score ~ year)$estimate[2]) |> 
  mutate(mk.pval=kendallTrendTest(overall_score ~ year)$p.value) |> 
  select(site_code, download_specs, mk.tau,mk.Sen, mk.pval)  |> 
  unique() |> 
  mutate(mk.trend = if(mk.pval < 0.05 & mk.tau < 0){"negative"} 
         else if(mk.pval < 0.05 & mk.tau > 0){"positive"} 
         else{"none"})
  

colnames(df)
overall_trends <- df |> select(site_code, download_specs, mk.tau)  |> unique()

#refactor everything above to avoid cutting/pasting.  

 

##reuse Beth's structure
###BEth Site Trends ####
#####Site trends ####
library(EnvStats)
scal<-"Site.Code" # "Site.Code" or "Subbasin"
# form<-"Overall.Score"
form<-names(bibi.lr)[9:(ncol(bibi.lr)-2)] ##Beka, this is the variable is the score.
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

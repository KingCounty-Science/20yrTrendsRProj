#--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# Beka Stiling created this script 2026-02-04 
# Project: Seattle trends
# Goal: The goal of this script is to calculate any trends at the Seattle sites
#--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 

library(Microsoft365R) #for reading file from sharepoint
library(readxl) #for reading excel file
library(janitor) #clean up name
library(tidyverse) #for data tidying and wrangling
library(rkt) #for Mann Kendall of the overall data sets
library(EnvStats) #for site trends script

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


### evaluate overall trend, blocked by site, no pre-filtering for low number of years ####
PSSB_4ways$site_code_factor <- as.numeric(as_factor(PSSB_4ways$site_code)) # the rkt script requires block be numeric

PSSB_groups<-PSSB_4ways |> group_by(download_specs) #review the groups of data

#separate out the groups into small dataframes
Coarse_Mapped_FullyRandom<-group_split(PSSB_groups)[[1]]
Coarse_NotMapped_SemiRan<-group_split(PSSB_groups)[[2]]
Meta_Mapped_FullyRandom  <-group_split(PSSB_groups)[[3]]
Meta_NotMapped_SemiRan <-group_split(PSSB_groups)[[4]]

#test each dataframe for regional trend blocked by site.
mk_CMF<-rkt(date= Coarse_Mapped_FullyRandom$year, 
            y = Coarse_Mapped_FullyRandom$overall_score, 
            block = Coarse_Mapped_FullyRandom$site_code_factor,
            correct= F,
            rep = "a")
mk_CMF

mk_CNS <-rkt(date= Coarse_NotMapped_SemiRan$year, 
             y = Coarse_NotMapped_SemiRan$overall_score, 
             block = Coarse_NotMapped_SemiRan$site_code_factor,
             correct= F,
             rep = "a")
mk_CNS

mk_MMF<-rkt(date= Meta_Mapped_FullyRandom$year, 
            y = Meta_Mapped_FullyRandom$overall_score, 
            block = Meta_Mapped_FullyRandom$site_code_factor,
            correct= F,
            rep = "a")
mk_MMF

mk_MNS <-rkt(date= Meta_NotMapped_SemiRan$year, 
             y = Meta_NotMapped_SemiRan$overall_score, 
             block = Meta_NotMapped_SemiRan$site_code_factor,
             correct= F,
             rep = "a")
mk_MNS

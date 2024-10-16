setwd(here::here())
setwd("./inputs")

categories<-c(  "22"="Urban", "23"="Urban", "24"="Urban", "21"="Open Spaces Developed", 
           "81"="Agricultural", "82"="Agricultural",  "41"="Forest", "42"="Forest", 
           "43"="Forest", "52"="Scrub-Shrub", "71"="Grassland/Herbaceous", "90"="Woody Wetlands", "95"="Emergent Herbaceous Wetlands", 
            "11"="Open Water", 
            "31"="Barren Land", "12"="Perennial Snow/Ice")

maxYear<-2021#max year to consider for %LU change
minYear<-2001
# if(!exists("LULCBind", mode="function")) source ("PSSB_utilites.r")
# lulc<-LULCBind(file.p, categ, minYear, maxYear)

library(openxlsx)
library(stringr)
library(plyr)

file.path<-"./NLCD 2001to2019 183Basins 181Buffers"

# LULCBind <- function(file.path, categories=NULL, minYear, maxYear) {
  # read in files
  list.with.each.file <- sapply(paste(file.path,"/", list.files(file.path, "Excel.xlsx"), sep = ''), function(y) openxlsx::read.xlsx(y, sheet = 1), simplify = FALSE)
  # Get rid of Shape_Leng and Shape_Area that some of the .txts had
  # list.2 <- lapply(list.with.each.file, function(x) {x[c(1,2,3, match("Acres", colnames(x)))]})
  #bind into dataframe
  res<-do.call("rbind.data.frame", list.with.each.file)
  loc<-str_locate(row.names(res), "NLCD\\d{4}_")# The year in the file name needs to be surrounded by underscores
  res$year<-str_sub(row.names(res), loc[,1], loc[,2]-1)
  res$year<-str_replace(res$year, "NLCD", "")
  type<-str_locate(row.names(res), "18\\d{1}\\w{1,}_")
  res$type<-str_sub(row.names(res), type[,1], type[,2]-1)
  res$type<-str_replace(res$type, "183", "")
  res$type<-str_replace(res$type, "181", "")
  res$type<-str_replace(res$type, "_Pivot", "")
  rownames(res)<-NULL
  res<-res[,-1]
  res<-reshape2::melt(res, id.vars=c("Name", "year", "type"), variable.name="CODE", value.name = "Acres")
  res$CODE<-str_replace(res$CODE, "gridcode", "")
  res$CODE2<-revalue(res$CODE, categories)
  res_buff<-subset(res, type=="Buffers")
  res_basin<-subset(res, type=="Basins")
  
  
  res_basin$Acres[is.na(res_basin$Acres)]<-0
  
  res2<-reshape2::dcast(res_basin, Name~CODE2+year, sum, value.var = "Acres")
  res3<-reshape2::dcast(res_basin, Name~year, sum, value.var="Acres")
  res4<-merge(res2, res3, by="Name")
  
names(res4)
    
    for (i in which(colnames(res4)==paste0(minYear)):ncol(res4)) { ## select columns "1992" onward
      print(i)
      for (column in str_subset(colnames(res4), paste0("_",colnames(res4[i])))) { ## look at the columns up until the total years start
        print(column)
        res4[ncol(res4)+1]<-res4[c(str_subset(colnames(res4), column))]/res4[i]*100
        colnames(res4)[ncol(res4)]<-paste0("per_", column)
      }
    }
    test<-str_match(colnames(res4), "(per.*)_.*")
    test<-na.omit(test[,2])
    test<-unique(test)
    for(item in unique(test)){
      res4[ncol(res4)+1]<-res4[c(str_subset(colnames(res4), paste0(item, "_", maxYear)))] - res4[c(str_subset(colnames(res4), paste0(item, "_", minYear)))]
      colnames(res4)[ncol(res4)]<-paste0("Change_", item)
    }

    setwd(here::here())
    setwd("./outputs")
write.csv(res4, paste0("NLCD_LULC_2001-2021.csv"))
basins<-res4

res2<-reshape2::dcast(res_buff, Name~CODE2+year, sum, value.var = "Acres")
res3<-reshape2::dcast(res_buff, Name~year, sum, value.var="Acres")
res4<-merge(res2, res3, by="Name")

names(res4)

for (i in which(colnames(res4)==paste0(minYear)):ncol(res4)) { ## select columns "1992" onward
  print(i)
  for (column in str_subset(colnames(res4), paste0("_",colnames(res4[i])))) { ## look at the columns up until the total years start
    print(column)
    res4[ncol(res4)+1]<-res4[c(str_subset(colnames(res4), column))]/res4[i]*100
    colnames(res4)[ncol(res4)]<-paste0("per_", column)
  }
}
test<-str_match(colnames(res4), "(per.*)_.*")
test<-na.omit(test[,2])
test<-unique(test)
for(item in unique(test)){
  res4[ncol(res4)+1]<-res4[c(str_subset(colnames(res4), paste0(item, "_", maxYear)))] - res4[c(str_subset(colnames(res4), paste0(item, "_", minYear)))]
  colnames(res4)[ncol(res4)]<-paste0("Change_", item)
}


write.csv(res4, paste0("/NLCD_LULC_2001-2019_BUFFERS.csv"))
buffers<-res4



###################


rownames(buffers)<-buffers$Name
# buffers$per_Urban_OS_2019<-buffers$per_Urban_2019+buffers$`per_Open Spaces Developed_2016`
# buffers$per_Urban_OS_2001<-buffers$per_Urban_2001+buffers$`per_Open Spaces Developed_2001`
# buffers$Change_per_Urban_OS<-buffers$per_Urban_OS_2019-buffers$per_Urban_OS_2001

# 

names(buffers)
LURB<-buffers[,c(98:196)]
categ<-unique(str_remove_all(names(LURB), "[:digit:]"))
categ<-str_subset(categ, "^per_")

categ<-categ[1:11]

names(LURB)
# each<-categ[1]
for (each in categ){
  LURB<-mutate(LURB, new=(((get(paste0(each, "2001"))*2)+(get(paste0(each, "2004"))*1))/3))
  names(LURB)[ncol(LURB)]<-paste0(each, "2002")
  LURB<-mutate(LURB, new=(((get(paste0(each, "2001"))*1)+(get(paste0(each, "2004"))*2))/3))
  names(LURB)[ncol(LURB)]<-paste0(each, "2003")
  
  LURB<-mutate(LURB, new=(((get(paste0(each, "2004"))*1)+(get(paste0(each, "2006"))*1))/2))
  names(LURB)[ncol(LURB)]<-paste0(each, "2005")
  
  LURB<-mutate(LURB, new=(((get(paste0(each, "2006"))*1)+(get(paste0(each, "2008"))*1))/2))
  names(LURB)[ncol(LURB)]<-paste0(each, "2007")
  
  LURB<-mutate(LURB, new=(((get(paste0(each, "2008"))*2)+(get(paste0(each, "2011"))*1))/3))
  names(LURB)[ncol(LURB)]<-paste0(each, "2009")
  LURB<-mutate(LURB, new=(((get(paste0(each, "2008"))*1)+(get(paste0(each, "2011"))*2))/3))
  names(LURB)[ncol(LURB)]<-paste0(each, "2010")
  
  LURB<-mutate(LURB, new=(((get(paste0(each, "2011"))*1)+(get(paste0(each, "2013"))*1))/2))
  names(LURB)[ncol(LURB)]<-paste0(each, "2012")
  
  LURB<-mutate(LURB, new=(((get(paste0(each, "2013"))*2)+(get(paste0(each, "2016"))*1))/3))
  names(LURB)[ncol(LURB)]<-paste0(each, "2014")
  LURB<-mutate(LURB, new=(((get(paste0(each, "2013"))*1)+(get(paste0(each, "2016"))*2))/3))
  names(LURB)[ncol(LURB)]<-paste0(each, "2015")

  LURB<-mutate(LURB, new=(((get(paste0(each, "2016"))*2)+(get(paste0(each, "2019"))*1))/3))
  names(LURB)[ncol(LURB)]<-paste0(each, "2017")
  LURB<-mutate(LURB, new=(((get(paste0(each, "2016"))*1)+(get(paste0(each, "2019"))*2))/3))
  names(LURB)[ncol(LURB)]<-paste0(each, "2018")
  
  
  LURB<-mutate(LURB, new=(((get(paste0(each, "2019"))-get(paste0(each, "2001")))/18)*19)+get(paste0(each, "2001")))
  names(LURB)[ncol(LURB)]<-paste0(each, "2020")
  LURB<-mutate(LURB, new=(((get(paste0(each, "2019"))-get(paste0(each, "2001")))/18)*20)+get(paste0(each, "2001")))
  names(LURB)[ncol(LURB)]<-paste0(each, "2021")
  LURB<-mutate(LURB, new=(((get(paste0(each, "2019"))-get(paste0(each, "2001")))/18)*21)+get(paste0(each, "2001")))
  names(LURB)[ncol(LURB)]<-paste0(each, "2022")

}
LURB$Name<-rownames(LURB)
LURB3<-subset(LURB, select=c(names(LURB)[str_which(names(LURB), categ[2])], "Name"))
LURB3<-reshape2::melt(LURB3, id.vars="Name", vaLURBe.name=categ[2])
LURB3$Year<-str_sub(LURB3$variable, str_length(LURB3$variable)-3, str_length(LURB3$variable))
LURB3<-subset(LURB3, select=c(Name, Year))



# each<-categ[10]
for (each in categ){
  LURB2<-subset(LURB, select=c(names(LURB)[str_which(names(LURB), paste0(each))], "Name"))
  LURB2<-reshape2::melt(LURB2, id.vars="Name", value.name=each)
  LURB2$Year<-str_sub(LURB2$variable, str_length(LURB2$variable)-3, str_length(LURB2$variable))
  LURB2<-subset(LURB2, select=-c(variable))
  LURB3<-merge(LURB3, LURB2, by.x=c("Name", "Year"), by.y=c("Name", "Year"))
  rm(LURB2)
}

names(LURB3)<-paste0(names(LURB3), "RB")
write.csv(LURB3,"NLCD_RB.csv")

###################

names(basins)
rownames(basins)<-basins$Name

LU<-basins[,c(98:196)]
categ<-unique(str_remove_all(names(LU), "[:digit:]"))
categ<-str_subset(categ, "^per_")
# categ<-categ[2:10]


names(LU)
# each<-categ[1]
for (each in categ){
  LU<-mutate(LU, new=(((get(paste0(each, "2001"))*2)+(get(paste0(each, "2004"))*1))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2002")
  LU<-mutate(LU, new=(((get(paste0(each, "2001"))*1)+(get(paste0(each, "2004"))*2))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2003")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2004"))*1)+(get(paste0(each, "2006"))*1))/2))
  names(LU)[ncol(LU)]<-paste0(each, "2005")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2006"))*1)+(get(paste0(each, "2008"))*1))/2))
  names(LU)[ncol(LU)]<-paste0(each, "2007")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2008"))*2)+(get(paste0(each, "2011"))*1))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2009")
  LU<-mutate(LU, new=(((get(paste0(each, "2008"))*1)+(get(paste0(each, "2011"))*2))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2010")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2011"))*1)+(get(paste0(each, "2013"))*1))/2))
  names(LU)[ncol(LU)]<-paste0(each, "2012")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2013"))*2)+(get(paste0(each, "2016"))*1))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2014")
  LU<-mutate(LU, new=(((get(paste0(each, "2013"))*1)+(get(paste0(each, "2016"))*2))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2015")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2016"))*2)+(get(paste0(each, "2019"))*1))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2017")
  LU<-mutate(LU, new=(((get(paste0(each, "2016"))*1)+(get(paste0(each, "2019"))*2))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2018")
  
  
  LU<-mutate(LU, new=(((get(paste0(each, "2019"))-get(paste0(each, "2001")))/18)*19)+get(paste0(each, "2001")))
  names(LU)[ncol(LU)]<-paste0(each, "2020")
  LU<-mutate(LU, new=(((get(paste0(each, "2019"))-get(paste0(each, "2001")))/18)*20)+get(paste0(each, "2001")))
  names(LU)[ncol(LU)]<-paste0(each, "2021")
  LU<-mutate(LU, new=(((get(paste0(each, "2019"))-get(paste0(each, "2001")))/18)*21)+get(paste0(each, "2001")))
  names(LU)[ncol(LU)]<-paste0(each, "2022")
  
}

LU$Name<-rownames(LU)
LU3<-subset(LU, select=c(names(LU)[str_which(names(LU), categ[2])], "Name"))
LU3<-reshape2::melt(LU3, id.vars="Name", vaLUe.name=categ[2])
LU3$Year<-str_sub(LU3$variable, str_length(LU3$variable)-3, str_length(LU3$variable))
LU3<-subset(LU3, select=c(Name, Year))



# each<-categ[10]
for (each in categ){
  LU2<-subset(LU, select=c(names(LU)[str_which(names(LU), paste0(each))], "Name"))
  LU2<-reshape2::melt(LU2, id.vars="Name", value.name=each)
  LU2$Year<-str_sub(LU2$variable, str_length(LU2$variable)-3, str_length(LU2$variable))
  LU2<-subset(LU2, select=-c(variable))
  LU3<-merge(LU3, LU2, by.x=c("Name", "Year"), by.y=c("Name", "Year"))
  rm(LU2)
}

names(LU3)
write.csv(LU3,"NLCD_basin.csv")


#######impervious basin############
setwd(here::here())
setwd("./inputs")
Imperv_basins<-read_xlsx(paste0( file.path, "/Stats_PctDev_Basins_2001to2019.xlsx"), sheet = 1, skip = 6)
Imperv_basins<-as.data.frame(Imperv_basins)
names(Imperv_basins)
rownames(Imperv_basins)<-Imperv_basins$Name

LU<-Imperv_basins[,c(2:9)]
categ<-unique(str_remove_all(names(LU), "[:digit:]"))



names(LU)
# each<-categ[1]
for (each in categ){
  LU<-mutate(LU, new=(((get(paste0(each, "2001"))*2)+(get(paste0(each, "2004"))*1))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2002")
  LU<-mutate(LU, new=(((get(paste0(each, "2001"))*1)+(get(paste0(each, "2004"))*2))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2003")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2004"))*1)+(get(paste0(each, "2006"))*1))/2))
  names(LU)[ncol(LU)]<-paste0(each, "2005")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2006"))*1)+(get(paste0(each, "2008"))*1))/2))
  names(LU)[ncol(LU)]<-paste0(each, "2007")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2008"))*2)+(get(paste0(each, "2011"))*1))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2009")
  LU<-mutate(LU, new=(((get(paste0(each, "2008"))*1)+(get(paste0(each, "2011"))*2))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2010")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2011"))*1)+(get(paste0(each, "2013"))*1))/2))
  names(LU)[ncol(LU)]<-paste0(each, "2012")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2013"))*2)+(get(paste0(each, "2016"))*1))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2014")
  LU<-mutate(LU, new=(((get(paste0(each, "2013"))*1)+(get(paste0(each, "2016"))*2))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2015")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2016"))*2)+(get(paste0(each, "2019"))*1))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2017")
  LU<-mutate(LU, new=(((get(paste0(each, "2016"))*1)+(get(paste0(each, "2019"))*2))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2018")
  
  
  LU<-mutate(LU, new=(((get(paste0(each, "2019"))-get(paste0(each, "2001")))/18)*19)+get(paste0(each, "2001")))
  names(LU)[ncol(LU)]<-paste0(each, "2020")
  LU<-mutate(LU, new=(((get(paste0(each, "2019"))-get(paste0(each, "2001")))/18)*20)+get(paste0(each, "2001")))
  names(LU)[ncol(LU)]<-paste0(each, "2021")
  LU<-mutate(LU, new=(((get(paste0(each, "2019"))-get(paste0(each, "2001")))/18)*21)+get(paste0(each, "2001")))
  names(LU)[ncol(LU)]<-paste0(each, "2022")
  
}

LU$Name<-rownames(LU)
LU3<-subset(LU, select=c(names(LU)[str_which(names(LU), categ[1])], "Name"))
LU3<-reshape2::melt(LU3, id.vars="Name", vaLUe.name=categ[1])
LU3$Year<-str_sub(LU3$variable, str_length(LU3$variable)-3, str_length(LU3$variable))
LU3<-subset(LU3, select=c(Name, Year))



# each<-categ[10]
for (each in categ){
  LU2<-subset(LU, select=c(names(LU)[str_which(names(LU), paste0(each))], "Name"))
  LU2<-reshape2::melt(LU2, id.vars="Name", value.name=each)
  LU2$Year<-str_sub(LU2$variable, str_length(LU2$variable)-3, str_length(LU2$variable))
  LU2<-subset(LU2, select=-c(variable))
  LU3<-merge(LU3, LU2, by.x=c("Name", "Year"), by.y=c("Name", "Year"))
  rm(LU2)
}

names(LU3)
setwd(here::here())
setwd("./outputs")
write.csv(LU3,"NLCD_basin_impervious.csv")

#######impervious buffer############
setwd(here::here())
setwd("./inputs")
Imperv_buffers<-read_xlsx(paste0( file.path, "/Stats_PctDev_buffers_2001to2019.xlsx"), sheet = 1, skip = 6)
Imperv_buffers<-as.data.frame(Imperv_buffers)
names(Imperv_buffers)
rownames(Imperv_buffers)<-Imperv_buffers$Name

LU<-Imperv_buffers[,c(2:9)]
categ<-unique(str_remove_all(names(LU), "[:digit:]"))



names(LU)
# each<-categ[1]
for (each in categ){
  LU<-mutate(LU, new=(((get(paste0(each, "2001"))*2)+(get(paste0(each, "2004"))*1))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2002")
  LU<-mutate(LU, new=(((get(paste0(each, "2001"))*1)+(get(paste0(each, "2004"))*2))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2003")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2004"))*1)+(get(paste0(each, "2006"))*1))/2))
  names(LU)[ncol(LU)]<-paste0(each, "2005")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2006"))*1)+(get(paste0(each, "2008"))*1))/2))
  names(LU)[ncol(LU)]<-paste0(each, "2007")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2008"))*2)+(get(paste0(each, "2011"))*1))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2009")
  LU<-mutate(LU, new=(((get(paste0(each, "2008"))*1)+(get(paste0(each, "2011"))*2))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2010")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2011"))*1)+(get(paste0(each, "2013"))*1))/2))
  names(LU)[ncol(LU)]<-paste0(each, "2012")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2013"))*2)+(get(paste0(each, "2016"))*1))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2014")
  LU<-mutate(LU, new=(((get(paste0(each, "2013"))*1)+(get(paste0(each, "2016"))*2))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2015")
  
  LU<-mutate(LU, new=(((get(paste0(each, "2016"))*2)+(get(paste0(each, "2019"))*1))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2017")
  LU<-mutate(LU, new=(((get(paste0(each, "2016"))*1)+(get(paste0(each, "2019"))*2))/3))
  names(LU)[ncol(LU)]<-paste0(each, "2018")
  
  
  LU<-mutate(LU, new=(((get(paste0(each, "2019"))-get(paste0(each, "2001")))/18)*19)+get(paste0(each, "2001")))
  names(LU)[ncol(LU)]<-paste0(each, "2020")
  LU<-mutate(LU, new=(((get(paste0(each, "2019"))-get(paste0(each, "2001")))/18)*20)+get(paste0(each, "2001")))
  names(LU)[ncol(LU)]<-paste0(each, "2021")
  LU<-mutate(LU, new=(((get(paste0(each, "2019"))-get(paste0(each, "2001")))/18)*21)+get(paste0(each, "2001")))
  names(LU)[ncol(LU)]<-paste0(each, "2022")
  
}

LU$Name<-rownames(LU)
LU3<-subset(LU, select=c(names(LU)[str_which(names(LU), categ[1])], "Name"))
LU3<-reshape2::melt(LU3, id.vars="Name", vaLUe.name=categ[1])
LU3$Year<-str_sub(LU3$variable, str_length(LU3$variable)-3, str_length(LU3$variable))
LU3<-subset(LU3, select=c(Name, Year))



# each<-categ[10]
for (each in categ){
  LU2<-subset(LU, select=c(names(LU)[str_which(names(LU), paste0(each))], "Name"))
  LU2<-reshape2::melt(LU2, id.vars="Name", value.name=each)
  LU2$Year<-str_sub(LU2$variable, str_length(LU2$variable)-3, str_length(LU2$variable))
  LU2<-subset(LU2, select=-c(variable))
  LU3<-merge(LU3, LU2, by.x=c("Name", "Year"), by.y=c("Name", "Year"))
  rm(LU2)
}

names(LU3)
setwd(here::here())
setwd("./outputs")
write.csv(LU3,"NLCD_buffer_impervious.csv")

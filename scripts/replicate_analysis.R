
setwd(here::here())
setwd("./outputs")
OTU_collapsed5<-read.csv("Collapsed_Coarse_Taxa_reps_wrapped_in_LR_exclude.csv")
# OTU_collapsed5<-read.csv("Collapsed_Coarse_Taxa_reps_wrapped_in.csv")
reps<-read.csv("replicates_for_sqft_analysis.csv")
library(plyr)
ddply(reps, .(Year), summarize, tally=length(Visit.ID))

OTU_collapsed5<-ddply(OTU_collapsed5, .(Visit.ID), mutate, sumorgs=sum(Quantity_OTU))
# OTU_collapsed5<-subset(OTU_collapsed5, Unique_OTU==TRUE)
OTU_collapsed4<-read.csv("Collapsed_Coarse_Taxa_LR_exclude.csv")
# OTU_collapsed4<-read.csv("Collapsed_Coarse_Taxa.csv")
OTU_collapsed4<-ddply(OTU_collapsed4, .(Sample.Code), mutate, sumorgs=sum(Quantity_OTU))
presamp_richness_sep<-ddply(OTU_collapsed4, .(Sample.Code, Visit.ID), summarize, ntaxa=length(OTU_COARSE))
presamp_richness_wrap<-ddply(OTU_collapsed5, .(Visit.ID), summarize, ntaxa=length(OTU_COARSE))

presam_richness<-merge(presamp_richness_sep, presamp_richness_wrap, by="Visit.ID")
presam_richness<-subset(presam_richness, Visit.ID %in% reps$Visit.ID)


# OTU_collapsed4<-subset(OTU_collapsed4, Unique_OTU==TRUE)
# OTU_collapsed4<-ddply(OTU_collapsed4, .(Sample.Code), mutate, sumorgs2=sum(Quantity_OTU))
# unique(subset(OTU_collapsed4, sumorgs>500&sumorgs2<500)$Sample.Code)
###Rarify####

# visit<-116
KC_rarified<-OTU_collapsed5
KC_rarified<-KC_rarified[0,]
set.seed(17760704)
for(visit in unique(OTU_collapsed5$Visit.ID)){
  print(visit)
  test<-subset(OTU_collapsed5, Visit.ID==visit)
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
  

KC_rarified2<-OTU_collapsed4
KC_rarified2<-KC_rarified2[0,]
set.seed(17760704)
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
  KC_rarified2<-rbind(subsamp_comp, KC_rarified2)
}

# OTU_collapsed6<-ddply(KC_rarified2, .(OTU_COARSE,WRIA.Number, Basin, 
#                                         Subbasin, Stream.or.River, Project, Visit.Date, 
#                                         Year, Latitude, Longitude, Lab.Name, Site.Code, Visit.ID
# ), summarize, Quantity_OTU = sum(Quantity_OTU), Unique_OTU=any(Unique_OTU))
# 
# OTU_collapsed6<-ddply(OTU_collapsed6, .(Visit.ID), mutate, sumorgs=sum(Quantity_OTU))
# 
# KC_rarified<-OTU_collapsed6
# KC_rarified<-KC_rarified[0,]
# set.seed(17760704)
# for(visit in unique(OTU_collapsed6$Visit.ID)){
#   print(visit)
#   test<-subset(OTU_collapsed6, Visit.ID==visit)
#   testsample<-rep(test$OTU_COARSE, test$Quantity_OTU)
#   if(sum(test$Quantity_OTU)>=450){
#     subsamp<-sample(x = testsample, size = 450,replace=F)
#   }
#   else {subsamp<-sample(x = testsample, size = sum(test$Quantity_OTU),replace=F)}
#   subsamp<-as.data.frame(table(subsamp))
#   names(subsamp)<-c("OTU_COARSE","Quantity_OTU")
#   subsamp_meta<-test[c(match(subsamp$OTU_COARSE, test$OTU_COARSE)),]
#   subsamp_meta<-subset(subsamp_meta, select=-c(Quantity_OTU))
#   subsamp_comp<-merge(subsamp_meta, subsamp, by="OTU_COARSE")
#   KC_rarified<-rbind(subsamp_comp, KC_rarified)
# }
# 


KC_rarified<-subset(KC_rarified, sumorgs>=900)
KC_rarified2<-subset(KC_rarified2, sumorgs>=450)

test<-ddply(KC_rarified, .(Visit.ID), summarize, ntaxa=length(unique(OTU_COARSE)))
ddply(KC_rarified, .(Visit.ID), summarize, ntaxa=sum(Quantity_OTU))
test2<-ddply(KC_rarified2, .(Sample.Code, Visit.ID), summarize, ntaxa=length(unique(OTU_COARSE)))
ddply(KC_rarified2, .(Sample.Code), summarize, ntaxa=sum(Quantity_OTU))
test3<-merge(test, test2, by="Visit.ID")
test_rep<-subset(test3, Visit.ID %in% reps$Visit.ID)
keep<-ddply(test_rep, .(Visit.ID), summarize, len=length(Sample.Code))
keep<-subset(keep, len>1)
test_rep<-subset(test_rep, Visit.ID %in% keep$Visit.ID)

ggplot(test_rep, aes(x=ntaxa.x, y=ntaxa.y))+geom_point()+geom_smooth(method="lm")+
  stat_poly_eq(formula = y ~ x, rr.digits = 2, coef.digits = 3, size = 4, color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~")), parse = TRUE) 

# ddply(OTU_collapsed3, .(Visit.ID), summarise, total=sum(Quantity_OTU))
# ddply(KC_rarified, .(Visit.ID), summarise, total=sum(Quantity_OTU))
# detach("package:reshape2", unload = TRUE)
detach("package:plyr", unload = TRUE)
library(dplyr)

Tot_Richness<-KC_rarified %>% dplyr::filter(Unique_OTU==TRUE)  %>% dplyr::group_by(Visit.ID) %>% dplyr::summarise(Total_Richness=length(OTU_COARSE))
E_Richness<-KC_rarified %>% dplyr::filter(Order=="Ephemeroptera", Unique_OTU==TRUE)  %>% dplyr::group_by(Visit.ID) %>% dplyr::summarise(E_Richness=length(OTU_COARSE))
P_Richness<-KC_rarified %>% dplyr::filter(Order=="Plecoptera", Unique_OTU==TRUE)  %>% dplyr::group_by(Visit.ID) %>% dplyr::summarise(P_Richness=length(OTU_COARSE))
T_Richness<-KC_rarified %>% dplyr::filter(Order=="Trichoptera", Unique_OTU==TRUE)  %>% dplyr::group_by(Visit.ID) %>% dplyr::summarise(T_Richness=length(OTU_COARSE))
Cling_Richness<-KC_rarified %>% dplyr::filter(`X2012.Clinger`==TRUE, Unique_OTU==TRUE)  %>% dplyr::group_by(Visit.ID) %>% dplyr::summarise(Clin_Richness=length(OTU_COARSE))
LL_Richness<-KC_rarified %>% dplyr::filter(`X2012.Long.Lived`==TRUE, Unique_OTU==TRUE)  %>% dplyr::group_by(Visit.ID) %>% dplyr::summarise(LL_Richness=length(OTU_COARSE))
Intol_Richness<-KC_rarified %>% dplyr::filter(`X2012.Intolerant`==TRUE, Unique_OTU==TRUE)  %>% dplyr::group_by(Visit.ID) %>% dplyr::summarise(Intol_Richness=length(OTU_COARSE))
Tot_Abund<-KC_rarified   %>% dplyr::group_by(Visit.ID) %>% dplyr::summarise(Tot_Abund=sum(Quantity_OTU)) 
Tol_Abund<-KC_rarified   %>% dplyr::group_by(Visit.ID)%>% dplyr::filter(`X2012.Tolerant`==TRUE) %>% dplyr::summarise(Tol_Abund=sum(Quantity_OTU))
Pred_Abund<-KC_rarified   %>% dplyr::group_by(Visit.ID)%>% dplyr::filter(`X2012.Predator`==TRUE) %>% dplyr::summarise(Pred_Abund=sum(Quantity_OTU))
Dom_abund<-KC_rarified  %>% dplyr::group_by(Visit.ID) %>% dplyr::arrange(desc(Quantity_OTU)) %>% dplyr::slice(1:3) %>% dplyr::summarise(Dom_Abund=sum(Quantity_OTU))

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

KC_results<-left_join(KC_results, unique(subset(OTU_collapsed5, select=c("Visit.ID","WRIA.Number", "Basin", 
                                                                         "Subbasin", "Stream.or.River", "Project", "Visit.Date", 
                                                                         "Year", "Lab.Name", "Site.Code"))), by="Visit.ID")
KC_results[KC_results$Visit.ID==946,]$Site.Code<-"09DUW0277"

KC_results_wrap<-KC_results


##############################

Tot_Richness<-KC_rarified2 %>% dplyr::filter(Unique_OTU==TRUE)  %>% dplyr::group_by(Sample.Code) %>% dplyr::summarise(Total_Richness=length(OTU_COARSE))
E_Richness<-KC_rarified2 %>% dplyr::filter(Order=="Ephemeroptera", Unique_OTU==TRUE)  %>% dplyr::group_by(Sample.Code) %>% dplyr::summarise(E_Richness=length(OTU_COARSE))
P_Richness<-KC_rarified2 %>% dplyr::filter(Order=="Plecoptera", Unique_OTU==TRUE)  %>% dplyr::group_by(Sample.Code) %>% dplyr::summarise(P_Richness=length(OTU_COARSE))
T_Richness<-KC_rarified2 %>% dplyr::filter(Order=="Trichoptera", Unique_OTU==TRUE)  %>% dplyr::group_by(Sample.Code) %>% dplyr::summarise(T_Richness=length(OTU_COARSE))
Cling_Richness<-KC_rarified2 %>% dplyr::filter(`X2012.Clinger`==TRUE, Unique_OTU==TRUE)  %>% dplyr::group_by(Sample.Code) %>% dplyr::summarise(Clin_Richness=length(OTU_COARSE))
LL_Richness<-KC_rarified2 %>% dplyr::filter(`X2012.Long.Lived`==TRUE, Unique_OTU==TRUE)  %>% dplyr::group_by(Sample.Code) %>% dplyr::summarise(LL_Richness=length(OTU_COARSE))
Intol_Richness<-KC_rarified2 %>% dplyr::filter(`X2012.Intolerant`==TRUE, Unique_OTU==TRUE)  %>% dplyr::group_by(Sample.Code) %>% dplyr::summarise(Intol_Richness=length(OTU_COARSE))
Tot_Abund<-KC_rarified2   %>% dplyr::group_by(Sample.Code) %>% dplyr::summarise(Tot_Abund=sum(Quantity_OTU)) 
Tol_Abund<-KC_rarified2   %>% dplyr::group_by(Sample.Code)%>% dplyr::filter(`X2012.Tolerant`==TRUE) %>% dplyr::summarise(Tol_Abund=sum(Quantity_OTU))
Pred_Abund<-KC_rarified2   %>% dplyr::group_by(Sample.Code)%>% dplyr::filter(`X2012.Predator`==TRUE) %>% dplyr::summarise(Pred_Abund=sum(Quantity_OTU))
Dom_abund<-KC_rarified2   %>% dplyr::group_by(Sample.Code) %>% dplyr::arrange(desc(Quantity_OTU)) %>% dplyr::slice(1:3) %>% dplyr::summarise(Dom_Abund=sum(Quantity_OTU))

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



KC_results<-left_join(KC_results, unique(subset(OTU_collapsed4, select=c("Sample.Code", "Visit.ID","WRIA.Number", "Basin", 
                                                                         "Subbasin", "Stream.or.River", "Project", "Visit.Date", 
                                                                         "Year", "Lab.Name", "Site.Code"))), by="Sample.Code")

KC_results[KC_results$Visit.ID==946,]$Site.Code<-"09DUW0277"
KC_results_SC<-KC_results


# KC_results<-left_join(KC_results, unique(subset(raw, select=c(Site.Code, Latitude, Longitude))), by="Site.Code")
# KC_results[KC_results$Visit.ID==946,]$Site.Code<-"09DUW0277"
# write.csv(KC_results, "B-IBI_results_1182023_2002-2021_reps_wrapped_in_noUNIQUE.csv")

# KC_wrap_results<-KC_results
# KC_results_SC<-KC_results
reps

rep_wrapped_scores<-subset(KC_results_wrap, Visit.ID %in% reps$Visit.ID)
# test<-subset(reps, !Visit.ID %in% KC_results_wrap$Visit.ID)

# KC_results_SC<-read.csv("B-IBI_results_2272023_2002-2021_Subsamp_NO_Unique.csv")
# KC_results_SC<-read.csv("B-IBI_results_1182023_2002-2021.csv")
# OTU_collapsed4<-read.csv("Collapsed_Coarse_Taxa.csv")
library(plyr)
OTU_collapsed4<-ddply(OTU_collapsed4, .(Visit.ID), mutate, sumorgs=sum(Quantity_OTU))
# testB<-ddply(OTU_collapsed4, .(Visit.ID), summarize, ntaxa=length(OTU_COARSE))
# testC<-ddply(OTU_collapsed4, .(Visit.ID), summarize, ntaxa_unique=length(unique(OTU_COARSE)))
# test<-merge(testA, testB, by="Visit.ID")
# test<-merge(test, testC, by="Visit.ID")
# test_rep<-subset(test, Visit.ID %in% reps$Visit.ID)

unique(OTU_collapsed4[OTU_collapsed4$Visit.ID %in% test$Visit.ID,"sumorgs"])
KC_results_SC<-left_join(KC_results_SC, unique(subset(OTU_collapsed4, select=c("Sample.Code", "sumorgs"))), by="Sample.Code")


OTU_collapsed4<-ddply(OTU_collapsed4, .(Sample.Code), mutate, sumorgs=sum(Quantity_OTU))
KC_results_SC<-left_join(KC_results_SC, unique(subset(OTU_collapsed4, select=c("Sample.Code", "sumorgs"))), by="Sample.Code")
KC_results_SC<-subset(KC_results_SC, sumorgs.y>=450)
KC_results_SC<-subset(KC_results_SC, sumorgs.x>=900)
# KC_results_SC<-subset(KC_results_SC, !is.na(Visit.ID))

rep_sep_scores<-subset(KC_results_SC, Visit.ID %in% reps$Visit.ID)
rep_sep_scores<-subset(rep_sep_scores, Tot_Abund>=450)
rep_wrapped_scores<-subset(rep_wrapped_scores, Tot_Abund>=450)
rep_wrapped_scores<-subset(rep_wrapped_scores, Visit.ID %in% rep_sep_scores$Visit.ID)
rep_sep_scores<-subset(rep_sep_scores, Visit.ID %in% rep_wrapped_scores$Visit.ID)

rep_wrapped_scores1<-subset(rep_wrapped_scores, select = c("Visit.ID", "Site.Code", "Year", "Overall.Score", "Total_Richness"))
rep_sep_scores1<-subset(rep_sep_scores, select = c("Visit.ID", "Site.Code",  "Year", "Overall.Score", "Total_Richness","Sample.Code"))
names(rep_sep_scores1)[4]<-"rep_sep_Overall.Score"
names(rep_wrapped_scores1)[4]<-"rep_wrap_Overall.Score"

scores<-merge(rep_sep_scores1, rep_wrapped_scores1, by=c("Site.Code", "Year", "Visit.ID"))
keep<-ddply(scores, .(Visit.ID), summarize, len=length(Sample.Code))
keep<-subset(keep, len>1)
scores<-subset(scores, Visit.ID %in% keep$Visit.ID)
presam_richness
merge(scores, presam_richness, by=c("Visit.ID","Sample.Code"))

ggplot(scores, aes(x=Year, y=rep_sep_Overall.Score, group=Year))+geom_boxplot()
ggplot(scores, aes(x=Year, y=rep_wrap_Overall.Score, group=Year))+geom_boxplot()
ggplot(scores, aes(x=Year, y=rep_sep_Overall.Score, group=Year))+geom_point()+facet_wrap(.~Site.Code)
ggplot(scores, aes(x=Year, y=rep_wrap_Overall.Score, group=Year))+geom_point()+facet_wrap(.~Site.Code)


ggplot(scores)+geom_point(aes(x=Year, y=rep_sep_Overall.Score, group=Year), color="red")+geom_smooth(aes(x=Year, y=rep_sep_Overall.Score),color="red", method="lm")+
  geom_point(aes(x=Year, y=rep_wrap_Overall.Score, group=Year), color="blue")+geom_smooth(aes(x=Year, y=rep_wrap_Overall.Score),color="blue", method="lm")+
  facet_wrap(.~Site.Code)+ylab("B-IBI Score")
ggsave("replicates_analysis_overtime.png", width=17, height=9)

scores$prepost<-ifelse(scores$Year>2012, "Post-2012", "Pre-2012")
check<-ddply(scores, .(Site.Code, Year, prepost), summarize, vardiff=abs(diff(rep_sep_Overall.Score)))
ggplot(check, aes(x=Year, y=vardiff, group=Year))+geom_boxplot()
ggplot(check, aes(x=prepost, y=vardiff, group=prepost))+geom_boxplot()
median(subset(check, Year>=2012)$vardiff)
median(subset(check, Year<2012)$vardiff)

check2<-ddply(check, .(Year), summarize, meanvar=mean(abs(vardiff)))
ggplot(check2, aes(x=Year, y=meanvar, group=Year))+geom_boxplot()
mean(subset(check2, Year>=2012)$meanvar)
mean(subset(check2, Year<2012)$meanvar)



scores2<-ddply(scores, .(Site.Code, Year, Visit.ID, prepost), mutate, max_sep=max(rep_sep_Overall.Score))
scores2<-subset(scores2, max_sep==rep_sep_Overall.Score)

scores3<-ddply(scores, .(Site.Code, Year, Visit.ID, prepost), mutate, min_sep=min(rep_sep_Overall.Score))
scores3<-subset(scores3, min_sep==rep_sep_Overall.Score)

scores4<-ddply(scores, .(Site.Code, Year, Visit.ID, prepost), mutate, mean_sep=mean(rep_sep_Overall.Score))
scores4<-unique(subset(scores4, select=c(Site.Code, Year, Visit.ID, rep_wrap_Overall.Score, mean_sep, prepost)))

ggplot(scores4, aes(x=mean_sep, y=rep_wrap_Overall.Score, color=factor(prepost, levels=c("Pre-2012", "Post-2012")), grp.label = prepost))+geom_point()+geom_smooth(method="lm")+#facet_grid(.~factor(prepost, levels=c("Pre-2012", "Post-2012")))+
  labs(x="Mean replicate B-IBI score", y="Pseudo-composite score")+ scale_color_discrete(name = "Time period")+
  stat_poly_eq(formula = y ~ x, rr.digits = 2, coef.digits = 3, size = 4, aes(label = paste( stat(grp.label), stat(eq.label), stat(rr.label), sep = "*\", \"*")), parse = TRUE) 
ggsave("replicates_analysis_meanrepVscomposite.png", width=6, height=4)


ggplot(scores3, aes(x=min_sep, y=rep_wrap_Overall.Score,color=factor(prepost, levels=c("Pre-2012", "Post-2012")), grp.label = prepost))+geom_point()+geom_smooth(method="lm")+#facet_grid(.~factor(prepost, levels=c("Pre-2012", "Post-2012")))+
  labs(x="Minimum replicate B-IBI score", y="Pseudo-composite score")+ scale_color_discrete(name = "Time period")+
  stat_poly_eq(formula = y ~ x, rr.digits = 2, coef.digits = 3, size = 4, aes(label = paste( stat(grp.label), stat(eq.label), stat(rr.label), sep = "*\", \"*")), parse = TRUE) 
ggsave("replicates_analysis_minrepVscomposite.png", width=6, height=4)


ggplot(scores2, aes(x=max_sep, y=rep_wrap_Overall.Score,  color=factor(prepost, levels=c("Pre-2012", "Post-2012")), grp.label = prepost))+geom_point()+geom_smooth(method="lm")+#facet_grid(.~factor(prepost, levels=c("Pre-2012", "Post-2012")))+
  labs(x="Maximum replicate B-IBI score", y="Pseudo-composite score")+ scale_color_discrete(name = "Time period")+
  stat_poly_eq(formula = y ~ x, rr.digits = 2, coef.digits = 3, size = 4, aes(label = paste( stat(grp.label), stat(eq.label), stat(rr.label), sep = "*\", \"*")), parse = TRUE) 
ggsave("replicates_analysis_maxrepVscomposite.png", width=6, height=4)


ggplot(subset(scores, Year<2012), aes(x=rep_sep_Overall.Score, y=rep_wrap_Overall.Score))+geom_point()+geom_smooth(method="lm")+
  stat_poly_eq(formula = y ~ x, rr.digits = 2, coef.digits = 3, size = 4, color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~")), parse = TRUE) 

ggplot(subset(scores, Year>2012), aes(x=rep_sep_Overall.Score, y=rep_wrap_Overall.Score))+geom_point()+geom_smooth(method="lm")+
  stat_poly_eq(formula = y ~ x, rr.digits = 2, coef.digits = 3, size = 4, color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~")), parse = TRUE) 

#################################################

# bibi<-read.csv("B-IBI_results_1182023_2002-2021.csv")
# 
# exclude<-c("09SOO1209") #Site to remove from trends analysis
# # bibi$Visit.Date<-bibi$Event.Date
# # bibi$Stream.or.River<-bibi$Stream
# 
# 
# #########  Format, remove data from wrong months, average scores from samples with same site+year, and subset B-IBI data to data since 2001 with more than 9 years of data
# bibi$month <- format(as.Date(bibi$Visit.Date, '%Y-%m-%d'), "%m") # add month column
# # bibi$jdate<-yday(as.Date(bibi$Visit.Date, '%m/%d/%Y'))
# # bibi$Year <- format(as.Date(bibi$Visit.Date, '%m/%d/%Y'), "%Y") # add year column
# bibi$sampnum <- paste(bibi$Site.Code, format(as.Date(bibi$Visit.Date, '%Y-%m-%d'), "%y"), sep = "_") # add sample identifier
# bibi <- droplevels(bibi[bibi$month %in% c('07', '08', '09', '10'),]) # exclude samples collected outside of July - Oct
# dups<-bibi[duplicated(bibi[,c("Project" , "WRIA.Number"  , "Basin" , "Subbasin" , "Stream.or.River" , "Site.Code" , "Year" , "sampnum")]),]
# # bibi <- aggregate(cbind(Latitude, Longitude, Visit.Date, Overall.Score, Tot_Richness_Score, E_Richness_Score, P_Richness_Score, T_Richness_Score, Cling_Richness_Score, LL_Richness_Score, Intol_Richness_Score, Dom_Percent_Score,  Pred_Percent_Score, Tol_Percent_Score) ~  Project + WRIA.Number  + Basin + Subbasin + Stream.or.River + Site.Code + Year + sampnum, data = bibi, FUN = "mean", na.action = na.exclude)
# # bibi <-aggregate(cbind( Overall.Score, Taxa.Richness.Score, Ephemeroptera.Richness.Score, Plecoptera.Richness.Score, Trichoptera.Richness.Score, Clinger.Richness.Score, `Long-Lived.Richness.Score`, Intolerant.Richness.Score, Percent.Dominant.Score,  Predator.Percent.Score, Tolerant.Percent.Score) ~  Project + WRIA.Number  + Basin + Subbasin + Stream.or.River + Site.Code + Year + sampnum, data = bibi, FUN = "mean", na.action = na.exclude)#+jdate
# bibi <-aggregate(cbind( Overall.Score, Tot_Richness_Score, E_Richness_Score, P_Richness_Score, T_Richness_Score, Cling_Richness_Score, LL_Richness_Score, Intol_Richness_Score, Dom_Percent_Score,  Pred_Percent_Score, Tol_Percent_Score) ~  Project + WRIA.Number  + Basin + Subbasin + Stream.or.River + Site.Code + Year + sampnum, data = bibi, FUN = "mean", na.action = na.exclude)#+jdate
# 
# ## need to exclude CAO samples because sampled differently
# bibi<-subset(bibi, Project!="Regulatory Effectiveness")
# # bibi<-subset(bibi, Project!="KC Historical")
# # bibi<-subset(bibi, Project!="Des Moines Creek Habitat Enhancements, Phase 3")
# # bibi<-subset(bibi, Project!="Des Moines Creek Water Quality Monitoring Program")
# # bibi<-subset(bibi, Project!="Restoration and Protection of Select B-IBI Basins - King County riffle method")
# # bibi<-subset(bibi, Project!="Restoration and Protection of Select B-IBI Basins - Ecology transect method")
# unique(bibi$Project)
# 
# # bibi <- aggregate(cbind(Latitude, Longitude, Event.Date, Overall.Score, Taxa.Richness.Quantity, Ephemeroptera.Richness.Quantity, Plecoptera.Richness.Quantity, Trichoptera.Richness.Quantity, EPT.Richness.Quantity, Clinger.Richness.Quantity, Long.Lived.Richness.Quantity, Intolerant.Richness.Quantity, Percent.Dominant.Quantity,  Predator.Percent.Quantity, Tolerant.Percent.Quantity) ~  Project + WRIA.Number + WRIA + Basin + Subbasin + Stream + Site.Code + year + sampnum + year, data = bibi, FUN = "mean", na.action = na.exclude)
# bibi <- droplevels(bibi[bibi$Year > 2001,]) # subset to sites since 2002
# bibi <- ddply(bibi, 'Site.Code', mutate, por = length(Overall.Score)) # add on period of record col
# bibi<-droplevels(bibi[bibi$por > 9,])# look only at sites with >9 years of data
# bibi<-subset(bibi, bibi$Site.Code!=exclude)
# bibi <- ddply(bibi, 'Site.Code', mutate, timerange = (max(Year)-min(Year))) # add on period of record col
# bibi<-droplevels(bibi[bibi$timerange > 9,])# look only at streams with >9 years of data
# 
# bibi.lr<-bibi
# bibi.lr1<-bibi.lr
# # bibi.lr<-subset(bibi.lr, WRIA.Number==9)
# # bibi.lr1<-subset(bibi.lr1, WRIA.Number==9)
# # bibi<-subset(bibi, WRIA.Number==9)
# 
# sites<-unique(bibi.lr[, c("Subbasin","Stream.or.River", "Site.Code")] )
# sites<-data.frame(site=sites)
# # bibi.lr1$Year<-as.numeric(paste0(bibi.lr1$Year))
# 
# # ggplot(bibi.lr1, aes(x=Year, y=Tot_Richness_Score, color=Site.Code))+geom_smooth(se=F)+theme(legend.position = "none")+
# #   facet_wrap(~Site.Code)+geom_point()+
# #   geom_vline(aes(xintercept=2013), color="black")
# 
# bibi.lr1[bibi.lr1$Year<2012,"Overall.Score"]
# bibi.lr1[bibi.lr1$Year<2012,"Overall.Score"]<-(bibi.lr1[bibi.lr1$Year<2012,"Overall.Score"]*1)+10.1
# # bibi.lr1[bibi.lr1$Year<2012,"Overall.Score"]<-(bibi.lr1[bibi.lr1$Year<2012,"Overall.Score"]*0.968)+12
# 
# ggplot(bibi.lr1, aes(x=Year, y=Overall.Score))+geom_smooth(se=F)+theme(legend.position = "none")+
#   geom_point()
# library(ggpmisc)
# 
# meanscores<-ddply(bibi.lr1, .(Year), summarize, meanScore=mean(Overall.Score), nyears=length(unique(Site.Code)), sd=sd(Overall.Score), min=min(Overall.Score), max=max(Overall.Score), median=median(Overall.Score))
# meanscore<-ggplot(meanscores, aes(x=Year, y=meanScore))+geom_smooth(method="lm",se=F)+theme(legend.position = "none")+
#   geom_point()+ylim(c(0,100))+labs(y="BIBI Score")+
#   stat_poly_eq(formula = y ~ x, rr.digits = 2, coef.digits = 3, size = 4, color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~")), parse = TRUE) 
# 
# ggsave("2002-2021_meansscores.png", plot=meanscore, width=20, height=10)
# 
# scores<-ggplot(bibi )+
#   geom_rect(xmin=2001, xmax=2022, ymin=0, ymax=20, fill="firebrick1", color="black")+
#   geom_rect(xmin=2001, xmax=2022, ymin=20, ymax=40, fill="tan1", color="black")+
#   geom_rect(xmin=2001, xmax=2022, ymin=40, ymax=60, fill="lightgoldenrod", color="black")+
#   geom_rect(xmin=2001, xmax=2022, ymin=60, ymax=80, fill="lightgreen", color="black")+
#   geom_rect(xmin=2001, xmax=2022, ymin=80, ymax=100, fill="steelblue1", color="black")+
#   geom_boxplot(aes(y=Overall.Score, x=as.numeric(paste0(Year)), group=as.factor(Year)), fill="white")+
#   geom_point(data=meanscores, mapping=aes(x=as.numeric(paste0(Year)), y=meanScore))+
#   geom_smooth(data=meanscores, mapping=aes(x=as.numeric(paste0(Year)), y=meanScore), method="lm",se=F)+
#   labs(y="Overall Score", x="Year")+ theme(text = element_text(size = 28))+
#   geom_quantile(data=bibi, mapping=aes(x=as.numeric(paste0(Year)), y=Overall.Score), quantiles=.5, color="red", size=1)
# ggsave("2002-2021_scores_pres.png", plot=scores, width=20, height=10)
# 

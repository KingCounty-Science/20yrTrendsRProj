setwd(here::here())
setwd("./outputs")
countsa<-read.csv("Collapsed_Coarse_Taxa_density.csv")

###highest richness####
# test<-subset(counts, Sample.Code=="08CED5046_15")
# sum(test$total_sample_count)

###highest abundance#########
test<-subset(counts, Sample.Code=="08BEA3474_19")
testsample<-rep(test$OTU_COARSE, test$total_sample_count)

round(runif(50, min=1, max=120))


##########add a bunch of rarer made up taxa
madeuptaxa<-c(LETTERS[1:26], letters[1:24])
madeuptaxa<-as.data.frame(madeuptaxa)
madeuptaxa[,2]<-round(runif(50, min=1, max=120))
madeuptaxa<-rep(madeuptaxa$madeuptaxa, madeuptaxa$V2)
testsample<-c(testsample, madeuptaxa)
length(unique(sample(x = testsample, size = 2000)))
table(testsample)
#################change sample size, looks for differences
test1<-replicate(n=100, expr=length(unique(sample(sample(x = testsample, size = 2000,replace=F), size=500,replace=F))))
test2<-replicate(n=100, expr=length(unique(sample(sample(x = testsample, size = 4000,replace=F), size=500,replace=F))))
test3<-replicate(n=100, expr=length(unique(sample(sample(x = testsample, size = 1000,replace=F), size=500,replace=F))))
test4<-replicate(n=100, expr=length(unique(sample(sample(x = testsample, size = 450,replace=F), size=450,replace=F))))

hist(test1)       
hist(test2)
hist(test3)
hist(test4)
mean(test1)
mean(test2)
mean(test3)
mean(test4)
median(test1)
median(test2)
median(test3)
median(test4)


#####same test, but without adding a bunch of rare made up taxa
test<-subset(counts, Sample.Code=="08BEA3474_19")
testsample<-rep(test$OTU_COARSE, test$total_sample_count)


test1a<-replicate(n=100, expr=length(unique(sample(sample(x = testsample, size = 2000,replace=F), size=500,replace=F))))
test2a<-replicate(n=100, expr=length(unique(sample(sample(x = testsample, size = 4000,replace=F), size=500,replace=F))))
test3a<-replicate(n=100, expr=length(unique(sample(sample(x = testsample, size = 1000,replace=F), size=500,replace=F))))
test4a<-replicate(n=100, expr=length(unique(sample(sample(x = testsample, size = 100,replace=F), size=100,replace=F))))
hist(test1a)       
hist(test2a)
hist(test3a)
mean(test1a)
mean(test2a)
mean(test3a)
mean(test4a)
median(test1a)
median(test2a)
median(test3a)


####now lets look at real sample with a lot of richness

test<-subset(counts, Sample.Code=="08CED5046_15")
testsample<-rep(test$OTU_COARSE, test$total_sample_count)
testsample<-sample(testsample, size=60000, replace=T)#########sample size is too small, re-sample to a larger population
length(unique(testsample))
table(testsample)


###adding even more rare taxa
madeuptaxa<-c(LETTERS[1:26], letters[1:24])
madeuptaxa<-as.data.frame(madeuptaxa)
madeuptaxa[,2]<-round(runif(50, min=1, max=60))
madeuptaxa<-rep(madeuptaxa$madeuptaxa, madeuptaxa$V2)
testsample<-c(testsample, madeuptaxa)
length(unique(sample(x = testsample, size = 2000)))
table(testsample)

test1b<-replicate(n=100, expr=length(unique(sample(sample(x = testsample, size = 2000,replace=F), size=500,replace=F))))
test2b<-replicate(n=100, expr=length(unique(sample(sample(x = testsample, size = 4000,replace=F), size=500,replace=F))))
test3b<-replicate(n=100, expr=length(unique(sample(sample(x = testsample, size = 1000,replace=F), size=500,replace=F))))
test4b<-replicate(n=100, expr=length(unique(sample(sample(x = testsample, size = 500,replace=F), size=500,replace=F))))
hist(test1b)       
hist(test2b)
hist(test3b)
mean(test1b)
mean(test2b)
mean(test3b)
mean(test4b)
median(test1b)
median(test2b)
median(test3b)
test1d<-replicate(n=100, expr=length(unique(sample(sample(sample(x = testsample, size = 2000,replace=F), size=800,replace=F), size=500, replace=F))))
hist(test1d)
mean(test1d)
test1d<-replicate(n=100, expr=length(unique(sample(sample(sample(x = testsample, size = 1000,replace=F), size=550,replace=F), size=500, replace=F))))
hist(test1d)
mean(test1d)
####################################################################################

test<-subset(countsa, Sample.Code=="08CED5046_15")
testsample<-rep(test$OTU_COARSE, test$total_sample_count)
testsample<-sample(testsample, size=60000, replace=T)#########sample size is too small, re-sample to a larger population
# test<-subset(countsa, Sample.Code=="08BEA3474_19")
# testsample<-rep(test$OTU_COARSE, test$total_sample_count)
length(unique(testsample))
keep<-unique(testsample)[1:15]
testsample3<-testsample[testsample %in% keep]

table(testsample)
###adding even more rare taxa
# madeuptaxa<-c(paste0(LETTERS[1:26], LETTERS[1:26]))
# madeuptaxa<-as.data.frame(madeuptaxa)
# madeuptaxa[,2]<-round(runif(26, min=1, max=10))
# madeuptaxa<-rep(madeuptaxa$madeuptaxa, madeuptaxa$V2)
# testsample3<-c(testsample, madeuptaxa)
# length(unique(sample(x = testsample, size = 1000)))
# table(testsample)
# paste0(LETTERS[1:26], LETTERS[1:26])


###adding even more rare taxa
madeuptaxa<-c(LETTERS[1:26], letters[1:24])
madeuptaxa<-as.data.frame(madeuptaxa)
madeuptaxa[,2]<-round(runif(50, min=1, max=10))
madeuptaxa<-rep(madeuptaxa$madeuptaxa, madeuptaxa$V2)
testsample2<-c(testsample, madeuptaxa)
length(unique(sample(x = testsample, size = 1000)))
table(testsample)


testsample4<-c(testsample, rep("fake", 20000))

# test1b<-replicate(n=100, expr=length(unique(sample(sample(x = testsample, size = 700,replace=F), size=100,replace=F))))
# test2b<-replicate(n=100, expr=length(unique(sample(sample(x = testsample, size = 500,replace=F), size=100,replace=F))))
# test3b<-replicate(n=100, expr=length(unique(sample(sample(x = testsample, size = 100,replace=F), size=100,replace=F))))
# hist(test1b)       
# hist(test2b)
# hist(test3b)
# mean(test1b)
# mean(test2b)
# mean(test3b)
# median(test1b)
# median(test2b)
# median(test3b)


test4b<-replicate(n=100, expr=sample(sample(x = testsample, size = 2000,replace=F), size=550,replace=F))
test6b<-replicate(n=100, expr=sample(sample(x = testsample3, size = 1800,replace=F), size=590,replace=F))

test4c<-replicate(n=100, expr=sample(sample(x = testsample, size = 2000,replace=F)))
test6c<-replicate(n=100, expr=sample(sample(x = testsample3, size = 1800,replace=F)))
test5c<-replicate(n=100, expr=sample(rbind(test6c, test4c), size=450,replace=F), simplify = T)
# test6b<-sample(test6b, size=575, replace=T)
# test4b<-rbind(test4b, rep("FAKE", 100))
# test6b<-rbind(test6b, rep("FAKE2", 100))
test5b<-replicate(n=100, expr=sample(rbind(test6b, test4b), size=450,replace=F), simplify = T)


test4b2<-apply(test4b, MARGIN=2,FUN=function(x) sample(x, size=450, replace=F))
test6b2<-apply(test6b, MARGIN=2,FUN=function(x) sample(x, size=450, replace=F))
# test5b2<-replicate(n=100, expr=sample(rbind(test6b2, test4b2), size=450,replace=F), simplify = T)

test4b_richness<-apply(test4b, MARGIN=2,FUN=function(x) length(unique(x)))
test6b_richness<-apply(test6b, MARGIN=2,FUN=function(x) length(unique(x)))
test5b_richness<-apply(test5b, MARGIN=2,FUN=function(x) length(unique(x)))
test5c_richness<-apply(test5c, MARGIN=2,FUN=function(x) length(unique(x)))
test4b2_richness<-apply(test4b2, MARGIN=2,FUN=function(x) length(unique(x)))
test6b2_richness<-apply(test6b2, MARGIN=2,FUN=function(x) length(unique(x)))
# test5b2_richness<-apply(test5b2, MARGIN=2,FUN=function(x) length(unique(x)))

mean(test5b_richness)
mean(test5c_richness)
# hist(test5b_richness)
# 
# mean(test4b_richness)
# # hist(test4b_richness)
# 
# 
# mean(test6b_richness)
# hist(test6b_richness)


# 
mean(test4b2_richness)
mean(test6b2_richness)
# mean(test5b2_richness)

comb<-cbind(test4b2_richness, test5b_richness)
comb2<-cbind(test6b2_richness, test5b_richness)
comb3<-rbind(comb, comb2)
mean(comb3[,"test4b2_richness"])
mean(comb3[,"test5b_richness"])
#######################
###testing the idea of compositing replicate samples 
test<-subset(counts, Sample.Code=="08CED5046_15")
testsample<-rep(test$OTU_COARSE, test$total_sample_count)
testsample<-sample(testsample, size=60000, replace=T)#########sample size is too small, re-sample to a larger population
length(unique(testsample))
table(testsample)


###adding even more rare taxa
madeuptaxa<-c(LETTERS[1:26], letters[1:24])
madeuptaxa<-as.data.frame(madeuptaxa)
madeuptaxa[,2]<-round(runif(50, min=1, max=60))
madeuptaxa<-rep(madeuptaxa$madeuptaxa, madeuptaxa$V2)
testsample<-c(testsample, madeuptaxa)
length(unique(sample(x = testsample, size = 2000)))

test1c<-replicate(n=100, expr=sample(sample(x = testsample, size = 2000,replace=F), size=500,replace=F), simplify = T)
test2c<-replicate(n=100, expr=sample(sample(x = testsample, size = 2000,replace=F), size=500,replace=F), simplify = T)
test3c<-rbind(test1c, test2c)
test3c<-replicate(n=100, expr=sample(rbind(test1c, test2c), size=500,replace=F), simplify = T)

test1c_richness<-apply(test1c, MARGIN=2,FUN=function(x) length(unique(x)))
test2c_richness<-apply(test2c, MARGIN=2,FUN=function(x) length(unique(x)))
test3c_richness<-apply(test3c, MARGIN=2,FUN=function(x) length(unique(x)))
mean(test1c_richness)
hist(test1c_richness)
mean(test2c_richness)
hist(test2c_richness)
mean(test3c_richness)
hist(test3c_richness)

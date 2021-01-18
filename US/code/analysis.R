library(geepack)
library(dplyr)
library(rlang)
setwd("/Users/fanxia/Dropbox/covid/")
#source("msmweight.R")
filldata <- read.csv("filldata3045.csv",header = T)[,-1]
xy <- read.csv("xy3045.csv",header = T)[,-1]

# confounders=c("Physicians.per","area","election","gdp.2019","pop.2019q4","inc.2019q4",
# "Number.Homeless..2019.", "Percent.at.risk.for.serious.illness.due.to.COVID","Percent.living.under.the.federal.poverty.line..2018.",
# "Percent.Unemployed..2018.","All.cause.deaths.2018","Date.closed.K.12.schools","Closed.non.essential.businesses","Closed.restaurants.except.take.out" ,
# "Mandate.face.mask.use.by.all.individuals.in.public.spaces",
# "Mandate.face.mask.use.by.employees.in.public.facing.businesses",
# "State.of.emergency","sumcase")
confounders=c("Physicians.num","area","election","gdp.2019","pop.2019q4","inc.2019q4",
              "Date.closed.K.12.schools","Closed.non.essential.businesses",
              "Mandate.face.mask.use.by.employees.in.public.facing.businesses",
              "State.of.emergency","sumcase","totalTestResults","Population.Staying.at.Home","days")
tvconfounders = c("Date.closed.K.12.schools","Closed.non.essential.businesses",
                  "Mandate.face.mask.use.by.employees.in.public.facing.businesses",
                  "State.of.emergency","totalTestResults")
lagc <- paste(tvconfounders,"_lag",sep = "")
timevar <- c("relative_time","timeQ1","timeQ2")

timevar <- c("relative_time")

baseline.char <- filldata %>% group_by(id) %>% slice_head(n=1)
baseline.char <- data.frame(baseline.char[,c("id",confounders)])
colnames(baseline.char) <- c("id",paste(confounders,"_base",sep=""))
basev <- paste(confounders,"_base",sep="")
#log area and GDP ??
#baseline.char$area_base <- log(baseline.char$area_base)
#baseline.char$gdp.2019_base <- log(baseline.char$gdp.2019_base)



#####merge data
dataset1 <- merge(xy,baseline.char,by="id",all.x = T)

#create natural spline for time
library(Hmisc)
timespline <- rcspline.eval(dataset1$relative_time,  knots=quantile(dataset1$relative_time,c(0,0.33,0.66,1)))
colnames(timespline) <- c("timeQ1","timeQ2")


dataset <- data.frame(cbind(dataset1,timespline))

#fill in NA test using 0
dataset$totalTestResults_lag[which(is.na(dataset$totalTestResults_lag))] <- 0
dataset$totalTestResults_base[which(is.na(dataset$totalTestResults_base))] <- 0

#model 1:pooled logistics regression, numerator, without confounders
datam1 <- dataset[which(dataset$lagx1==0),]


formula.m1 <- paste0("x","~",paste(c(timevar),collapse="+"))

fitm1 <- glm(formula.m1, data=datam1,
             family = binomial(link = logit))


dataset$fitted_nom <- 0
dataset$fitted_nom[which(dataset$lagx1==0)] <- fitm1$fitted.values

#model 2:pooled logistic regression, denominator, with time-varying confounders
datam2 <- dataset[which(dataset$lagx1==0),]
#which(is.na(datam1), arr.ind=TRUE)

formula.m2 <- paste0("x","~",paste(c(basev,timevar,lagc),collapse="+"))

fitm2 <- glm(formula.m2, data=datam2,
             family = binomial(link = logit))

dataset$fitted_denom <- 0
dataset$fitted_denom[which(dataset$lagx1==0)] <- fitm2$fitted.values



#calculate weights
dataset$weightcomp_nom <- with(dataset,
                               (1-(x-lagx1))*(1-fitted_nom)+
                                 ((x-lagx1))*fitted_nom)
dataset$weightcomp_denom <- with(dataset,
                                 (1-(x-lagx1))*(1-fitted_denom)+
                                   ((x-lagx1))*fitted_denom)

#because we start from Y2, Y1 is used in the weighting model(check!)
dataset %>% group_by(id) %>% slice(2:n()) -> dataset
dataset %>% group_by(id) %>% mutate(weight_nom=cumprod(weightcomp_nom),
                                    weight_denom=cumprod(weightcomp_denom),
                                    sw = weight_nom/weight_denom) -> weightdata
boxplot(weightdata$sw)
quantile(weightdata$sw)
mean(weightdata$sw)
sd(weightdata$sw)

weightdata <- weightdata %>% 
  group_by(id) %>%
  mutate(cumtreat=cumsum(lagx1),meantreat=cummean(lagx1))

weightdata <- data.frame(weightdata)
#weightdata$y[which(weightdata$y<0)] <- 0

#truncate extreme weight
# weightdata$sw <- ifelse(weightdata$sw < quantile(weightdata$sw,0.025),quantile(weightdata$sw,0.025), 
#                         ifelse(weightdata$sw > quantile(weightdata$sw,0.975),quantile(weightdata$sw,0.975),weightdata$sw))
# 
# boxplot(weightdata$sw)
# quantile(weightdata$sw)
# mean(weightdata$sw)
# sd(weightdata$sw)


formula1 <- as.formula(paste0("newy","~",paste(c("meantreat15"),collapse="+")))
#formula1mar <- as.formula(paste0("y","~",paste(c("meantreat*days",v),collapse="+")))

fit1.geeglm <- geeglm(formula1, data=weightdata,id=id,corstr = "ar1")                     
summary(fit1.geeglm)
fit2.geeglm <- geeglm(formula1, data=weightdata,id=id,corstr = "ar1",weights = sw) 
summary(fit2.geeglm)

library(doBy)
theta0.1 <- esticon(fit1.geeglm,L=c(1,0))
theta1.1 <- esticon(fit1.geeglm,L=c(0,1))
summary(theta0.1)
summary(theta1.1)

theta0.2 <- esticon(fit2.geeglm,L=c(1,0))
theta1.2 <- esticon(fit2.geeglm,L=c(0,1))
summary(theta0.2)
summary(theta1.2)

# filldata %>% group_by(id) %>% slice(2:n()) -> forsumcase
# weighted.theta <- coef(fit2.geeglm)[2]
# data_projection <- data.frame(cbind(weightdata$id,weightdata$relative_time,weightdata$newy, weightdata$meantreat15,
#                                     weightdata$y,weightdata$sumcase_base,forsumcase$sumcase))
# colnames(data_projection) <- c("id","rtime","growth_rate","treat_history_15","newcase","sumcase_base","realcase")
# write.csv(data_projection,"projectdata.csv")
# write.csv(theta1.2,"theta.csv")


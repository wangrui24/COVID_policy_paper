library(geepack)
library(dplyr)
library(rlang)
library(doBy)
setwd("/Users/fanxia/Dropbox/covid/")

######variables
confounders=c("Physicians.num","area","election","gdp.2019","pop.2019q4","inc.2019q4",
              "Date.closed.K.12.schools","Closed.non.essential.businesses",
              "Mandate.face.mask.use.by.employees.in.public.facing.businesses",
              "State.of.emergency","sumcase","totalTestResults","Population.Staying.at.Home","days")
tvconfounders = c("Date.closed.K.12.schools","Closed.non.essential.businesses",
                  "Mandate.face.mask.use.by.employees.in.public.facing.businesses",
                  "State.of.emergency","totalTestResults")
lagc <- paste(tvconfounders,"_lag",sep = "")
#timevar <- c("relative_time","timeQ1","timeQ2")
timevar <- c("relative_time")

######read in datasets
filldata.original <- read.csv("filldata_original.csv",header = T)[,-1]
xy <- read.csv("xy.csv",header = T)[,-1]

#####Set time 0
filldata.original %>% group_by(id) %>% filter(sumcase>=30) -> filldata2
range(filldata2$days)
filldata2 %>% group_by(id) %>% mutate(mintime = min(days)) -> filldata2
xy$sumcase <- filldata.original$sumcase
xy %>% group_by(id) %>% filter(sumcase>= 30) -> xy2
xy2 %>% group_by(id) %>% mutate(mintime = min(days)) -> xy2

#is there NA in testing data?
length(which(is.na(filldata2$totalTestResults)))

#####generate subset data and estimate
subgen <- function(add_day)
{
  filldata2 %>% group_by(id) %>% filter(days >= mintime & days <= mintime + add_day) ->filldata3
  filldata <- data.frame(filldata3)
  xy2 %>% group_by(id) %>% filter(days >= mintime & days <= mintime + add_day) -> xy3
  xy3<- xy3 %>% group_by(id) %>% mutate(relative_time = 1:n())
  xy.final <- data.frame(xy3)
  
  list(filldata,xy.final)
}
estimation <- function(filldata,xy)
{
  baseline.char <- filldata %>% group_by(id) %>% slice_head(n=1)
  baseline.char <- data.frame(baseline.char[,c("id",confounders)])
  colnames(baseline.char) <- c("id",paste(confounders,"_base",sep=""))
  basev <- paste(confounders,"_base",sep="")
  
  #####merge data
  dataset <- merge(xy,baseline.char,by="id",all.x = T)
  
  #create natural spline for time
  # library(Hmisc)
  # timespline <- rcspline.eval(dataset1$relative_time,  knots=quantile(dataset1$relative_time,c(0,0.33,0.66,1)))
  # colnames(timespline) <- c("timeQ1","timeQ2")
  # dataset <- data.frame(cbind(dataset1,timespline))
  
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
  # boxplot(weightdata$sw)
  # quantile(weightdata$sw)
  # mean(weightdata$sw)
  # sd(weightdata$sw)
  
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
  
  theta0.1 <- esticon(fit1.geeglm,L=c(1,0))
  theta1.1 <- esticon(fit1.geeglm,L=c(0,1))
  est0.unweight <- summary(theta0.1)
  est1.unweight <-  summary(theta1.1)
  
  theta0.2 <- esticon(fit2.geeglm,L=c(1,0))
  theta1.2 <- esticon(fit2.geeglm,L=c(0,1))
  est0.weight <-  summary(theta0.2)
  est1.weight <-  summary(theta1.2)
  
  res <- rbind(est0.unweight,est1.unweight,est0.weight,est1.weight)
  
  return(res)
}


est <- est.up <- est.low <- rep(NA,21)
for(i in 30:50)
{
  data.temp <- subgen(i)
  res.temp <- estimation(filldata = data.temp[[1]],xy = data.temp[[2]])
  est[i-30+1] <- res.temp[4,"estimate"]
  est.up[i-30+1] <- res.temp[4,"upr"]
  est.low[i-30+1] <- res.temp[4,"lwr"]
}

plot.data <- data.frame(30:50,est,est.up,est.low)
colnames(plot.data) <- c("time","growthrate","upper","lower")
plot.data$type <- "sensitivity"
plot.data[45-30+1,"type"] <- "main"

ggplot(data = plot.data, aes(y = time, x = growthrate))+
  geom_pointrange(aes(xmin=lower, xmax=upper,colour=type))+
  scale_x_continuous(limits = c(-0.35, -0))+
  scale_y_continuous(breaks = c(30:50))+
  ylab("Relative dates included")+
  xlab("Estimated effect on daily growth rate") + geom_point(aes(colour = type)) + theme(legend.position = "none")
ggsave("change_time_period_US.png" , plot = last_plot(), width = 7, height = 10)

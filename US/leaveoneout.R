library(geepack)
library(dplyr)
library(rlang)
library(doBy)
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
lagc <- tvconfounders
timevar <- c("relative_time")
stateandid <- data.frame(apply(table(filldata$Province.State,filldata$id), 1, function(x)which(x>0)))
colnames(stateandid) <- "id"
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
dataset$totalTestResults[which(is.na(dataset$totalTestResults))] <- 0
dataset$totalTestResults_base[which(is.na(dataset$totalTestResults_base))] <- 0
dataset %>% group_by(id) %>% mutate(lagx1 = lag(x,1)) ->dataset
estimation <- function(state.id,data)
{
  dataset <- subset(data,id!=state.id)
  
  #model 1:pooled logistics regression, numerator, without confounders
  datam1 <- dataset[which(dataset$lagx1==0),]
  
  
  formula.m1 <- paste0("x","~",paste(c(timevar),collapse="+"))
  
  fitm1 <- glm(formula.m1, data=datam1,
               family = binomial(link = logit))
  
  
  dataset$fitted_nom <- 0
  dataset$fitted_nom[which(dataset$lagx1==0)] <- fitm1$fitted.values
  dataset %>% group_by(id) %>% mutate(lagr = lag(newy,1)) -> dataset
  
  #model 2:pooled logistic regression, denominator, with time-varying confounders
  datam2 <- dataset[which(dataset$lagx1==0),]
  #which(is.na(datam1), arr.ind=TRUE)
  
  formula.m2 <- paste0("x","~",paste(c(basev,timevar,lagc,"lagr"),collapse="+"))
  
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
  
  dataset$weightcomp_denom[which(is.na(dataset$weightcomp_denom))] <- 1
  dataset$weightcomp_nom[which(is.na(dataset$weightcomp_nom))] <- 1
  
  #because we start from Y2, Y1 is used in the weighting model(check!)
  dataset %>% group_by(id) %>% mutate(weight_nom=cumprod(weightcomp_nom),
                                      weight_denom=cumprod(weightcomp_denom),
                                      sw = weight_nom/weight_denom) -> weightdata
  # boxplot(weightdata$sw)
  # quantile(weightdata$sw)
  # mean(weightdata$sw)
  # sd(weightdata$sw)

  
  weightdata <- data.frame(weightdata)
  
  formula1 <- as.formula(paste0("newy","~",paste(c("meantreat15"),collapse="+")))

  
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

est <- est.up <- est.low <- rep(NA,52)
for(i in 1:52)
{
  res.temp <- estimation(state.id = i, data = dataset)
  est[i] <- res.temp[4,"estimate"]
  est.up[i] <- res.temp[4,"upr"]
  est.low[i] <- res.temp[4,"lwr"]
}

plot.data <- data.frame(c(rownames(stateandid),"All"),est,est.up,est.low)
colnames(plot.data) <- c("states","growthrate","upper","lower")
plot.data$type <- "sensitivity"
plot.data[52,"type"] <- "main"

plot.data$states <- as.character(plot.data$states)
plot.data$states <- factor(plot.data$states,levels = plot.data$states)

ggplot(data = plot.data, aes(y = states, x = growthrate))+
  geom_pointrange(aes(xmin=lower, xmax=upper,colour=type))+
  scale_x_continuous(limits = c(-0.3, -0.1))+
  ylab("State excluded")+
  xlab("Estimated effect on daily growth rate") + geom_point(aes(colour = type)) + theme(legend.position = "none")
ggsave("leave_one_out_US_new.png" , plot = last_plot(), width = 7, height = 10)

setwd("/Users/fanxia/Dropbox/covid")
library(dplyr)
library(lubridate)
covariates <- read.csv("us_covariates.csv", header = T)
covariates2 <- na.omit(read.csv("statecharacteristics.csv", header = T))

physical <- read.csv("physicaldistance.csv",header = T)[1:51,]
state_em <- read.csv("stateofemergency.csv",header = T)[1:51,]
mask <- read.csv("facemask.csv",header = T)[1:51,]


otherpolicies <- merge(physical,state_em,by="State")
otherpolicies <- merge(otherpolicies,mask,by="State")



timeseriescase <- read.csv("time_series_covid19_confirmed_US.csv",header = T)
stayathome <- read.csv("stayathomecsv.csv",header = T)
colnames(stayathome) <- c("State", "Stay.at.home..shelter.in.place")
testing <- read.csv("all-states-history.csv")

data.long <- reshape(timeseriescase,
                     varying = c(12:162),
                     v.names = "ncase",
                     direction = "long")

data.by.state <- data.long %>% group_by(Province_State,time) %>%
  summarise(sumcase = sum(ncase))

state1 <- unique(timeseriescase$Province_State)
state2 <- unique(stayathome$State)
state.excluded <- setdiff(state1,state2)

data.by.state <- data.frame(data.by.state[which(data.by.state$Province_State %in% state2),])
data.by.state$Province_State <- as.character(data.by.state$Province_State)
stayathome$Stay.at.home..shelter.in.place <- mdy(stayathome$Stay.at.home..shelter.in.place)
#recode time
stayathome2 <- stayathome
colnames(stayathome2) <- c("Province_State","time")
stayathome2$time <- as.integer((stayathome2$time) - mdy("1/22/20"))
stayathome2$stay  <- 0
stayathome2[!is.na(stayathome2$time),"stay"] <- 1
stayathome2$Province_State <- as.character(stayathome2$Province_State)

stayathome2$time[which(is.na(stayathome2$time))] <- 10000

#expand stayathome table
fulldate <- data.frame(matrix(NA, nrow=151*51,ncol = 3))
colnames(fulldate) <- c("Province_State","time","stay")
fulldate$Province_State <- rep(stayathome2$Province_State, each=151)
fulldate$time <- rep(1:151,51)

for(i in 1:51)
{
  for(j in 1:151)
  {
    state <- stayathome2$Province_State[i]
    time <- stayathome2$time[i]
    #    print(fulldate$Province_State[(i-1)*151+j]==state)
    fulldate$stay[(i-1)*151+j] <- ifelse(j<time,0,1)
  }
}

case.state.time <- left_join(data.by.state,fulldate)
case.state.time[is.na(case.state.time$stay),"stay"] <- 0 

sum(case.state.time$stay)


filldata.original <- case.state.time
colnames(filldata.original) <- c("Province.State","days","sumcase","treatment")
filldata.state <- unique(filldata.original$Province.State)
id <- rep(NA, nrow(filldata.original))
for(i in 1:nrow(filldata.original))
{
  for (j in 1:length(filldata.state))
  {
    if(filldata.original$Province.State[i]==filldata.state[j])
    {
      id[i] <- j
    }
  }
}
filldata.original$id <- id

filldata.original <- data.frame(filldata.original%>% group_by(id) %>% mutate(newcase2 = sumcase - lag(sumcase, default = 0)))
##merge with test data
test620 <- testing[which(testing$date <= 20200621),c("date","state","dataQualityGrade","totalTestResults","totalTestResultsIncrease")]
library(dplyr)
test620 <- data.frame(test620)

test620$days <- as.integer( ymd(test620$date) - mdy("1/22/20"))


library(tidyverse)

st_crosswalk <- tibble(abb = state.abb) %>%
  bind_cols(tibble(state = state.name)) %>% 
  bind_rows(tibble(state = "District of Columbia", abb = "DC"))%>% 
  bind_rows(tibble(state = "Guam", abb = "GU"))%>% 
  bind_rows(tibble(state = "Puerto Rico", abb = "PR"))

test620$abb <- test620$state
test620 <- test620[,-2]


test620.state <- data.frame(left_join(test620, st_crosswalk, by = "abb"))
test620.state <- test620.state[which(!is.na(test620.state$state)),]
test620.state$Province.State <- test620.state$state
test620.state <- test620.state[with(test620.state, order(test620.state$Province.State,test620.state$days)),]
test620.state <- test620.state[,-c(1,6,7)]

testbeforereport <- test620.state %>% group_by(Province.State) %>% summarise(start = first(totalTestResults),start2 = first(totalTestResultsIncrease))

filldata.original <- filldata.original[which(filldata.original$Province.State!="Grand Princess"),]

filldata.try <- merge(filldata.original,test620.state,by=c("Province.State","days"),all.x = T)

tripuse <- read.csv("tripuse.csv",header = T)[,-1]
filldata.try <- merge(filldata.try,tripuse,by=c("Province.State","days"),all.x = T)
####
##merge with the covariate dataset
filldata.merge <- merge(filldata.try, covariates, by.x = "Province.State", by.y = "states")
filldata.merge <- merge(filldata.merge,covariates2, by.x = "Province.State", by.y = "State")


colnames(otherpolicies)
otherpolicies.byday <- apply(otherpolicies[,-1],2,function(x){x <- as.integer(mdy(x)-mdy("1/22/20"))})
otherpolicies.byday <- data.frame(otherpolicies[,1],otherpolicies.byday)
colnames(otherpolicies.byday)[1] <- "State"

filldata.merge <- merge(filldata.merge,otherpolicies.byday,by.x = "Province.State", by.y = "State")

policies <- colnames(otherpolicies.byday)[-1]

filldata.merge[,policies] <- apply(filldata.merge[,policies],2,function(x){ifelse(x<filldata.merge$days,0,1)})


filldata.original <- filldata.merge
###
filldata.original[,policies] <- apply(filldata.original[,policies],2, function(x){ifelse(is.na(x),0,x)})

filldata.original <- data.frame(filldata.original %>% group_by(id) %>% mutate(., 
                                                                              lagy1 = lag(sumcase, 1),
                                                                              lagtreat = lag(treatment,1)
))
filldata.original[which(filldata.original$Province.State=="Washington"),"lagy1"][1] <- 1

###

confounders=c("Physicians.per","area","election","gdp.2019","pop.2019q4","inc.2019q4",
              "Date.closed.K.12.schools","Closed.non.essential.businesses",
              "Mandate.face.mask.use.by.employees.in.public.facing.businesses",
              "State.of.emergency","sumcase","totalTestResults")
tvconfounders = c("Date.closed.K.12.schools","Closed.non.essential.businesses",
                  "Mandate.face.mask.use.by.employees.in.public.facing.businesses",
                  "State.of.emergency","totalTestResults")
#timevar <- c("relative_time")



xy <- cbind(filldata.original$days,filldata.original$id,filldata.original$newcase2, filldata.original$treatment,filldata.original[,tvconfounders])
colnames(xy)[1:4] <- c("days","id","y","x")
xy <- data.frame(xy)

###create lag C for time-varying confounders (tvconfounder)
lagc <- paste(tvconfounders,"_lag",sep = "")
lagformula <- paste0("lag(",tvconfounders,",1)")
xy <- xy %>% group_by(id) %>% mutate(lagy1 = lag(y,1),lagx1= lag(x,1))
xy$y[which(xy$y<0)] <- 0
xy$logy <- log(xy$y+1)
xy %>% group_by(id) %>% mutate(laglogy=lag(logy,1)) -> xy
xy %>% group_by(id) %>% mutate(newy=logy-laglogy) ->xy


library(rlang)

for(i in 1:length(tvconfounders))
{
  xy %>% group_by(id) %>% mutate(!!lagc[i] := !!parse_expr(lagformula[i])) -> xy
}

all.lag <- 0
for(i in 1:15)
{
  lagx.temp <- paste("lagx",i,sep="")
  all.lag <- paste(all.lag,lagx.temp,sep=",")
  lag.formula.temp <- paste0("lag(x,",i,",default=0)")
  xy  %>% mutate(!!lagx.temp := !!rlang::parse_expr(lag.formula.temp)) -> xy
}

lagtreat.vec <- unlist(strsplit(all.lag,","))[-1]
xy <- data.frame(xy)
xy$meantreat15 <- apply(xy[,lagtreat.vec],1,mean)

write.csv(filldata.original,"filldata_original.csv")
write.csv(xy,"xy.csv")

###only look at 60 period after reaching 100 cases
filldata.original %>% group_by(id) %>% filter(sumcase>=30) -> filldata2
range(filldata2$days)
#is there NA in testing data?
length(which(is.na(filldata2$totalTestResults)))
##wrong!!!
filldata2 %>% group_by(id) %>% mutate(mintime = min(days)) -> filldata2
filldata2 %>% group_by(id) %>% filter(days >= mintime & days <= mintime + 45) -> filldata3
filldata <- filldata3

xy$sumcase <- filldata.original$sumcase
xy %>% group_by(id) %>% filter(sumcase>=30) -> xy2
xy2 %>% group_by(id) %>% mutate(mintime = min(days)) -> xy2
xy2 %>% group_by(id) %>% filter(days >= mintime & days <= mintime + 45) -> xy3
xy3<- xy3 %>% group_by(id) %>% mutate(relative_time = 1:n())
xy.final <- xy3

table(filldata$treatment)


#create natural spline for time
# library(Hmisc)
# timespline <- rcspline.eval(xy.final$relative_time, knots=quantile(filldata$days,c(0,0.33,0.66,1)))
# colnames(timespline) <- c("timeQ1","timeQ2")
# #colnames(timespline) <- c("dayQ1","dayQ2","dayQ3")
# filldata <- data.frame(cbind(filldata,timespline))



table(filldata$treatment)

write.csv(filldata,"filldata3045.csv")
write.csv(xy.final,"xy3045.csv")


# a <- subset(filldata.original,sumcase>=30)
# a1 <- a %>% group_by(id) %>% slice(n=1)
# range(a1$Date)
# table(a1$Date)
# 
# 
# filldata.pic <- subset(filldata.original,(Date <= ymd("2020-04-30") & Date >= ymd("2020-03-10")))
# filldata.pic %>% group_by(id) %>% 
#   mutate(treat0 = ifelse(sumcase>=30,0,treatment),treat1 = ifelse(sumcase>=30,1,treatment)) -> filldata.pic
# filldata.pic %>% group_by(id) %>%
#   mutate(sumcase_base = first(sumcase)) -> filldata.pic
# 
# rangefill <- range(filldata.pic$days)
# xy %>% group_by(id) %>% filter(days <= rangefill[2]  & days >= rangefill[1]) -> xy2
# 
# projectdata <- merge(filldata.pic,xy,by=c("id","days"))
# projectdata <- projectdata[,c("id","days","Date","sumcase","treatment","treat0",
#                               "treat1","meantreat15","newy","y","sumcase_base")]
# colnames(projectdata)[8:10] <- c("treat_history_15","growth_rate","newcase")
# 
# write.csv(projectdata,"projectdata.csv")

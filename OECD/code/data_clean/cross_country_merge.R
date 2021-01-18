setwd("D:/R collection/COVID-19/COVID-19/covid_policy_paper/OECD")

#######################################################
# Author: Rui Wang
# Date: 2021/1/18
# Theme: Cross Country data merge
#######################################################
covariates <- read.csv("data/cross_country/interim/crosscounrty_covariates.csv")
case <- read.csv("data/cross_country/interim/case.csv")
policy <- read.csv("data/cross_country/interim/policy.csv",header = TRUE)


day <- dim(case)[2]-4
n <- dim(case)[1]

Country <- as.character(rep(covariates$country.use,rep(day,n)))
Countrycode <- as.character(rep(policy$countrycode,rep(day,n)))
Confirmed.cases <- unlist(c(t(case[,c(5:(4+day))])))
Policy <- as.numeric(unlist(c(t(policy[,c(3:(2+day))]))))

#Date
Date.raw <- colnames(policy)[c(-1,-2)]
Date.list <- strsplit(Date.raw,"X")
Date.str <- c()
for (i in c(1:day)) {
  Date.str[i] <- Date.list[[i]][2]
}
Date.str2 <- c()
for (i in c(1:day)) {
  Date.i <- Date.str[i]
  Date.split.i <- strsplit(Date.i,"")[[1]]
  Date.str2[i] <- paste(paste(Date.split.i[c(1:4)],collapse =""),
                        paste(Date.split.i[c(5:6)],collapse =""),
                        paste(Date.split.i[c(7:8)],collapse =""), sep = "-")
}
Date.str2 <- as.character(Date.str2)
start.date <- as.POSIXct(strptime("2020-01-22", format = "%Y-%m-%d"))
end.date <- as.POSIXct(strptime("2020-06-30", format = "%Y-%m-%d"))
Date <- seq(from = start.date, to = end.date, by = 60*60*24)

#Covariate
Area <- rep(covariates$area, rep(day,n))
GDP <- rep(covariates$gdp, rep(day,n))
Le <- rep(covariates$le, rep(day,n))
Pop <- rep(covariates$pop, rep(day,n))

Tour.arrive <- rep(covariates$tour.arrive, rep(day,n))
Maternal.mortalityratio <- rep(covariates$maternal.mortalityratio, rep(day,n))
Democ <- rep(covariates$democ, rep(day,n))

National.income <- rep(covariates$national.income, rep(day,n))
Elderly.pop <- rep(covariates$elderly.pop, rep(day,n))

Confirmed.cases.pre <- c(NA,Confirmed.cases[-length(Confirmed.cases)])
Growth.rate <- log(Confirmed.cases+1)-log(Confirmed.cases.pre+1)
Growth.rate[Date.str=="20200122"] <- NA

Growth.rate.pre <- c(NA,Growth.rate[-length(Growth.rate)])
Growth.rate.pre[Date.str=="20200122"] <- NA
Growth.rate.pre[Date.str=="20200123"] <- NA
Growth.rate.post <- c(Growth.rate[-1],NA)

newcase <- Confirmed.cases-Confirmed.cases.pre
newcase[newcase<0] <- 0
newcase[Date.str=="20200122"] <- NA
newcase.pre <- c(NA,newcase[-length(newcase)])
newcase.pre[Date.str=="20200122"] <- NA
newcase.pre[Date.str=="20200123"] <- NA
newcase.post <- c(newcase[-1],NA)

#Newcases growth rate
New.Growth.rate <- log(newcase+1)-log(newcase.pre+1)
New.Growth.rate[Date.str=="20200122"] <- NA
New.Growth.rate[Date.str=="20200123"] <- NA

New.Growth.rate.pre <- c(NA,New.Growth.rate[-length(New.Growth.rate)])
New.Growth.rate.pre[Date.str=="20200122"] <- NA
New.Growth.rate.pre[Date.str=="20200123"] <- NA
New.Growth.rate.pre[Date.str=="20200124"] <- NA
New.Growth.rate.post <- c(New.Growth.rate[-1],NA)


#lag2 new cases
newcase.lag2 <- c(NA,newcase.pre[-length(newcase.pre)])
newcase.lag2[Date.str=="20200122"] <- NA
newcase.lag2[Date.str=="20200123"] <- NA
newcase.lag2[Date.str=="20200124"] <- NA

#lag3 new cases
newcase.lag3 <- c(NA,newcase.lag2[-length(newcase.lag2)])
newcase.lag3[Date.str=="20200122"] <- NA
newcase.lag3[Date.str=="20200123"] <- NA
newcase.lag3[Date.str=="20200124"] <- NA
newcase.lag3[Date.str=="20200125"] <- NA

#lag4 new cases
newcase.lag4 <- c(NA,newcase.lag3[-length(newcase.lag3)])
newcase.lag4[Date.str=="20200122"] <- NA
newcase.lag4[Date.str=="20200123"] <- NA
newcase.lag4[Date.str=="20200124"] <- NA
newcase.lag4[Date.str=="20200125"] <- NA
newcase.lag4[Date.str=="20200126"] <- NA

#lag5 new cases
newcase.lag5 <- c(NA,newcase.lag4[-length(newcase.lag4)])
newcase.lag5[Date.str=="20200122"] <- NA
newcase.lag5[Date.str=="20200123"] <- NA
newcase.lag5[Date.str=="20200124"] <- NA
newcase.lag5[Date.str=="20200125"] <- NA
newcase.lag5[Date.str=="20200126"] <- NA
newcase.lag5[Date.str=="20200127"] <- NA

#lag6 new cases
newcase.lag6 <- c(NA,newcase.lag5[-length(newcase.lag5)])
newcase.lag6[Date.str=="20200122"] <- NA
newcase.lag6[Date.str=="20200123"] <- NA
newcase.lag6[Date.str=="20200124"] <- NA
newcase.lag6[Date.str=="20200125"] <- NA
newcase.lag6[Date.str=="20200126"] <- NA
newcase.lag6[Date.str=="20200127"] <- NA
newcase.lag6[Date.str=="20200128"] <- NA


Policy.indicator <- c(rep(0,length(Policy)))
Policy.indicator[Policy>0] <- 1
Policy.indicator.pre <- c(NA,Policy.indicator[-length(Policy.indicator)])
Policy.indicator.pre[Date.str=="20200122"] <- NA

is.case.30 <- rep(0,length(Confirmed.cases))
is.case.30[Confirmed.cases>=30] <- 1

analysis.data.full <- data.frame(Country, Countrycode, Date.str2, Date, 
                                 Area, GDP, Le, Pop,
                                 Tour.arrive,
                                 Maternal.mortalityratio,
                                 Democ,
                                 National.income,Elderly.pop,
                                 Growth.rate, Growth.rate.pre, Growth.rate.post,
                                 New.Growth.rate, New.Growth.rate.pre, New.Growth.rate.post,
                                 Confirmed.cases, Confirmed.cases.pre,
                                 newcase, newcase.pre, newcase.post,
                                 newcase.lag2, newcase.lag3, newcase.lag4,
                                 newcase.lag5, newcase.lag6,
                                 is.case.30,
                                 Policy, Policy.indicator, Policy.indicator.pre)


################################################################
########
########Define Relative date and treatment, start with 30 cases
########
################################################################

Rela.Date30.matrix <- matrix(data = rep(0,length(Confirmed.cases)),nrow = n, ncol = day)
for (i in c(1:n)) {
  country.i <- as.character(covariates$country.use[i])
  index.i <- which(analysis.data.full$Country==country.i)
  data.i <- analysis.data.full[index.i,]
  is.case.30.i <- data.i$is.case.30
  Rela.Date30.i <- is.case.30.i
  start.i <- min(which(Rela.Date30.i==1))
  end.i <- day-start.i+1
  Rela.Date30.i[c(start.i:day)]<-c(1:end.i)
  Rela.Date30.matrix[i,] <- Rela.Date30.i
}

Rela.Date30 <- unlist(c(t(Rela.Date30.matrix)))

analysis.data.full$Rela.Date30 <- Rela.Date30

#Define Treatment
Treatment.cum30.matrix <- matrix(data = NA,nrow = n, ncol = day)
for (i in c(1:n)) {
  country.i <- as.character(covariates$country.use[i])
  index.i <- which(analysis.data.full$Country==country.i)
  data.i <- analysis.data.full[index.i,]
  policy.i <- data.i$Policy.indicator
  is.case.30.i <- data.i$is.case.30
  Rela.Date.i <- data.i$Rela.Date
  Treatment.cum30.i <- rep(0,day)
  if (!all(policy.i==rep(0,day))){
  Treatment.cum30.i[c(1:max(which(is.case.30.i==0)))] <- NA
  start.i <- max(min(which(policy.i==1)),min(which(is.case.30.i==1)))
  end.i <- max(which(policy.i==1))
  Treatment.cum30.i[c(start.i:end.i)]<- c(1:(end.i-start.i+1))
  Treatment.cum30.i[c(end.i:day)] <- rep(Treatment.cum30.i[end.i],(day-end.i+1))}
  Treatment.cum30.matrix[i,] <- Treatment.cum30.i
}

Treatment.cum30 <- unlist(c(t(Treatment.cum30.matrix)))

Treatment.prop30 <-Treatment.cum30/Rela.Date30
max(na.omit(Treatment.prop30))
which(Treatment.prop30==Inf)

analysis.data.full$Treatment.cum30 <- Treatment.cum30
analysis.data.full$Treatment.prop30 <- Treatment.prop30

#Define Treatment of recent 14 days
Treatment.within14.prop.matrix <- matrix(data = NA,nrow = n, ncol = day)
for (i in c(1:n)) {
  country.i <- as.character(covariates$country.use[i])
  index.i <- which(analysis.data.full$Country==country.i)
  data.i <- analysis.data.full[index.i,]
  policy.i <- data.i$Policy.indicator
  Treatment.within14.prop.i <- c()
  for (j in c(1:day)) {
    if(j < 15){
      Treatment.within14.prop.i[j] <- sum(policy.i[c(1:j)])/length(c(1:j))
    } else if (j >= 15){
      Treatment.within14.prop.i[j] <- sum(policy.i[c((j-14):j)])/15
    }
  }
  Treatment.within14.prop.matrix[i,] <- Treatment.within14.prop.i
}
Treatment.within14.prop <- unlist(c(t(Treatment.within14.prop.matrix)))
analysis.data.full$Treatment.within14.prop <- Treatment.within14.prop


#Define Treatment of recent 4-10 days
Treatment.within4_10.prop.matrix <- matrix(data = NA,nrow = n, ncol = day)
for (i in c(1:n)) {
  country.i <- as.character(covariates$country.use[i])
  index.i <- which(analysis.data.full$Country==country.i)
  data.i <- analysis.data.full[index.i,]
  policy.i <- data.i$Policy.indicator
  Treatment.within4_10.prop.i <- c()
  for (j in c(1:day)) {
    if(j < 7){
      Treatment.within4_10.prop.i[j] <- sum(policy.i[c(1:j)])/length(c(1:j))
    } else if ((j >= 7)&(j <= 12)){
      Treatment.within4_10.prop.i[j] <- sum(policy.i[c((1):(j-6))])/(j-6)
    } else if ((j >= 12)){
      Treatment.within4_10.prop.i[j] <- sum(policy.i[c((j-12):(j-6))])/7
    }
  }
  Treatment.within4_10.prop.matrix[i,] <- Treatment.within4_10.prop.i
}
Treatment.within4_10.prop <- unlist(c(t(Treatment.within4_10.prop.matrix)))
analysis.data.full$Treatment.within4_10.prop <- Treatment.within4_10.prop

#some baseline characters, including confirmed cases at day 1
case.day0.30 <- analysis.data.full$Confirmed.cases[which(analysis.data.full$Rela.Date30==1)-1]
analysis.data.full$case.day0.30 <- rep(case.day0.30, rep(day,n))





################################################################
########
########Define Relative date and treatment, start with 50 cases
########
################################################################
is.case.50 <- rep(0,length(Confirmed.cases))
is.case.50[Confirmed.cases>=50] <- 1
analysis.data.full$is.case.50 <- is.case.50

#Define Relative date 50
Rela.Date50.matrix <- matrix(data = rep(0,length(Confirmed.cases)),nrow = n, ncol = day)
for (i in c(1:n)) {
  country.i <- as.character(covariates$country.use[i])
  index.i <- which(analysis.data.full$Country==country.i)
  data.i <- analysis.data.full[index.i,]
  is.case.50.i <- data.i$is.case.50
  Rela.Date50.i <- is.case.50.i
  start.i <- min(which(Rela.Date50.i==1))
  end.i <- day-start.i+1
  Rela.Date50.i[c(start.i:day)]<-c(1:end.i)
  Rela.Date50.matrix[i,] <- Rela.Date50.i
}

Rela.Date50 <- unlist(c(t(Rela.Date50.matrix)))

analysis.data.full$Rela.Date50 <- Rela.Date50

#Define Treatment
Treatment.cum50.matrix <- matrix(data = NA,nrow = n, ncol = day)
for (i in c(1:n)) {
  country.i <- as.character(covariates$country.use[i])
  index.i <- which(analysis.data.full$Country==country.i)
  data.i <- analysis.data.full[index.i,]
  policy.i <- data.i$Policy.indicator
  is.case.50.i <- data.i$is.case.50
  Rela.Date.i <- data.i$Rela.Date
  Treatment.cum50.i <- rep(0,day)
  if (!all(policy.i==rep(0,day))){
    Treatment.cum50.i[c(1:max(which(is.case.50.i==0)))] <- NA
    start.i <- max(min(which(policy.i==1)),min(which(is.case.50.i==1)))
    end.i <- max(which(policy.i==1))
    Treatment.cum50.i[c(start.i:end.i)]<- c(1:(end.i-start.i+1))
    Treatment.cum50.i[c(end.i:day)] <- rep(Treatment.cum50.i[end.i],(day-end.i+1))}
  Treatment.cum50.matrix[i,] <- Treatment.cum50.i
}

Treatment.cum50 <- unlist(c(t(Treatment.cum50.matrix)))

Treatment.prop50 <-Treatment.cum50/Rela.Date50
max(na.omit(Treatment.prop50))
which(Treatment.prop50==Inf)

analysis.data.full$Treatment.cum50 <- Treatment.cum50
analysis.data.full$Treatment.prop50 <- Treatment.prop50

#some baseline characters, including confirmed cases at day 1
case.day0.50 <- analysis.data.full$Confirmed.cases[which(analysis.data.full$Rela.Date50==1)-1]
analysis.data.full$case.day0.50 <- rep(case.day0.50, rep(day,n))



#Time-variant-covariates
key.use <- as.character(policy$countrycode)
test.raw <- read.csv("data/cross_country/raw/covid-testing-all-observations.csv",encoding = "UTF-8")
test.raw$Date <- as.POSIXct(strptime(as.character(test.raw$Date), format = "%Y-%m-%d"))
test.raw$Date.str <- as.character(test.raw$Date)

test.matrix <- matrix(data = NA,nrow = n, ncol = day)
test.7smooth.matrix <- matrix(data = NA,nrow = n, ncol = day)
test.per.matrix <- matrix(data = NA,nrow = n, ncol = day)
test.per.7smooth.matrix <- matrix(data = NA,nrow = n, ncol = day)


#other policies
school.closure.raw <- 
  read.csv("data/cross_country/interim/school.closure.csv",header = TRUE)
school.closure <- as.numeric(unlist(c(t(school.closure.raw[,c(3:(2+day))]))))
school.closure[school.closure>0] <- 1
analysis.data.full$school.closure <- school.closure
school.closure.pre <- c(NA,school.closure[-length(school.closure)])
school.closure.pre[Date.str=="20200122"] <- NA
analysis.data.full$school.closure.pre <- school.closure.pre
school.closure.day0.30 <- school.closure.pre[which(analysis.data.full$Rela.Date30==1)]
analysis.data.full$school.closure.day0.30 <- rep(school.closure.day0.30,rep(day,n))

school.closure.day0.50 <- school.closure.pre[which(analysis.data.full$Rela.Date50==1)]
analysis.data.full$school.closure.day0.50 <- rep(school.closure.day0.50,rep(day,n))


workplace.close.raw <- 
  read.csv("data/cross_country/interim/workplace.close.csv",header = TRUE)
workplace.close <- as.numeric(unlist(c(t(workplace.close.raw[,c(3:(2+day))]))))
workplace.close[workplace.close>0] <- 1
analysis.data.full$workplace.close <- workplace.close
workplace.close.pre <- c(NA,workplace.close[-length(workplace.close)])
workplace.close.pre[Date.str=="20200122"] <- NA
analysis.data.full$workplace.close.pre <- workplace.close.pre
workplace.close.day0.30 <- workplace.close.pre[which(analysis.data.full$Rela.Date30==1)]
analysis.data.full$workplace.close.day0.30 <- rep(workplace.close.day0.30,rep(day,n))

workplace.close.day0.50 <- workplace.close.pre[which(analysis.data.full$Rela.Date50==1)]
analysis.data.full$workplace.close.day0.50 <- rep(workplace.close.day0.50,rep(day,n))


cancel.public.events.raw <- 
  read.csv("data/cross_country/interim/cancel.public.events.csv",header = TRUE)
cancel.public.events <- as.numeric(unlist(c(t(cancel.public.events.raw[,c(3:(2+day))]))))
cancel.public.events[cancel.public.events>0] <- 1
analysis.data.full$cancel.public.events <- cancel.public.events
cancel.public.events.pre <- c(NA,cancel.public.events[-length(cancel.public.events)])
cancel.public.events.pre[Date.str=="20200122"] <- NA
analysis.data.full$cancel.public.events.pre <- cancel.public.events.pre
cancel.public.events.day0.30 <- 
  cancel.public.events.pre[which(analysis.data.full$Rela.Date30==1)]
analysis.data.full$cancel.public.events.day0.30 <- rep(cancel.public.events.day0.30,rep(day,n))
cancel.public.events.day0.50 <- 
  cancel.public.events.pre[which(analysis.data.full$Rela.Date50==1)]
analysis.data.full$cancel.public.events.day0.50 <- rep(cancel.public.events.day0.50,rep(day,n))




gather.restriction.raw <- 
  read.csv("data/cross_country/interim/gather.restriction.csv",header = TRUE)
gather.restriction <- as.numeric(unlist(c(t(gather.restriction.raw[,c(3:(2+day))]))))
gather.restriction[gather.restriction>0] <- 1
analysis.data.full$gather.restriction <- gather.restriction
gather.restriction.pre <- c(NA,gather.restriction[-length(gather.restriction)])
gather.restriction.pre[Date.str=="20200122"] <- NA
analysis.data.full$gather.restriction.pre <- gather.restriction.pre
gather.restriction.day0.30 <- 
  gather.restriction.pre[which(analysis.data.full$Rela.Date30==1)]
analysis.data.full$gather.restriction.day0.30 <- 
  rep(gather.restriction.day0.30,rep(day,n))
gather.restriction.day0.50 <- 
  gather.restriction.pre[which(analysis.data.full$Rela.Date50==1)]
analysis.data.full$gather.restriction.day0.50 <- 
  rep(gather.restriction.day0.50,rep(day,n))



transport.close.raw <- 
  read.csv("data/cross_country/interim/transport.close.csv",header = TRUE)
transport.close <- as.numeric(unlist(c(t(transport.close.raw[,c(3:(2+day))]))))
transport.close[transport.close>0] <- 1
analysis.data.full$transport.close <- transport.close
transport.close.pre <- c(NA,transport.close[-length(transport.close)])
transport.close.pre[Date.str=="20200122"] <- NA
analysis.data.full$transport.close.pre <- transport.close.pre
transport.close.day0.30 <- 
  transport.close.pre[which(analysis.data.full$Rela.Date30==1)]
analysis.data.full$transport.close.day0.30 <- 
  rep(transport.close.day0.30,rep(day,n))
transport.close.day0.50 <- 
  transport.close.pre[which(analysis.data.full$Rela.Date50==1)]
analysis.data.full$transport.close.day0.50 <- 
  rep(transport.close.day0.50,rep(day,n))


inter.move.restriction.raw <- 
  read.csv("data/cross_country/interim/inter.move.restriction.csv",header = TRUE)
inter.move.restriction <- as.numeric(unlist(c(t(inter.move.restriction.raw[,c(3:(2+day))]))))
inter.move.restriction[inter.move.restriction>0] <- 1
analysis.data.full$inter.move.restriction <- inter.move.restriction
inter.move.restriction.pre <- c(NA,inter.move.restriction[-length(inter.move.restriction)])
inter.move.restriction.pre[Date.str=="20200122"] <- NA
analysis.data.full$inter.move.restriction.pre <- inter.move.restriction.pre
inter.move.restriction.day0.30 <- 
  inter.move.restriction.pre[which(analysis.data.full$Rela.Date30==1)]
analysis.data.full$inter.move.restriction.day0.30 <- 
  rep(inter.move.restriction.day0.30,rep(day,n))
inter.move.restriction.day0.50 <- 
  inter.move.restriction.pre[which(analysis.data.full$Rela.Date50==1)]
analysis.data.full$inter.move.restriction.day0.50 <- 
  rep(inter.move.restriction.day0.50,rep(day,n))



inter.move.travel.raw <- 
  read.csv("data/cross_country/interim/inter.move.travel.control.csv",header = TRUE)
inter.move.travel <- as.numeric(unlist(c(t(inter.move.travel.raw[,c(3:(2+day))]))))
inter.move.travel[inter.move.travel>0] <- 1
analysis.data.full$inter.move.travel <- inter.move.travel
inter.move.travel.pre <- c(NA,inter.move.travel[-length(inter.move.travel)])
inter.move.travel.pre[Date.str=="20200122"] <- NA
analysis.data.full$inter.move.travel.pre <- inter.move.travel.pre
inter.move.travel.day0.30 <- 
  inter.move.travel.pre[which(analysis.data.full$Rela.Date30==1)]
analysis.data.full$inter.move.travel.day0.30 <- 
  rep(inter.move.travel.day0.30,rep(day,n))
inter.move.travel.day0.50 <- 
  inter.move.travel.pre[which(analysis.data.full$Rela.Date50==1)]
analysis.data.full$inter.move.travel.day0.50 <- 
  rep(inter.move.travel.day0.50,rep(day,n))



test.policy.raw <- 
  read.csv("data/cross_country/interim/test.policy.csv",header = TRUE)
test.policy <- as.numeric(unlist(c(t(test.policy.raw[,c(3:(2+day))]))))
test.policy[test.policy>0] <- 1
analysis.data.full$test.policy <- test.policy
test.policy.pre <- c(NA,test.policy[-length(test.policy)])
test.policy.pre[Date.str=="20200122"] <- NA
analysis.data.full$test.policy.pre <- test.policy.pre
test.policy.day0.30 <- 
  test.policy.pre[which(analysis.data.full$Rela.Date30==1)]
analysis.data.full$test.policy.day0.30 <- 
  rep(test.policy.day0.30,rep(day,n))
test.policy.day0.50 <- 
  test.policy.pre[which(analysis.data.full$Rela.Date50==1)]
analysis.data.full$test.policy.day0.50 <- 
  rep(test.policy.day0.50,rep(day,n))



contact.trace.raw <- 
  read.csv("data/cross_country/interim/contact.trace.csv",header = TRUE)
contact.trace <- as.numeric(unlist(c(t(contact.trace.raw[,c(3:(2+day))]))))
contact.trace[contact.trace>0] <- 1
analysis.data.full$contact.trace <- contact.trace
contact.trace.pre <- c(NA,contact.trace[-length(contact.trace)])
contact.trace.pre[Date.str=="20200122"] <- NA
analysis.data.full$contact.trace.pre <- contact.trace.pre
contact.trace.day0.30 <- 
  contact.trace.pre[which(analysis.data.full$Rela.Date30==1)]
analysis.data.full$contact.trace.day0.30 <- 
  rep(contact.trace.day0.30,rep(day,n))
contact.trace.day0.50 <- 
  contact.trace.pre[which(analysis.data.full$Rela.Date50==1)]
analysis.data.full$contact.trace.day0.50 <- 
  rep(contact.trace.day0.50,rep(day,n))



#test number
for (i in c(1:length(key.use))) {
  key.i <- key.use[i]
  index.i <- which(test.raw$ISO.code==key.i)
  data.i <- test.raw[index.i,]
  test.matrix[i,] <- data.i$Daily.change.in.cumulative.total[match(Date.str2,data.i$Date.str)]
  test.7smooth.matrix[i,] <- data.i$X7.day.smoothed.daily.change[match(Date.str2,data.i$Date.str)]
  test.per.matrix[i,] <- data.i$Cumulative.total.per.thousand[match(Date.str2,data.i$Date.str)]
  test.per.7smooth.matrix[i,] <- 
    data.i$X7.day.smoothed.daily.change.per.thousand[match(Date.str2,data.i$Date.str)]
}

Test <- unlist(c(t(test.matrix))); analysis.data.full$Test <- Test
Test.7smooth <- unlist(c(t(test.7smooth.matrix))); analysis.data.full$Test.7smooth <- Test.7smooth
Test.per <- unlist(c(t(test.per.matrix))); analysis.data.full$Test.per <- Test.per
Test.per.7smooth <- unlist(c(t(test.per.7smooth.matrix))); 
analysis.data.full$Test.per.7smooth <- Test.per.7smooth













###Record the date when policy start and end date, when case reach 30
policy.start.date <- c()
policy.end.date <- c()
policy.time <- c()
case.30.date <- c()
case.50.date <- c()
case.end.ob.date <- c()
for (i in c(1:n)) {
  country.i <- as.character(covariates$country.use[i])
  index.i <- which(analysis.data.full$Country==country.i)
  data.i <- analysis.data.full[index.i,]
  policy.i <- data.i$Policy.indicator
  case.30.date[i] <- data.i$Date.str2[min(which(data.i$is.case.30==1))]
  case.50.date[i] <- data.i$Date.str2[min(which(data.i$is.case.50==1))]
  case.end.ob.date[i] <- as.POSIXct(strptime(case.30.date[i], format = "%Y-%m-%d"))+45
  if (all(policy.i==rep(0,day))){policy.start.date[i] <- NA;policy.end.date[i]<- NA;policy.time[i] <- NA}
  else {
  policy.start.date[i]<- as.character(data.i$Date.str2[min(which(policy.i==1))])
  policy.end.date[i]<- as.character(data.i$Date.str2[max(which(policy.i==1))])
  policy.time[i] <- as.POSIXct(strptime(policy.end.date[i], format = "%Y-%m-%d"))-
    as.POSIXct(strptime(policy.start.date[i], format = "%Y-%m-%d"))+1
  }
}
country.name <- as.character(covariates$country.use)
policy.date <- data.frame(country.name,policy.start.date, policy.end.date, 
                          policy.time, case.30.date, case.50.date,
                          case.end.ob.date)
write.csv(policy.date,"data/cross_country/processed/policy_date.csv")
write.csv(policy.date,"results/tables/cross_country/policy_date.csv")

#the time length from the first case in Wuhan and 30 cases in each country:
wuhan_first_time <- as.POSIXct((strptime("2019-12-08", format = "%Y-%m-%d")))
Time_length <- as.POSIXct((strptime(case.30.date, format = "%Y-%m-%d")))-wuhan_first_time
Time_length_50 <- as.POSIXct((strptime(case.50.date, format = "%Y-%m-%d")))-wuhan_first_time
analysis.data.full$Time_length <- rep(Time_length, rep(day,n))
analysis.data.full$Time_length_50 <- rep(Time_length_50, rep(day,n))

covariates$Time_length <- Time_length
covariates$Time_length_50 <- Time_length_50

write.csv(covariates,"data/cross_country/interim/crosscounrty_covariates.csv")
write.csv(analysis.data.full,"data/cross_country/processed/analysis_data_full.csv")

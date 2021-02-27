setwd("D:/R collection/COVID-19/COVID-19/covid_policy_paper/OECD_0212")

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


Policy.indicator <- c(rep(0,length(Policy)))
Policy.indicator[Policy>0] <- 1
Policy.indicator.pre <- c(NA,Policy.indicator[-length(Policy.indicator)])
Policy.indicator.pre[Date.str=="20200122"] <- NA

Policy.indicator.pre2 <- c(NA,Policy.indicator.pre[-length(Policy.indicator.pre)])
Policy.indicator.pre2[Date.str=="20200122"] <- NA
Policy.indicator.pre2[Date.str=="20200123"] <- NA

is.case.30 <- rep(0,length(Confirmed.cases))
is.case.30[Confirmed.cases>=30] <- 1

analysis.data.full <- data.frame(Country, Countrycode, Date.str2, Date, 
                                 Area, GDP, Le, Pop,
                                 Tour.arrive,
                                 Maternal.mortalityratio,
                                 Democ,
                                 National.income,Elderly.pop,
                                 Growth.rate, Growth.rate.pre, Growth.rate.post,
                                 New.Growth.rate, New.Growth.rate.pre,
                                 New.Growth.rate.post,
                                 Confirmed.cases, Confirmed.cases.pre,
                                 newcase, newcase.pre, newcase.post,
                                 is.case.30,
                                 Policy, Policy.indicator, 
                                 Policy.indicator.pre, Policy.indicator.pre2)


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

analysis.data.full$Rela.Date30 <- Rela.Date30 - 1


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

Treatment.within14.prop.pre <- c(NA,Treatment.within14.prop[-length(Treatment.within14.prop)])
Treatment.within14.prop.pre[Date.str=="20200122"] <- NA
analysis.data.full$Treatment.within14.prop.pre <- Treatment.within14.prop.pre


#some baseline characters, including confirmed cases at day 0
case.day.bas <- analysis.data.full$Confirmed.cases[which(analysis.data.full$Rela.Date30==0)]
analysis.data.full$case.day.bas <- rep(case.day.bas, rep(day,n))

#other policies
school.closure.raw <- 
  read.csv("data/cross_country/interim/school.closure.csv",header = TRUE)
school.closure <- as.numeric(unlist(c(t(school.closure.raw[,c(3:(2+day))]))))
school.closure[school.closure>0] <- 1
analysis.data.full$school.closure <- school.closure
school.closure.pre <- c(NA,school.closure[-length(school.closure)])
school.closure.pre[Date.str=="20200122"] <- NA
school.closure.pre2 <- c(NA,school.closure.pre[-length(school.closure.pre)])
school.closure.pre2[Date.str=="20200122"] <- NA
school.closure.pre2[Date.str=="20200123"] <- NA

analysis.data.full$school.closure.pre <- school.closure.pre
analysis.data.full$school.closure.pre2 <- school.closure.pre2


school.closure.bas <- school.closure.pre2[which(analysis.data.full$Rela.Date30==0)]
analysis.data.full$school.closure.bas <- rep(school.closure.bas,rep(day,n))

####

workplace.close.raw <- 
  read.csv("data/cross_country/interim/workplace.close.csv",header = TRUE)
workplace.close <- as.numeric(unlist(c(t(workplace.close.raw[,c(3:(2+day))]))))
workplace.close[workplace.close>0] <- 1
analysis.data.full$workplace.close <- workplace.close
workplace.close.pre <- c(NA,workplace.close[-length(workplace.close)])
workplace.close.pre[Date.str=="20200122"] <- NA

workplace.close.pre2 <- c(NA,workplace.close.pre[-length(workplace.close.pre)])
workplace.close.pre2[Date.str=="20200122"] <- NA
workplace.close.pre2[Date.str=="20200123"] <- NA

analysis.data.full$workplace.close.pre <- workplace.close.pre
analysis.data.full$workplace.close.pre2 <- workplace.close.pre2


workplace.close.bas <- workplace.close.pre2[which(analysis.data.full$Rela.Date30==0)]
analysis.data.full$workplace.close.bas <- rep(workplace.close.bas,rep(day,n))

####

cancel.public.events.raw <- 
  read.csv("data/cross_country/interim/cancel.public.events.csv",header = TRUE)
cancel.public.events <- as.numeric(unlist(c(t(cancel.public.events.raw[,c(3:(2+day))]))))
cancel.public.events[cancel.public.events>0] <- 1
analysis.data.full$cancel.public.events <- cancel.public.events
cancel.public.events.pre <- c(NA,cancel.public.events[-length(cancel.public.events)])
cancel.public.events.pre[Date.str=="20200122"] <- NA

cancel.public.events.pre2 <- c(NA,cancel.public.events.pre[-length(cancel.public.events.pre)])
cancel.public.events.pre2[Date.str=="20200122"] <- NA
cancel.public.events.pre2[Date.str=="20200123"] <- NA

analysis.data.full$cancel.public.events.pre <- cancel.public.events.pre
analysis.data.full$cancel.public.events.pre2 <- cancel.public.events.pre2


cancel.public.events.bas <- cancel.public.events.pre2[which(analysis.data.full$Rela.Date30==0)]
analysis.data.full$cancel.public.events.bas <- rep(cancel.public.events.bas,rep(day,n))

####

gather.restriction.raw <- 
  read.csv("data/cross_country/interim/gather.restriction.csv",header = TRUE)
gather.restriction <- as.numeric(unlist(c(t(gather.restriction.raw[,c(3:(2+day))]))))
gather.restriction[gather.restriction>0] <- 1
analysis.data.full$gather.restriction <- gather.restriction
gather.restriction.pre <- c(NA,gather.restriction[-length(gather.restriction)])
gather.restriction.pre[Date.str=="20200122"] <- NA

gather.restriction.pre2 <- c(NA,gather.restriction.pre[-length(gather.restriction.pre)])
gather.restriction.pre2[Date.str=="20200122"] <- NA
gather.restriction.pre2[Date.str=="20200123"] <- NA

analysis.data.full$gather.restriction.pre <- gather.restriction.pre
analysis.data.full$gather.restriction.pre2 <- gather.restriction.pre2


gather.restriction.bas <- gather.restriction.pre2[which(analysis.data.full$Rela.Date30==0)]
analysis.data.full$gather.restriction.bas <- rep(gather.restriction.bas,rep(day,n))

####

transport.close.raw <- 
  read.csv("data/cross_country/interim/transport.close.csv",header = TRUE)
transport.close <- as.numeric(unlist(c(t(transport.close.raw[,c(3:(2+day))]))))
transport.close[transport.close>0] <- 1
analysis.data.full$transport.close <- transport.close
transport.close.pre <- c(NA,transport.close[-length(transport.close)])
transport.close.pre[Date.str=="20200122"] <- NA

transport.close.pre2 <- c(NA,transport.close.pre[-length(transport.close.pre)])
transport.close.pre2[Date.str=="20200122"] <- NA
transport.close.pre2[Date.str=="20200123"] <- NA

analysis.data.full$transport.close.pre <- transport.close.pre
analysis.data.full$transport.close.pre2 <- transport.close.pre2


transport.close.bas <- transport.close.pre2[which(analysis.data.full$Rela.Date30==0)]
analysis.data.full$transport.close.bas <- rep(transport.close.bas,rep(day,n))

###

inter.move.restriction.raw <- 
  read.csv("data/cross_country/interim/inter.move.restriction.csv",header = TRUE)
inter.move.restriction <- as.numeric(unlist(c(t(inter.move.restriction.raw[,c(3:(2+day))]))))
inter.move.restriction[inter.move.restriction>0] <- 1
analysis.data.full$inter.move.restriction <- inter.move.restriction
inter.move.restriction.pre <- c(NA,inter.move.restriction[-length(inter.move.restriction)])
inter.move.restriction.pre[Date.str=="20200122"] <- NA

inter.move.restriction.pre2 <- c(NA,inter.move.restriction.pre[-length(inter.move.restriction.pre)])
inter.move.restriction.pre2[Date.str=="20200122"] <- NA
inter.move.restriction.pre2[Date.str=="20200123"] <- NA

analysis.data.full$inter.move.restriction.pre <- inter.move.restriction.pre
analysis.data.full$inter.move.restriction.pre2 <- inter.move.restriction.pre2


inter.move.restriction.bas <- inter.move.restriction.pre2[which(analysis.data.full$Rela.Date30==0)]
analysis.data.full$inter.move.restriction.bas <- rep(inter.move.restriction.bas,rep(day,n))

####

inter.move.travel.raw <- 
  read.csv("data/cross_country/interim/inter.move.travel.control.csv",header = TRUE)
inter.move.travel <- as.numeric(unlist(c(t(inter.move.travel.raw[,c(3:(2+day))]))))
inter.move.travel[inter.move.travel>0] <- 1
analysis.data.full$inter.move.travel <- inter.move.travel
inter.move.travel.pre <- c(NA,inter.move.travel[-length(inter.move.travel)])
inter.move.travel.pre[Date.str=="20200122"] <- NA

inter.move.travel.pre2 <- c(NA,inter.move.travel.pre[-length(inter.move.travel.pre)])
inter.move.travel.pre2[Date.str=="20200122"] <- NA
inter.move.travel.pre2[Date.str=="20200123"] <- NA

analysis.data.full$inter.move.travel.pre <- inter.move.travel.pre
analysis.data.full$inter.move.travel.pre2 <- inter.move.travel.pre2


inter.move.travel.bas <- inter.move.travel.pre2[which(analysis.data.full$Rela.Date30==0)]
analysis.data.full$inter.move.travel.bas <- rep(inter.move.travel.bas,rep(day,n))

####

test.policy.raw <- 
  read.csv("data/cross_country/interim/test.policy.csv",header = TRUE)
test.policy <- as.numeric(unlist(c(t(test.policy.raw[,c(3:(2+day))]))))
test.policy[test.policy>0] <- 1
analysis.data.full$test.policy <- test.policy
test.policy.pre <- c(NA,test.policy[-length(test.policy)])
test.policy.pre[Date.str=="20200122"] <- NA

test.policy.pre2 <- c(NA,test.policy.pre[-length(test.policy.pre)])
test.policy.pre2[Date.str=="20200122"] <- NA
test.policy.pre2[Date.str=="20200123"] <- NA

analysis.data.full$test.policy.pre <- test.policy.pre
analysis.data.full$test.policy.pre2 <- test.policy.pre2


test.policy.bas <- test.policy.pre2[which(analysis.data.full$Rela.Date30==0)]
analysis.data.full$test.policy.bas <- rep(test.policy.bas,rep(day,n))



###Record the date when policy start and end date, when case reach 30
policy.start.date <- c()
policy.end.date <- c()
policy.time <- c()
case.30.date <- c()
case.end.ob.date <- c()
for (i in c(1:n)) {
  country.i <- as.character(covariates$country.use[i])
  index.i <- which(analysis.data.full$Country==country.i)
  data.i <- analysis.data.full[index.i,]
  policy.i <- data.i$Policy.indicator
  case.30.date[i] <- data.i$Date.str2[min(which(data.i$is.case.30==1))]
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
                          policy.time, case.30.date,
                          case.end.ob.date)
write.csv(policy.date,"data/cross_country/processed/policy_date.csv")
write.csv(policy.date,"results/tables/cross_country/policy_date.csv")

#the time length from the first case in Wuhan and 30 cases in each country:
wuhan_first_time <- as.POSIXct((strptime("2019-12-08", format = "%Y-%m-%d")))
Time_length <- as.POSIXct((strptime(case.30.date, format = "%Y-%m-%d")))-wuhan_first_time
analysis.data.full$Time_length <- rep(Time_length, rep(day,n))

covariates$Time_length <- Time_length

write.csv(covariates,"data/cross_country/interim/crosscounrty_covariates.csv")
write.csv(analysis.data.full,"data/cross_country/processed/analysis_data_full.csv")

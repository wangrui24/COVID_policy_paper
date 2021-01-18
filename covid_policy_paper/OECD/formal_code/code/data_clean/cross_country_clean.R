setwd("D:/R collection/COVID-19/COVID-19/covid_policy_paper/OECD/formal_code")

#######################################################
# Author: Rui Wang
# Date: 2021/1/18
# Theme: Cross Country data clean
#######################################################

#####section 1: clean the covariates data
area.raw <- read.csv("data/cross_country/raw/API_AG.SRF.TOTL.K2_DS2_en_csv_v2_1307696.csv",encoding = "UTF-8")
gdp.raw <- read.csv("data/cross_country/raw/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_1217511.csv",encoding = "UTF-8")
le.raw <- read.csv("data/cross_country/raw/API_SP.DYN.LE00.IN_DS2_en_csv_v2_1308162.csv",encoding = "UTF-8")
pop.raw <- read.csv("data/cross_country/raw/API_SP.POP.TOTL_DS2_en_csv_v2_1308146.csv",encoding = "UTF-8")

tour.arrive.raw <- read.csv("data/cross_country/raw/International-tourism-number-of-arrivals.csv",encoding = "UTF-8")
maternal.mortalityratio.raw <- read.csv("data/cross_country/raw/Maternal-mortality-ratio.csv",encoding = "UTF-8")
national.income.raw <- read.csv("data/cross_country/raw/National_income.csv",encoding = "UTF-8")

demo.raw <- read.csv("data/cross_country/raw/V-Dem-CY-Core-v10.csv",encoding = "UTF-8")


elderly.pop.raw <- read.csv("data/cross_country/raw/Elderly_population.csv",encoding = "UTF-8")

country.use <- c("Australia","Austria","Belgium","Canada","Chile","Colombia","Czech Republic","Denmark",
                 "Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Israel",
                 "Italy","Japan","Korea, Rep.","Latvia","Lithuania","Luxembourg","Mexico","Netherlands",
                 "New Zealand","Norway","Poland","Portugal","Slovak Republic","Slovenia","Spain","Sweden",
                 "Switzerland","Turkey","United Kingdom")

key.use <- as.character(area.raw$Country.Code[match(country.use,area.raw$X.U.FEFF.Country.Name)])


#World bank data
area <- area.raw$X2018[match(country.use, area.raw$X.U.FEFF.Country.Name)]
gdp <- gdp.raw$X2018[match(country.use, gdp.raw$X.U.FEFF.Country.Name)]
le <- le.raw$X2018[match(country.use, le.raw$X.U.FEFF.Country.Name)]
pop <- pop.raw$X2018[match(country.use, pop.raw$X.U.FEFF.Country.Name)]
national.income <- national.income.raw$X2017[match(country.use, national.income.raw$X.U.FEFF.Country.Name)]


tour.arrive <- tour.arrive.raw$X2018[match(country.use, tour.arrive.raw$Country.Name)]
maternal.mortalityratio <- maternal.mortalityratio.raw$X2017[match(country.use,
                                                                   maternal.mortalityratio.raw$Country.Name)]

####democracy data
demo.2019 <- demo.raw[demo.raw$year==2019,]
demo.2019 <- data.frame(demo.2019$country_name,demo.2019$country_text_id,
                           demo.2019$v2x_polyarchy)
demo.2019$demo.2019.country_name[demo.2019$demo.2019.country_name=="Slovakia"] <- "Slovak Republic"
demo.2019$demo.2019.country_name[demo.2019$demo.2019.country_name=="South Korea"] <- "Korea, Rep."
country.use[is.na(match(country.use, demo.2019$demo.2019.country_name))]
democ <- demo.2019$demo.2019.v2x_polyarchy[match(country.use, demo.2019$demo.2019.country_name)]


#OECD country data
elderly.pop.2018 <- elderly.pop.raw[elderly.pop.raw$TIME==2018,]
elderly.pop <- elderly.pop.2018$Value[match(key.use, elderly.pop.2018$X.U.FEFF.LOCATION)]

crosscounrty_covariates <- data.frame(country.use, key.use, area, gdp, le, 
                                      pop, tour.arrive,
                                      maternal.mortalityratio,
                                      democ, national.income, elderly.pop)
write.csv(crosscounrty_covariates,"data/cross_country/interim/crosscounrty_covariates.csv")


#####section 2: clean the cases data (new confirmed cases and )
country.special <- c("Australia","Canada","Denmark","France","Netherlands","United Kingdom")
country.nospecial <- setdiff(country.use,country.special)

case.raw <- read.csv("data/cross_country/raw/time_series_covid19_confirmed_global.csv")



case.raw$Country.Region <- as.character(case.raw$Country.Region)
case.raw$Country.Region[case.raw$Country.Region=="Czechia"]<-"Czech Republic"
case.raw$Country.Region[case.raw$Country.Region=="Korea, South"]<-"Korea, Rep."
case.raw$Country.Region[case.raw$Country.Region=="Slovakia"]<-"Slovak Republic"


case.use1 <- case.raw[match(country.nospecial,case.raw$Country.Region),-1]

case.use2 <- matrix(data = NA, nrow = length(country.special),ncol = (dim(case.raw)[2]-1))
for (i in c(1:length(country.special))) {
  country.i <- country.special[i]
  index.i <- which(case.raw$Country.Region==country.i)
  case.raw.i <- case.raw[index.i,-c(1:4)]
  case.i <- apply(case.raw.i, 2, sum)
  case.use2[i,] <- as.matrix(cbind(case.raw[index.i[1],c(2:4)], t(as.matrix(case.i))))
}
colnames(case.use2)<-colnames(case.use1)
case.use <- rbind(case.use1,case.use2)

case.use <- case.use[match(country.use, case.use$Country.Region),]
row.names(case.use) <- NULL
day <- 161
write.csv(case.use[,c(1:(day+3))],"data/cross_country/interim/case.csv")



#####section 3: clean the policy data

policy <- matrix(data = NA, nrow = length(country.use),ncol = (day+1))
policy.raw <- read.csv("data/cross_country/raw/OxCGRT_latest.csv")
for (i in c(1:length(country.use))) {
  key.i <- key.use[i]
  index.i <- which(policy.raw$CountryCode==key.i)
  data.i <- policy.raw[index.i,]
  start.key <- which(data.i$Date==20200122)
  end.key <- which(data.i$Date==20200630)
  policy.i <- data.i$C6_Stay.at.home.requirements[c(start.key:end.key)]
  policy[i,] <- t(as.matrix(c(key.i, policy.i)))
}
date <- data.i$Date[c(start.key:end.key)]
colnames(policy) <- c("countrycode",date)
write.csv(policy,"data/cross_country/interim/policy.csv")

#####section 4: clean the time-variant covariate data
#in this part, we include other policies as time-variant covariates
school.closure <- matrix(data = NA, nrow = length(country.use),ncol = (day+1))
workplace.close <- matrix(data = NA, nrow = length(country.use),ncol = (day+1))
cancel.public.events <- matrix(data = NA, nrow = length(country.use),ncol = (day+1))
gather.restriction <- matrix(data = NA, nrow = length(country.use),ncol = (day+1))
transport.close <- matrix(data = NA, nrow = length(country.use),ncol = (day+1))
inter.move.restriction <- matrix(data = NA, nrow = length(country.use),ncol = (day+1))
inter.move.travel.control <- matrix(data = NA, nrow = length(country.use),ncol = (day+1))
test.policy <- matrix(data = NA, nrow = length(country.use),ncol = (day+1))
contact.trace <- matrix(data = NA, nrow = length(country.use),ncol = (day+1))


policy.raw <- read.csv("data/cross_country/raw/OxCGRT_latest.csv")
for (i in c(1:length(country.use))) {
  key.i <- key.use[i]
  index.i <- which(policy.raw$CountryCode==key.i)
  data.i <- policy.raw[index.i,]
  start.key <- which(data.i$Date==20200122)
  end.key <- which(data.i$Date==20200630)
  school.closure.i <- data.i$C1_School.closing[c(start.key:end.key)]
  school.closure[i,] <- t(as.matrix(c(key.i, school.closure.i)))
  
  workplace.close.i <- data.i$C2_Workplace.closing[c(start.key:end.key)]
  workplace.close[i,] <- t(as.matrix(c(key.i, workplace.close.i)))
  
  cancel.public.events.i <- data.i$C3_Cancel.public.events[c(start.key:end.key)]
  cancel.public.events[i,] <- t(as.matrix(c(key.i, cancel.public.events.i)))
  
  gather.restriction.i <- data.i$C4_Restrictions.on.gatherings[c(start.key:end.key)]
  gather.restriction[i,] <- t(as.matrix(c(key.i, gather.restriction.i)))
  
  transport.close.i <- data.i$C5_Close.public.transport[c(start.key:end.key)]
  transport.close[i,] <- t(as.matrix(c(key.i, transport.close.i)))
  
  inter.move.restriction.i <- data.i$C7_Restrictions.on.internal.movement[c(start.key:end.key)]
  inter.move.restriction[i,] <- t(as.matrix(c(key.i, inter.move.restriction.i)))
  
  inter.move.travel.control.i <- data.i$C8_International.travel.controls[c(start.key:end.key)]
  inter.move.travel.control[i,] <- t(as.matrix(c(key.i, inter.move.travel.control.i)))
  
  test.policy.i <- data.i$H2_Testing.policy[c(start.key:end.key)]
  test.policy[i,] <- t(as.matrix(c(key.i, inter.move.travel.control.i)))
  
  contact.trace.i <- data.i$H3_Contact.tracing[c(start.key:end.key)]
  contact.trace[i,] <- t(as.matrix(c(key.i, inter.move.travel.control.i)))
}
date <- data.i$Date[c(start.key:end.key)]

colnames(school.closure) <- c("countrycode",date)
write.csv(school.closure,"data/cross_country/interim/school.closure.csv")

colnames(workplace.close) <- c("countrycode",date)
write.csv(workplace.close,"data/cross_country/interim/workplace.close.csv")

colnames(cancel.public.events) <- c("countrycode",date)
write.csv(cancel.public.events,"data/cross_country/interim/cancel.public.events.csv")

colnames(gather.restriction) <- c("countrycode",date)
write.csv(gather.restriction,"data/cross_country/interim/gather.restriction.csv")

colnames(transport.close) <- c("countrycode",date)
write.csv(transport.close, "data/cross_country/interim/transport.close.csv")

colnames(inter.move.restriction) <- c("countrycode",date)
write.csv(inter.move.restriction,"data/cross_country/interim/inter.move.restriction.csv")

colnames(inter.move.travel.control) <- c("countrycode",date)
write.csv(inter.move.travel.control,"data/cross_country/interim/inter.move.travel.control.csv")

colnames(test.policy) <- c("countrycode",date)
write.csv(test.policy,"data/cross_country/interim/test.policy.csv")

colnames(contact.trace) <- c("countrycode",date)
write.csv(contact.trace,"data/cross_country/interim/contact.trace.csv")
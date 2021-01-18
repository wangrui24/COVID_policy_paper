setwd("D:/R collection/COVID-19/COVID-19/covid_policy_paper/OECD/formal_code")
library("ggplot2")
#######################################################
# Author: Rui Wang
# Date: 2021/1/18
# Theme: Cross Country descriptive statistics
#######################################################

#####
#####PART ONE: descriptive statistics for covariates
#####

t <- 45

data.full <- read.csv("data/cross_country/processed/analysis_data_full.csv")
data.use <- data.full[which((data.full$Rela.Date30<=t)&(data.full$Rela.Date30>=1)),]
data.use$newcase[data.use$newcase<0] <- 0
data.use$newcase.pre[data.use$newcase.pre<0] <- 0




n <- length(data.use$Countrycode[!duplicated(data.use$Countrycode)])

country.use <- data.use$Country[!duplicated(data.use$Country)]
covariates <- read.csv("data/cross_country/interim/crosscounrty_covariates.csv")

covariate.use <- c("gdp","pop","area", "tour.arrive","maternal.mortalityratio","le",  
                    "Time_length","democ", "national.income", "elderly.pop")
bs_formal_name <- bs_formal_name <- c("GDP", "Population", "Area","International tourism number of arrivals",
                                      "Maternal mortality ratio","Life expectancy", "Time length from the first case",
                                      "Democracy", "National income per capital","Elderly population")
col.cov.use <- match(covariate.use,colnames(covariates))
n.cov <- length(covariate.use)

mean.cov <- c()
sd.cov <- c()
max.cov <- c()
min.cov <- c()
median.cov <- c()

for (i in c(1:n.cov)) {
  cov.i <- covariates[col.cov.use[i]]
  cov.i <- unlist(c(cov.i))
  mean.cov[i] <- signif(mean(cov.i),3)
  sd.cov[i] <- signif(sd(cov.i),3)
  max.cov[i] <- signif(max(cov.i),3)
  min.cov[i] <- signif(min(cov.i),3)
  median.cov[i] <- signif(median(cov.i),3)
}
describe.cov <- data.frame(bs_formal_name, mean.cov, sd.cov, max.cov, min.cov, median.cov)
write.csv(describe.cov,"results/tables/cross_country/descriptive_statistics_covariates.csv")


#####
#####PART TWO: descriptive statistics for confirmed cases using relative dates
#####
policy_date <- read.csv("results/tables/cross_country/policy_date.csv")
data.use$Date <- as.POSIXct(strptime(data.use$Date, format = "%Y-%m-%d"))
policy_date$policy.start.date <- 
  as.POSIXct(strptime(policy_date$policy.start.date, format = "%Y-%m-%d"))

country.use <- as.character(country.use)
for (i in c(1:n)) {
  country.i <- country.use[i]
  data.i <- data.use[data.use$Country==country.i,]
  policy.date.i <- policy_date$policy.start.date[policy_date$country.name==country.i]
  ggplot(data.i)+
    geom_point(aes(Date,newcase))+
    geom_line(aes(Date,newcase))+
    geom_vline(aes(xintercept = policy.date.i),color="blue")+
    geom_text(mapping = aes (y = 0,x = policy.date.i,label = as.character(policy.date.i)))+
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
  ggsave(paste(country.i,".newcase.png",sep = ""),width = 8, height = 4,path ="results/graphs/cross_country/new_cases_each_country")
}



#####
#####PART TWO: descriptive statistics for Growth rate using relative dates
#####
policy_date <- read.csv("results/tables/cross_country/policy_date.csv")
data.use$Date <- as.POSIXct(strptime(data.use$Date, format = "%Y-%m-%d"))
policy_date$policy.start.date <- 
  as.POSIXct(strptime(policy_date$policy.start.date, format = "%Y-%m-%d"))
policy_date$case.50.date <- 
  as.POSIXct(strptime(policy_date$case.50.date, format = "%Y-%m-%d"))

country.use <- as.character(country.use)
for (i in c(1:n)) {
  country.i <- country.use[i]
  data.i <- data.use[data.use$Country==country.i,]
  policy.date.i <- policy_date$policy.start.date[policy_date$country.name==country.i]
  date.50.i <- policy_date$case.50.date[policy_date$country.name==country.i]
  ggplot(data.i)+
    geom_point(aes(Date,New.Growth.rate))+
    geom_line(aes(Date,New.Growth.rate))+
    geom_vline(aes(xintercept = policy.date.i),color="blue")+
    geom_vline(aes(xintercept = data.i$Date[30]),color="red")+
    geom_vline(aes(xintercept = date.50.i),color="green")+
    geom_text(mapping = aes (y = 0,x = policy.date.i,label = "policy"),hjust = 0.5)+
    geom_text(mapping = aes (y = 0,x = data.i$Date[30],label = "30 days"))+
    geom_text(mapping = aes (y = 0,x = date.50.i,label = "50 cases"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
  ggsave(paste(country.i,".newgrowthrate.png",sep = ""),width = 8, height = 4,
         path ="results/graphs/cross_country/new_growthrate_each_country")
}
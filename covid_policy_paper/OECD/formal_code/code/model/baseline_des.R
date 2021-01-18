library(geepack)
library(dplyr)
library(rlang)
setwd("D:/R collection/COVID-19/COVID-19/covid_policy_paper/OECD/formal_code")
#source("msmweight.R")
filldata <- read.csv("data/usa/filldata3045.csv",header = T)[,-1]
xy <- read.csv("data/usa/xy3045.csv",header = T)[,-1]

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

baseline.char$totalTestResults_base[which(is.na(baseline.char$totalTestResults_base))] <- 0



library(qwraps2)

our_summary1 <-
  list(
       "Population" =
         list("min"       = ~ min(pop.2019q4_base),
              "max"       = ~ max(pop.2019q4_base),
              "mean (sd)" = ~ qwraps2::mean_sd(pop.2019q4_base)),
       "Area" =
         list("min"       = ~ min(area_base),
       #       "median"    = ~ median(area_base),
              "max"       = ~ max(area_base),
              "mean (sd)" = ~ qwraps2::mean_sd(area_base)),
       "Physicians Number" =
         list("min"       = ~ min(Physicians.num_base),
              "max"       = ~ max(Physicians.num_base),
              "mean (sd)" = ~ qwraps2::mean_sd(Physicians.num_base)),
       "GDP in 2019" =
         list("min"       = ~ min(gdp.2019_base),
              "max"       = ~ max(gdp.2019_base),
              "mean (sd)" = ~ qwraps2::mean_sd(gdp.2019_base)),
       "Income in 2019 quarter 4" =
         list("min"       = ~ min(inc.2019q4_base),
              "max"       = ~ max(inc.2019q4_base),
              "mean (sd)" = ~ qwraps2::mean_sd(inc.2019q4_base)),
       "Political position" = 
         list("Red" = ~ qwraps2::n_perc0(election_base == "R"),
              "Blue"  = ~ qwraps2::n_perc0(election_base == "B")),
       "Number of cases at baseline" =
         list("min"       = ~ min(sumcase_base),
              "max"       = ~ max(sumcase_base),
              "mean (sd)" = ~ qwraps2::mean_sd(sumcase_base)),
       "State of emergency status at baseline" = 
         list("No" = ~ qwraps2::n_perc0(State.of.emergency_base == 0),
              "Yes"  = ~ qwraps2::n_perc0(State.of.emergency_base == 1)),
       "Mandate facemask use by employees in public facing businesses" = 
         list("No" = ~ qwraps2::n_perc0(Mandate.face.mask.use.by.employees.in.public.facing.businesses_base == 0),
              "Yes"  = ~ qwraps2::n_perc0(Mandate.face.mask.use.by.employees.in.public.facing.businesses_base == 1)),
       "K12 schools closed" = 
         list("No" = ~ qwraps2::n_perc0(Date.closed.K.12.schools_base == 0),
              "Yes"  = ~ qwraps2::n_perc0(Date.closed.K.12.schools_base == 1)),
       "Non-essential business closure" = 
       list("No" = ~ qwraps2::n_perc0(Closed.non.essential.businesses_base == 0),
            "Yes"  = ~ qwraps2::n_perc0(Closed.non.essential.businesses_base == 1)),
       "Number of tests at baseline" =
         list("min"       = ~ min(totalTestResults_base),
              "max"       = ~ max(totalTestResults_base),
              "mean (sd)" = ~ qwraps2::mean_sd(totalTestResults_base,na_rm = T)),
       "Population staying at home at baseline" =
         list("min"       = ~ min(Population.Staying.at.Home_base),
              "max"       = ~ max(Population.Staying.at.Home_base),
              "mean (sd)" = ~ qwraps2::mean_sd(Population.Staying.at.Home_base)),
       "Calendar time" = 
         list("min"       = ~ min(days_base),
              "max"       = ~ max(days_base))
     )
whole <- summary_table(baseline.char, our_summary1)
whole

ymd("2020-01-22") + 43


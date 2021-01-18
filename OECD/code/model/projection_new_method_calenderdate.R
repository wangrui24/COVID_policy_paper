setwd("D:/R collection/COVID-19/COVID-19/covid_policy_paper/OECD")

library("ggplot2")

#######################################################
# Author: Rui Wang
# Date: 2021/1/18
# Theme: Projection
#######################################################

data.full <- read.csv("data/cross_country/processed/analysis_data_full.csv")
marginal_result <- read.csv("results/tables/cross_country/30case_45days_weighted_Growthrate_marginal.csv",)
diff <- marginal_result$estimate[2]
intercept <- marginal_result$estimate[1]

country.use <- data.full$Country[!duplicated(data.full$Country)]
n <- length(country.use)
#t <- 80   # Set the time length much longer than we need
day <- sum(!duplicated(data.full$Date.str2))


data.full$a1_counter <- 1
data.full$a1_counter[data.full$Rela.Date30 < 1] <- 
  data.full$Policy.indicator[data.full$Rela.Date30 < 1]


data.full$a0_counter <- 0
data.full$a1_counter[data.full$Rela.Date30 < 1] <- 
  data.full$Policy.indicator[data.full$Rela.Date30 < 1]

#Counterfactual treatment history all
treat.all.prop.matrix <- matrix(data = NA,nrow = n, ncol = day)

for (i in c(1:n)) {
  country.i <- as.character(country.use[i])
  index.i <- which(data.full$Country==country.i)
  data.i <- data.full[index.i,]
  policy.i <- data.i$a1_counter
  treat.all.prop.i <- c()
  for (j in c(1:day)) {
    if(j < 15){
      treat.all.prop.i[j] <- sum(policy.i[c(1:j)])/length(c(1:j))
    } else if (j >= 15){
      treat.all.prop.i[j] <- sum(policy.i[c((j-14):j)])/15
    }
  }
  treat.all.prop.matrix[i,] <- treat.all.prop.i
}


treat.all.prop <- unlist(c(t(treat.all.prop.matrix)))
data.full$treat.all.prop <- treat.all.prop


#Counterfactual treatment history none
treat.none.prop.matrix <- matrix(data = NA,nrow = n, ncol = day)

for (i in c(1:n)) {
  country.i <- as.character(country.use[i])
  index.i <- which(data.full$Country==country.i)
  data.i <- data.full[index.i,]
  policy.i <- data.i$a0_counter
  treat.none.prop.i <- c()
  for (j in c(1:day)) {
    if(j < 15){
      treat.none.prop.i[j] <- sum(policy.i[c(1:j)])/length(c(1:j))
    } else if (j >= 15){
      treat.none.prop.i[j] <- sum(policy.i[c((j-14):j)])/15
    }
  }
  treat.none.prop.matrix[i,] <- treat.none.prop.i
}


treat.none.prop <- unlist(c(t(treat.none.prop.matrix)))
data.full$treat.none.prop <- treat.none.prop


#### create confidence interval

# load the estimated parameters
marginal_result <- read.csv("results/tables/cross_country/30case_45days_weighted_Growthrate_marginal.csv",)
diff <- marginal_result$estimate[2]
intercept <- marginal_result$estimate[1]
sd <- marginal_result$std.error[2]
cum0 <- sum(data.full$Confirmed.cases[data.full$Date.str2=="2020-01-23"])

# cum 0 is the cumulated cases in time 0

#quantile 0.025 and 0.975
quant_25 <- function(x){
  return(quantile(x, prob = 0.025))
}

quant_975 <- function(x){
  return(quantile(x, prob = 0.975))
}

data.use <- data.full[which((data.full$Date.str2!="2020-01-22")&
                        (data.full$Date.str2!="2020-01-23")),] #a copy of data.full
t <- sum(!duplicated(data.use$Date.str2))

confidence_interval <- function(a_bar, iter = 5000){ 
  #a_bar is the treatment history
  #a_bar should be a (day*n)*1 vector, with all element range 0/15, 1/15,...,15/15
  
  #a_bar <- data.use$treat.all.prop; iter = 5000 #for test
  # create a empty vector to store the results
  cumcase <- matrix(data = NA, nrow = iter, ncol = t)
  
  for (i in c(1:iter)) {
    #i = 1 #for test
    print(i)
    # sample from the asymptotic distribution of beta
    beta_i <- rnorm(1, mean = diff, sd = sd)
    #beta_i <- diff   # for test
    # predict the growth rate
    growth_rate_counter_i <- data.use$New.Growth.rate + 
      beta_i*(a_bar - data.use$Treatment.within14.prop)
    
    # predict the new case number
    newcase_counter_matrix_i <- matrix(data = NA,nrow = n, ncol = t)
    for (j in c(1:n)) {
      country.j <- as.character(country.use[j])
      index.j <- which(data.use$Country==country.j)
      data.j <- data.use[index.j,]
      growthrate_counter <- growth_rate_counter_i[index.j]
      newcase.counter.j <- data.j$newcase
      start.index <- which(data.j$Rela.Date30==1)
      for (k in c(start.index:t)) {
        newcase.counter.j[k] <- max((exp(growthrate_counter[k]+
                                           log(newcase.counter.j[k-1]+1))-1),0)
      }
      newcase_counter_matrix_i[j,] <- newcase.counter.j
    }
    # predict the cumulated case：
    cumcase_i <- cumsum(apply(newcase_counter_matrix_i, 2, sum)) + cum0
    cumcase[i,] <- cumcase_i
  }
  low_bound <- apply(cumcase, 2, quant_25)
  up_bound <- apply(cumcase, 2, quant_975)
  predicted <- apply(cumcase, 2, mean)
  date <- c(3:day)
  return(data.frame(date, low_bound, up_bound))
}
set.seed(1234)

predicted_case <- function(a_bar){
  # predict the growth rate
  growth_rate_counter <- data.use$New.Growth.rate + 
    diff*(a_bar - data.use$Treatment.within14.prop)
  # predict the new cases
  newcase_counter_matrix <- matrix(data = NA,nrow = n, ncol = t)
  for (j in c(1:n)) {
    country.j <- as.character(country.use[j])
    index.j <- which(data.use$Country==country.j)
    data.j <- data.use[index.j,]
    growthrate_counter <- growth_rate_counter[index.j]
    newcase.counter.j <- data.j$newcase
    start.index <- which(data.j$Rela.Date30==1)
    for (k in c(start.index:t)) {
      newcase.counter.j[k] <- max((exp(growthrate_counter[k]+
                                         log(newcase.counter.j[k-1]+1))-1),0)
    }
    newcase_counter_matrix[j,] <- newcase.counter.j
  }
  # predict the cumulated case：
  cumcase_predicted <- cumsum(apply(newcase_counter_matrix, 2, sum)) + cum0
  return(cumcase_predicted)
} 

confin_treat_all <- confidence_interval(data.use$treat.all.prop)
confin_treat_all$cumcase_predicted <- predicted_case(data.use$treat.all.prop)
confin_treat_none <- confidence_interval(data.use$treat.none.prop)
confin_treat_none$cumcase_predicted <- predicted_case(data.use$treat.none.prop)

write.csv(confin_treat_all, 
          file = "data/cross_country/processed/confin_treat_all_calenderdate.csv")
write.csv(confin_treat_none, 
          file = "data/cross_country/processed/confin_treat_none_calenderdate.csv")

write.csv(confin_treat_all, 
          file = "results/tables/cross_country/sensitivity_analysis/confin_treat_all_calenderdate.csv")
write.csv(confin_treat_none, 
          file = "results/tables/cross_country/sensitivity_analysis/confin_treat_none_calenderdate.csv")
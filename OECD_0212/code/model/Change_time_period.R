setwd("D:/R collection/COVID-19/COVID-19/covid_policy_paper/OECD_0212")

library("geepack")
library("Hmisc")
library("ggplot2")
library("doBy")
library("broom")
#######################################################
# Author: Rui Wang
# Date: 2021/1/18
# Theme: Cross Country data analysis
#######################################################

bs_confounder <- c("GDP", "Pop", "Area","Tour.arrive",
                   "Maternal.mortalityratio","Le", "Time_length",
                   "Democ", "National.income", "Elderly.pop",
                   "case.day.bas","school.closure.bas","workplace.close.bas",
                   "cancel.public.events.bas","test.policy.bas",
                   "transport.close.bas",
                   "inter.move.restriction.bas"
)
bs_formal_name <- c("GDP", "Population", "Area","International tourism number of arrivals",
                    "Maternal mortality ratio","Life expectancy", "Time length from the first case",
                    "Democracy", "National income per capital", "Elderly population",
                    "Total number of cases*","School closing*","Workplace closing*",
                    "Cancel public events*","Testing policy*",
                    "Close public transport*",
                    "Restrictions on internal movement*")

tv_confounder <- c("New.Growth.rate.pre","school.closure.pre2","workplace.close.pre2",
                   "cancel.public.events.pre2","test.policy.pre2",
                   "transport.close.pre2",
                   "inter.move.restriction.pre2")


n <- 36

#######################################################
##########
########## analysis from 30 cases, a marginal model，t = 45
##########
#######################################################


#####section 1: Treatment model
data.full <- read.csv("data/cross_country/processed/analysis_data_full.csv")
country.set <- data.full$Country[!duplicated(data.full$Country)]

growth_rate_estimate <- c()
growth_rate_sd <- c()
growth_rate_CI <- c()
growth_rate_CI_low <- c()
growth_rate_CI_up <- c()
for (t in c(30:50)) {
  print(t)
  data.use <- data.full[which((data.full$Rela.Date30<=(t-1))&(data.full$Rela.Date30>=0)),]
  data.use$Policy.indicator.pre2[which(data.use$Rela.Date30==0)]<-NA
  
  #data.use <- data.use[!(data.use$Country=="Iceland"),]
  n <- length(data.use$Countrycode[!duplicated(data.use$Countrycode)])
  
  weight <- c()
  weight[data.use$Policy.indicator.pre2==1]<-1
  
  #weight model
  data.logit <- data.use[which(data.use$Policy.indicator.pre2==0),]
  
  tspline <- rcspline.eval(data.logit$Rela.Date30, 
                           knots=quantile(data.logit$Rela.Date30,c(0, 0.33, 0.66,
                                                                   1)))
  
  formula.wm1 <- paste0("Policy.indicator.pre","~",
                        paste(c("Rela.Date30","tspline",bs_confounder,tv_confounder),
                              collapse="+"))
  weight.model <- glm(formula.wm1,
                      data = data.logit, family = binomial(link = "logit"))
  
  summary(weight.model)
  
  
  weight[which(data.use$Policy.indicator.pre2==0)] <- predict(weight.model,type = "response")
  
  
  
  weight2 <- c()
  weight2[data.use$Policy.indicator.pre2==1]<-1
  weight2.model <- glm(Policy.indicator.pre ~ Rela.Date30 + tspline,
                       data = data.logit, family = binomial(link = "logit"))
  summary(weight2.model)
  weight2[which(data.use$Policy.indicator.pre2==0)] <- predict(weight2.model,type = "response")
  
  w1 <- data.use$Policy.indicator.pre*weight + (1-data.use$Policy.indicator.pre)*(1-weight)
  w2 <- data.use$Policy.indicator.pre*weight2 + (1-data.use$Policy.indicator.pre)*(1-weight2)
  
  w2.matrix <- matrix(w2, ncol = n, nrow = t)
  w1.matrix <- matrix(w1, ncol = n, nrow = t)
  
  w2.matrix[is.na(w2.matrix)] <- 1
  w1.matrix[is.na(w1.matrix)] <- 1
  
  sw1.matrix <- w1.matrix
  for (i in c(1:n)) {
    for (j in c(1:t)) {
      sw1.matrix[j,i] <- prod(w1.matrix[c(1:j),i])
    }
  }
  
  sw2.matrix <- w2.matrix
  for (i in c(1:n)) {
    for (j in c(1:t)) {
      sw2.matrix[j,i] <- prod(w2.matrix[c(1:j),i])
    }
  }
  
  sw.matrix <- sw2.matrix/sw1.matrix
  stable.weight <- unlist(c(sw.matrix))
  boxplot(stable.weight)
  
  #draw the picture for weights
  log.stablized.weights <- log(stable.weight)
  dat.sw <- as.data.frame(log.stablized.weights)
  
  
  # sw <- apply(matrix(w2, ncol = n, nrow = t),2,prod)/apply(matrix(w1, ncol = n, nrow = t),2,prod)
  # boxplot(sw)
  # stable.weight <- rep(sw, rep(t,n))
  
  #####section 2: Marginal structure model
  fit <- geeglm(New.Growth.rate ~ Treatment.within14.prop.pre, 
                id= as.factor(Country),
                data = data.use, corstr = "ar1") 
  fit.table <- tidy(fit, conf.int = TRUE); fit.table
  fit.table$"95% CI" <- paste0("[",signif(fit.table$conf.low,3),
                               ",",signif(fit.table$conf.high,3),"]")
  fit.table <- subset(fit.table, select = c(1,2,3,5,8))
  
  fit.table$estimate <- signif(fit.table$estimate,3)
  fit.table$std.error <- signif(fit.table$std.error,3)
  fit.table$p.value <- signif(fit.table$p.value,3)
  colnames(fit.table)[1] <- "Variable"
  fit.table$Variable <- c("Intercept","Stay at home policy history")
  
  
  
  fit.w <- geeglm(New.Growth.rate ~ Treatment.within14.prop.pre, 
                  weights = stable.weight,
                  data = data.use, id= as.factor(Country), corstr = "ar1")
  fit.table <- tidy(fit.w, conf.int = TRUE); fit.table
  growth_rate_CI_low[t-29] <- signif(fit.table$conf.low,3)[2]
  growth_rate_CI_up[t-29] <- signif(fit.table$conf.high,3)[2]
  fit.table$"95% CI" <- paste0("[",signif(fit.table$conf.low,3),
                               ",",signif(fit.table$conf.high,3),"]")
  fit.table <- subset(fit.table, select = c(1,2,3,5,8))
  
  fit.table$estimate <- signif(fit.table$estimate,3)
  fit.table$std.error <- signif(fit.table$std.error,3)
  fit.table$p.value <- signif(fit.table$p.value,3)
  colnames(fit.table)[1] <- "Variable"
  fit.table$Variable <- c("Intercept","Stay at home policy history")
  growth_rate_estimate[t-29] <- fit.table$estimate[2]
  growth_rate_sd[t-29] <- fit.table$std.error[2]
  growth_rate_CI[t-29] <- fit.table$`95% CI`[2]
}
time_period <- c(30:50)
change_time_period <- data.frame(time_period, growth_rate_estimate, 
                            growth_rate_sd, growth_rate_CI, growth_rate_CI_low,
                            growth_rate_CI_up)

t_main  <- which(change_time_period$time_period ==45)

ggplot(data = change_time_period, aes(y = time_period, x = growth_rate_estimate))+
  geom_point(color = "#1B9E77")+
  geom_point(data = change_time_period[t_main,], color = "#FF3300")+
  geom_pointrange(aes(xmin=growth_rate_CI_low, xmax=growth_rate_CI_up), 
                  color = "#6495ED")+
  geom_pointrange(aes(xmin=growth_rate_CI_low, xmax=growth_rate_CI_up),
                  data = change_time_period[t_main,],
                  color = "#FF3300")+
  scale_x_continuous(limits = c(-0.3, -0.1))+
  scale_y_continuous(breaks = c(30:50))+
  ylab("Relative dates included")+
  xlab("Estimated effect on daily growth rate")
ggsave("change_time_period.png" , plot = last_plot(), width = 7, height = 10,
       path = "results/graphs/cross_country/sensitivity_analysis")
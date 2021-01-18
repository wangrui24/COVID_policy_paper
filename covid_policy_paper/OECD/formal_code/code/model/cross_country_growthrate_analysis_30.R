setwd("D:/R collection/COVID-19/COVID-19/covid_policy_paper/OECD/formal_code")
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
                   "case.day0.30","school.closure.day0.30","workplace.close.day0.30",
                   "cancel.public.events.day0.30","test.policy.day0.30",
                   "transport.close.day0.30","inter.move.restriction.day0.30")
bs_formal_name <- c("GDP", "Population", "Area","International tourism number of arrivals",
                    "Maternal mortality ratio","Life expectancy", "Time length from the first case",
                    "Democracy", "National income per capital", "Elderly population",
                    "Total number of cases*","School closing*","Workplace closing*",
                    "Cancel public events*","Testing policy*",
                    "Close public transport*","Restrictions on internal movement*")

tv_confounder <- c("New.Growth.rate","school.closure","workplace.close",
                   "cancel.public.events","test.policy","transport.close",
                   "inter.move.restriction")

#######################################################
# Author: Rui Wang
# Date: 2020/8/24
# Theme: Cross Country data analysis
#######################################################
n <- 36
t <- 45

#######################################################
##########
########## analysis from 30 cases, a marginal model，t = 45
##########
#######################################################


#####section 1: Treatment model
data.full <- read.csv("data/cross_country/processed/analysis_data_full.csv")
data.use <- data.full[which((data.full$Rela.Date30<=t)&(data.full$Rela.Date30>=1)),]
data.use$newcase[data.use$newcase<0] <- 0
data.use$newcase.pre[data.use$newcase.pre<0] <- 0
data.use$newcase.post[data.use$newcase.post<0] <- 0

n <- length(data.use$Countrycode[!duplicated(data.use$Countrycode)])

weight <- c()
weight[data.use$Policy.indicator.pre==1]<-1

#weight model
data.logit <- data.use[which(data.use$Policy.indicator.pre==0),]

tspline <- rcspline.eval(data.logit$Rela.Date30, 
                         knots=quantile(data.logit$Rela.Date30,c(0, 0.33, 0.66, 1)))

formula.wm1 <- paste0("Policy.indicator","~",
                      paste(c("Rela.Date30","tspline",bs_confounder,tv_confounder),
                            collapse="+"))
weight.model <- glm(formula.wm1,
                      data = data.logit, family = binomial(link = "logit"))

summary(weight.model)


weight[data.use$Policy.indicator.pre==0] <- predict(weight.model,type = "response")



weight2 <- c()
weight2[data.use$Policy.indicator.pre==1]<-1
weight2.model <- glm(Policy.indicator ~ Rela.Date30 + tspline,
                     data = data.logit, family = binomial(link = "logit"))
summary(weight2.model)
weight2[data.use$Policy.indicator.pre==0] <- predict(weight2.model,type = "response")

w1 <- data.use$Policy.indicator*weight + (1-data.use$Policy.indicator)*(1-weight)
w2 <- data.use$Policy.indicator*weight2 + (1-data.use$Policy.indicator)*(1-weight2)

w2.matrix <- matrix(w2, ncol = n, nrow = t)
w1.matrix <- matrix(w1, ncol = n, nrow = t)

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
ggplot(dat.sw)+geom_boxplot(aes(y = log.stablized.weights))


#####section 2: Marginal structure model
fit <- geeglm(New.Growth.rate.post ~ Treatment.within14.prop, 
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



write.csv(fit.table, 
          file = "results/tables/cross_country/30case_45days_unweighted_Growthrate_marginal.csv")

fit.w <- geeglm(New.Growth.rate.post ~ Treatment.within14.prop, 
                weights = stable.weight,
                data = data.use, id= as.factor(Country), corstr = "ar1")
fit.table <- tidy(fit.w, conf.int = TRUE); fit.table
fit.table$"95% CI" <- paste0("[",signif(fit.table$conf.low,3),
                             ",",signif(fit.table$conf.high,3),"]")
#fit.table <- subset(fit.table, select = c(1,2,3,5,8))

fit.table$estimate <- signif(fit.table$estimate,3)
fit.table$std.error <- signif(fit.table$std.error,3)
fit.table$p.value <- signif(fit.table$p.value,3)
fit.table$conf.low <- signif(fit.table$conf.low,3)
fit.table$conf.high <- signif(fit.table$conf.high,3)
colnames(fit.table)[1] <- "Variable"
fit.table$Variable <- c("Intercept","Stay at home policy history")


write.csv(fit.table, 
          file = "results/tables/cross_country/30case_45days_weighted_Growthrate_marginal.csv")

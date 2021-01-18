setwd("D:/R collection/COVID-19/COVID-19/covid_policy_paper/OECD/formal_code")
library("ggplot2")
library("reshape")
#######################################################
# Author: Rui Wang
# Date: 2020/1/18
# Theme: Projection Picture with calender date
#######################################################
n <- 36

###The sum of cum cases
data.full <- read.csv("data/cross_country/processed/analysis_data_full.csv")
confin_treat_all <- read.csv("data/cross_country/processed/confin_treat_all_calenderdate.csv")
confin_treat_none <- read.csv("data/cross_country/processed/confin_treat_none_calenderdate.csv")

data.use <- data.full[which((data.full$Date.str2!="2020-01-22")&
                              (data.full$Date.str2!="2020-01-23")),] #a copy of data.full

calander_date <- data.use$Date[!duplicated(data.use$Date)]
t <- length(calander_date)
cum_cases_real <- c()

for (i in c(1:t)) {
  data.i <- data.use[which(data.use$Date.str2 == calander_date[i]),]
  cum_cases_real[i] <- sum(data.i$Confirmed.cases)
}
cum_cases_counter_none <- confin_treat_none$cumcase_predicted
cum_cases_counter_all <- confin_treat_all$cumcase_predicted

counter.dat <- data.frame(calander_date,cum_cases_real,cum_cases_counter_none,
                          cum_cases_counter_all)

counter.plot.dat <- melt(counter.dat, id.vars = "calander_date", 
                         measure.vars = c("cum_cases_real", "cum_cases_counter_none",
                                          "cum_cases_counter_all"),
                         variable_name = "type")
counter.plot.dat$low_bound <- c(cum_cases_real, confin_treat_none$low_bound, 
                                confin_treat_all$low_bound)
counter.plot.dat$up_bound <- c(cum_cases_real, confin_treat_none$up_bound, 
                               confin_treat_all$up_bound)
counter.plot.dat$type <- as.character(counter.plot.dat$type)
counter.plot.dat$type[counter.plot.dat$type=="cum_cases_real"] <- "Reality"
counter.plot.dat$type[counter.plot.dat$type=="cum_cases_counter_all"] <- "All treated scenario"
counter.plot.dat$type[counter.plot.dat$type=="cum_cases_counter_none"] <- "All untreated scenario"
counter.plot.dat$calander_date <- as.POSIXct(strptime(counter.plot.dat$calander_date, 
                                                           format = "%Y-%m-%d"))

index_keep <- which((counter.plot.dat$calander_date>=as.POSIXct(strptime("2020-03-01", 
                                                                        format = "%Y-%m-%d")))&
                    (counter.plot.dat$calander_date<=as.POSIXct(strptime("2020-04-20", 
                                                                           format = "%Y-%m-%d"))))
counter.plot.dat <- counter.plot.dat[index_keep,]


break_date <- as.POSIXct(strptime(c("2020-03-01", "2020-03-11", "2020-03-21",
                                "2020-03-31", "2020-04-10", "2020-04-20"
                                ), format = "%Y-%m-%d"))
break_date_label <- c("Mar 1", "Mar 11", "Mar 21", "Mar 31", "Apr 10", "Apr 20")
y_label <- c(expression(10^{4}),expression(10^{5}),expression(10^{6}),
             expression(10^{7}),expression(10^{8}))
#labels = c("10^4","10^5","10^6", "10^7","10^8")

ggplot(data = counter.plot.dat, aes(x = calander_date, y = value,
                                    color = type, fill = type))+
  geom_point()+
  geom_ribbon(aes(ymin = low_bound,
                  ymax = up_bound),
              alpha = 0.2)+
  geom_line() + 
  ylab("Number of Cumulative Cases")+
  xlab("Calender Time")+
  scale_x_datetime(breaks = break_date, date_labels = break_date_label)+
  scale_y_log10(breaks=c(10^4,10^5,10^6,10^7,10^8), labels = y_label)+
  theme(axis.text.x = element_text(size = 12,color="black"),
        axis.text.y = element_text(size = 12,color="black"))+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))+
  guides(fill=guide_legend(title = "Types of policies"))+
  guides(color=guide_legend(title = "Types of policies"))

ggsave("projection_calenderdate.png" , plot = last_plot(), width = 10, height = 5,
       path = "results/graphs/cross_country/projection_results")

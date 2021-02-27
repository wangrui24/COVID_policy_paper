setwd("/Users/fanxia/Dropbox/covid")
library(dplyr)
projectdata <- read.csv("projectdata.csv",header = T)[,-1]
#projectdata <- filldata.pic
thetadistn <- read.csv("theta.csv",header = T)[,-1]

all.lag1 <- all.lag0 <- 0
for(i in 1:15)
{
  lagx.temp1 <- paste("lagx_alltreat",i,sep="")
  lagx.temp0 <- paste("lagx_nontreat",i,sep="")
  all.lag1 <- paste(all.lag1,lagx.temp1,sep=",")
  all.lag0 <- paste(all.lag0,lagx.temp0,sep=",")
  lag.formula.temp1 <- paste0("lag(treat1,",i,",default=0)")
  lag.formula.temp0 <- paste0("lag(treat0,",i,",default=0)")
  projectdata %>% group_by(id) %>% mutate(!!lagx.temp1 := !!rlang::parse_expr(lag.formula.temp1),
                                          !!lagx.temp0 := !!rlang::parse_expr(lag.formula.temp0)) -> projectdata
}
projectdata <- data.frame(projectdata)
lagtreat.vec1 <- unlist(strsplit(all.lag1,","))[-1]
projectdata$alltreat15 <- apply(projectdata[,lagtreat.vec1],1,mean)
lagtreat.vec0 <- unlist(strsplit(all.lag0,","))[-1]
projectdata$nontreat15 <- apply(projectdata[,lagtreat.vec0],1,mean)
projectdata <- select(projectdata,-c(all_of(lagtreat.vec0),all_of(lagtreat.vec1),"treat1","treat0"))

single_group_newcase_cal <- function(data_group)
{
  n <- nrow(data_group)
  newcase1 <- newcase0 <- rep(NA,n)
  newcase1[1] <- newcase0[1] <- data_group$newcase[1]
  for(i in 2:n)
  {
    newcase1[i] <- with(data_group,max((exp(growth_rate1[i]+log(newcase1[i-1]+1))-1),0))
    newcase0[i] <- with(data_group,max((exp(growth_rate0[i]+log(newcase0[i-1]+1))-1),0))
  }
  return(list(newcase1,newcase0))
}

changetheta <- function(theta,projectdata)
{
  projectdata$theta <- theta
  
  projectdata %>% mutate(growth_rate1 = growth_rate + theta*(alltreat15 - treat_history_15), 
                         growth_rate0 = growth_rate + theta*(nontreat15 - treat_history_15)) -> projectdata
  
  newcase1 <- newcase0 <- c()
  for(i in unique(projectdata$id))
  {
    data_group <- subset(projectdata,id == i)
    temp <- single_group_newcase_cal(data_group)
    newcase1.temp <- temp[[1]]
    newcase0.temp <- temp[[2]]
    newcase1 <- c(newcase1, newcase1.temp)
    newcase0 <- c(newcase0, newcase0.temp)
  }
  
  projectdata$newcase1 <- newcase1
  projectdata$newcase0 <- newcase0
  
  projectdata %>% group_by(id) %>% mutate(projectcase1 = cumsum(newcase1)+sumcase_base,
                                          projectcase0 = cumsum(newcase0)+sumcase_base) -> projectdata
  
  projectdata[,c("projectcase1","projectcase0")]
}

point <- changetheta(thetadistn$estimate,projectdata)

B <- 5000
theta <- matrix(rnorm(B, mean = thetadistn$estimate, sd = thetadistn$std.error),nrow=B)
point.rd <- apply(theta,1,changetheta,projectdata=projectdata)
theta1 <- sapply(point.rd, `[[`, 1)
theta0 <- sapply(point.rd, `[[`, 2)

lower.theta1 <- apply(theta1,1,quantile,probs = 0.025)  
upper.theta1 <- apply(theta1,1,quantile,probs = 0.975)  

lower.theta0 <- apply(theta0,1,quantile,probs = 0.025)  
upper.theta0 <- apply(theta0,1,quantile,probs = 0.975) 

projectwci_bystate <- data.frame(projectdata$id,projectdata$Date,projectdata$sumcase,point$projectcase1,lower.theta1,upper.theta1,
                            point$projectcase0,lower.theta0,upper.theta0)
colnames(projectwci_bystate)[c(1:4,7)] <- c("id","Time","realcase","projectcase1","projectcase0")
#projectwci_bystate$rtime <- projectwci_bystate$rtime - 1
projectwci_bystate %>% group_by(Time) %>% summarise(cumcasereal = sum(realcase),
                                                     cumcase1 = sum(projectcase1),
                                                     cumcase1.lower = sum(lower.theta1),
                                                     cumcase1.upper = sum(upper.theta1),
                                                     cumcase0 = sum(projectcase0),
                                                     cumcase0.lower = sum(lower.theta0),
                                                     cumcase0.upper = sum(upper.theta0)
                                                     ) -> plotdata.ori

plotdata.ori <- data.frame(plotdata.ori)

library("reshape")
counter.plot.dat <- melt(plotdata.ori[,c("Time","cumcasereal","cumcase1","cumcase0")], 
                         id.vars = "Time", variable_name = "type")
counter.plot.dat$low_bound <- melt(plotdata.ori[,c("Time","cumcasereal","cumcase1.lower","cumcase0.lower")], 
                                   id.vars = "Time", variable_name = "type")$value
counter.plot.dat$up_bound <- melt(plotdata.ori[,c("Time","cumcasereal","cumcase1.upper","cumcase0.upper")], 
                                   id.vars = "Time", variable_name = "type")$value

counter.plot.dat$type <- as.character(counter.plot.dat$type)
counter.plot.dat$type[counter.plot.dat$type=="cumcasereal"] <- "Reality"
counter.plot.dat$type[counter.plot.dat$type=="cumcase1"] <- "All treated scenario"
counter.plot.dat$type[counter.plot.dat$type=="cumcase0"] <- "All untreated scenario"

library(ggplot2)
library(lubridate)
counter.plot.dat$Time <- ymd(counter.plot.dat$Time)
library(scales)

break_date <- as.POSIXct(strptime(c("2020-03-01", "2020-03-10", "2020-03-19",
                                    "2020-03-28", "2020-04-06","2020-04-15"), format = "%Y-%m-%d"))
break_date_label <- c("Mar 1", "Mar 10", "Mar 19", "Mar 28", "Apr 6", "Apr 15")
counter.plot.dat$Time <- as.POSIXct(counter.plot.dat$Time)
ggplot(data = counter.plot.dat, aes(x = Time, y = value,
                                    color = type, fill = type))+
  geom_point()+
  geom_ribbon(aes(ymin = low_bound,
                  ymax = up_bound),
              alpha = 0.2)+
  geom_line() + 
  ylab("Number of Cumulative Cases")+
  xlab("Calender Time")+
  scale_x_datetime(breaks = break_date, date_labels = break_date_label)+
  scale_y_log10(breaks=c(10^2,10^3,10^4,10^5,10^6,10^7),labels = trans_format("log10", math_format(10^.x)))+
  theme(axis.text.x = element_text(size = 12,color="black"),
        axis.text.y = element_text(size = 12,color="black"))+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))+
  guides(fill=guide_legend(title = "Types of policies"))+
  guides(color=guide_legend(title = "Types of policies"))

ggsave("projection_US_calendar_new.png" , plot = last_plot(), width = 10, height = 5)


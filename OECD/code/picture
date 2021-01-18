library("ggplot2")
library("reshape")
setwd("D:/R collection/COVID-19/COVID-19/covid_policy_paper/OECD")

#######################################################
# Author: Rui Wang
# Date: 2021/1/18
# Theme: graphs of policies
#######################################################
t <- 45
n <- 36

data.full <- read.csv("data/cross_country/processed/analysis_data_full.csv")
data.use <- data.full[which((data.full$Rela.Date30<=t)&(data.full$Rela.Date30>=1)),]

rela_time_30 <- c(1:45)
stay_at_home_num <- c()
school_close_num <- c()
workplace_close_num <- c()
cancel_public_events_num <- c()
test_policy_num <- c()
cancel_trans_num <- c()
inter_move_num <- c()

#create table for graphs
for (i in c(1:t)) {
  data.i <- data.use[which(data.use$Rela.Date30 == i),]
  stay_at_home_num[i] <- sum(data.i$Policy.indicator)
  school_close_num[i] <- sum(data.i$school.closure)
  workplace_close_num[i] <- sum(data.i$workplace.close)
  cancel_public_events_num[i] <- sum(data.i$cancel.public.events)
  test_policy_num[i] <- sum(data.i$test.policy)
  cancel_trans_num[i] <- sum(data.i$transport.close)
  inter_move_num[i] <- sum(data.i$inter.move.restriction)
}

policy_graph_dat <- data.frame(rela_time_30, stay_at_home_num, school_close_num,
                               workplace_close_num, cancel_public_events_num,
                               test_policy_num, cancel_trans_num, inter_move_num)
policy_num <- dim(policy_graph_dat)[2]-1

#plot the number of policy
y_labs <- c("Number of stay at home policies",
            "Number of school closing policies",
            "Number of workplace closing policies",
            "Number of cancel public events policies",
            "Number of testing policies",
            "Number of close public transport policies",
            "Number of restrictions on internal movement policies")




#stay at home policy
for (i in c(1:policy_num)) {
  y_lab <- y_labs[i]
  ggplot(data = policy_graph_dat, aes(x = rela_time_30, y = policy_graph_dat[,(i+1)]))+
    geom_point()+
    geom_line()+
    scale_x_continuous(breaks=seq(1,45,4))+
    xlab("Relative time")+
    scale_y_continuous(breaks=seq(1,36,5),limits=c(0,36))+
    ylab(y_lab)+
    theme_bw()+
    theme(panel.border = element_blank(),panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))
  file_name <- paste0(y_lab,".png")
  ggsave(file_name , plot = last_plot(), width = 6, height = 4,
         path = "results/graphs/cross_country/policy_time_graphs")
}


colnames(policy_graph_dat) <- c("relative_time", "Stay at home",
                                "School closing",
                                "Workplace closing",
                                "Cancel public events",
                                "Testing policies",
                                "Close public transport",
                                "Restrictions on internal movement")
policy_whole_graph_dat <- melt(policy_graph_dat, id = "relative_time", 
                               measure.vars = c("Stay at home",
                                                "School closing",
                                                "Workplace closing",
                                                "Cancel public events",
                                                "Testing policies",
                                                "Close public transport",
                                                "Restrictions on internal movement"),
                               variable_name = "Type_of_policies")
ggplot(data = policy_whole_graph_dat, aes(x = relative_time, y = value,
                                          color = Type_of_policies))+
  geom_point(size=2)+
  geom_line(size=1)+
  ylab("Number of implements")+
  scale_y_continuous(breaks=seq(0,36,6))+
  xlab("Relative time")+
  scale_x_continuous(breaks=seq(0,45,5))+
  guides(color=guide_legend(title = "Types of policies"))+
  theme(axis.text.x = element_text(size = 12,color="black"),
          axis.text.y = element_text(size = 12,color="black"))+
  theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14))

ggsave("policy_time_graph.png" , plot = last_plot(), width = 10, height = 5,
       path = "results/graphs/cross_country/policy_time_graphs")

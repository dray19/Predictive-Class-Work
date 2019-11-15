library(tidyverse)
library(ggplot2)
library(arules)
library(arulesViz)
setwd("~/Desktop")
df <- read.csv('Churn.csv')
df1 <- select(df,VMail.Plan,Int.l.Plan, CustServ.Calls, Churn.)
df1$CustServ.Calls <- factor(df1$CustServ.Calls, order = TRUE, 
                             levels = c(0,1,2,3,4,5,6,7,8,9))
head(df1)
#################################
df2 <- as(df1,'transactions')
summary(df2)
inspect(head(df2))
#############################3
itemFrequencyPlot(df2, topN=10, type="absolute", main="Item Frequency")
itemFrequencyPlot(df2, support = 0.01,main="Item Frequency Plot With support Greater than 1%" )
#################################
frequentItems <- eclat(df2, parameter = list(supp = 0.01, maxlen = 1,target="frequent itemsets"))
inspect(sort(frequentItems, by = 'support'))
################################
rules <- apriori(df2, parameter = list(supp = 0.01,conf = 0.05, maxlen = 2,minlen=2 ,ext = TRUE))
inspect(rules)
#######################################
rules2 <- subset(rules, lhs %in% c('Int.l.Plan=no','VMail.Plan=no','CustServ.Calls=1','VMail.Plan=yes',
                                   'CustServ.Calls=2','CustServ.Calls=0','CustServ.Calls=3', 'Int.l.Plan=yes',
                                   'CustServ.Calls=4','CustServ.Calls=5'))
inspect(rules2)
########################################
rules_conf <- sort(rules2, by="confidence", decreasing=TRUE)
inspect(rules_conf[1:5])
########################################
rules_lift <- sort(rules2, by="lift", decreasing=TRUE) 
inspect(head(rules_lift))
###############################################
plot(rules_lift)
##########################################
#######################################
df = data.frame(
  lhs = labels(lhs(rules_lift)),
  rhs = labels(rhs(rules_lift)), 
  rules_lift@quality)
names(df)[1] <- 'Antecedent'
names(df)[2] <- 'Consequents'
names(df)
df <- mutate(df, Modler_Support = df$support * df$confidence )
df <- mutate(df, Support_percent = df$support * 100)
df <- mutate(df, confidence_percent = df$confidence * 100)
names(df)[5] <- 'Antecedent_Support'
names(df)[7] <- 'Number_of_instances'
df2 <- select(df,Antecedent,Consequents, Number_of_instances,Support_percent,confidence_percent,lift,Modler_Support, Antecedent_Support)
head(df2,1)

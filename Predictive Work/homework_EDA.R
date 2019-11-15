##### Homework One 
library(ggplot2)
library(corrplot)
library(tigerstats)
library(AID)
library(psych)
library(knitr)
library(kableExtra)
library(e1071)
setwd("~/Desktop")
df <- read.csv('churn.txt',  stringsAsFactors = TRUE)
head(df)
##########################
colSums(is.na(df))
### No missing values in the data
###########################
summary(df)
df$Area.Code <- as.factor(df$Area.Code)
##### Basic stats for each variable
##############3
split_data <- function(x, y){
  factor <- list()
  number <- list()
  target <- x[[y]]
    for(var in colnames(x)){
      if (class(x[,var]) %in% c("factor","logical")){
        factor[[var]] <- x[var]
      } else if (class(df[,var]) %in% c("numeric","double","integer")){
        number[[var]]  <- x[var]
      }
    }
  df_factor <- as.data.frame(factor)
  df_number <- as.data.frame(number)
  df_number$Target <- target
  return(list(df_factor, df_number))
}

new_list <- split_data(df, y = 'Churn.')
num <- as.data.frame(new_list[[2]])
fact <- as.data.frame(new_list[[1]])
##################
#####################
stats_list <- list()

for (val in 1:16) {
  column <- colnames(num[val])
  mean = mean(num[[val]])
  median = median(num[[val]])
  sd = sd(num[[val]])
  len = length(num[[val]])
  stats_list[[val]] <- data.frame(Column = column, Mean = mean, Median = median, SD = sd, Length = len)
}
stats_list
stats <- do.call('rbind', stats_list)
stats
des <- describe(num[, !names(num) %in% c("Target")])
des <- select(des, mean, sd, median, skew, kurtosis)
des
###################################################
####################################################################
Outliers_iqr <- c()
for(i in colnames(num[, !names(num) %in% c("Target")])){
  df10 <- select(num, i)
  max <- quantile(df10[,i],0.75, na.rm=TRUE) + (IQR(df10[,i], na.rm=TRUE) * 1.5 )
  min <- quantile(df10[,i],0.25, na.rm=TRUE) - (IQR(df10[,i], na.rm=TRUE) * 1.5 )
  
  idx <- which(df10[,i] < min | df10[,i] > max)
  
  Outliers_iqr[[i]] <- df10[idx,]
}
Outliers_iqr

############################################################################
mean_og <- sapply(num[, !names(num) %in% c("Target")],tapply, INDEX=num$Target, mean)
median_og <- sapply(num[, !names(num) %in% c("Target")] ,tapply, INDEX=num$Target,median)
SD_og <- sapply(num[, !names(num) %in% c("Target")], tapply, INDEX=num$Target,sd)
length_og <- sapply(num[, !names(num) %in% c("Target")], tapply, INDEX=num$Target,length)
stats_churn <- list(Mean = mean_og, Median = median_og, Standard_Deviation = SD_og, Length = length_og)
stats_churn$Mean
stats_churn$Median
stats_churn$Standard_Deviation
#############################
#######################################3
skew <- function(x) {
  skew <- (3*(mean(x) - median(x)))/ sd(x)
  return(skew)
}

###############################
qq_plot <- function(x,y){
  qqnorm(x ,datax = TRUE, col = 'red', main =  paste0(y,' Normal Q-Q Plot'))
  qqline(x , col = 'blue', datax = TRUE)
}
##############################
ggplot(data = num) + geom_histogram(aes(x = Account.Length, fill = Target),colour = 'black') + 
    ggtitle('Account.Length Histogram') + xlab('Account.Length') + theme(plot.title = element_text(hjust = 0.5))
qq_plot(num$Account.Length, y = 'Account.Length')
skew(num$Account.Length)
#################
ggplot(data = num) + geom_histogram(aes(x = VMail.Message, fill = Target),colour = 'black') + 
  ggtitle('VMail.Message Histogram') + xlab('VMail.Message') + theme(plot.title = element_text(hjust = 0.5)) 
qq_plot(num$VMail.Message, y = 'VMail.Message') 
skewness(num$VMail.Message)

sq_vm <- sqrt(num$VMail.Message)
skew(sq_vm)
hist(sq_vm, breaks = 30, col = 'blue', border = 'black', 
      xlab = 'VMail.Message', ylab = 'Counts', main = ' Square Root VMail.Message Histograms')
box(which = 'plot')
#############################3
ggplot(data = num) + geom_histogram(aes(x = Day.Mins, fill = Target),colour = 'black') + 
  ggtitle('Day.Mins Histogram') + xlab('Day.Mins') + theme(plot.title = element_text(hjust = 0.5))
qq_plot(num$Day.Mins, y = 'Day.Mins')
skew(num$Day.Mins)
#######
ggplot(data = num) + geom_histogram(aes(x = Day.Calls, fill = Target),colour = 'black') + 
  ggtitle('Day.Calls Histogram') + xlab('Day.Calls') + theme(plot.title = element_text(hjust = 0.5))
qq_plot(num$Day.Calls, y = 'Day.Calls')
skew(num$Day.Calls)
###########
ggplot(data = num) + geom_histogram(aes(x = Day.Charge , fill = Target),colour = 'black') + 
  ggtitle('Day.Charge  Histogram') + xlab('Day.Charge ') + theme(plot.title = element_text(hjust = 0.5))
qq_plot(num$Day.Charge , y = 'Day.Charge ')
skew(num$Day.Charge)
#########
ggplot(data = num) + geom_histogram(aes(x = Eve.Mins, fill = Target),colour = 'black') + 
  ggtitle('Eve.Mins  Histogram') + xlab('Eve.Mins') + theme(plot.title = element_text(hjust = 0.5))
qq_plot(num$Eve.Mins , y = 'Eve.Mins')
skew(num$Eve.Mins)
###########3
ggplot(data = num) + geom_histogram(aes(x =Eve.Calls , fill = Target),colour = 'black') + 
  ggtitle('Eve.Calls  Histogram') + xlab('Eve.Calls') + theme(plot.title = element_text(hjust = 0.5))
qq_plot(num$Eve.Calls, y = 'Eve.Calls')
skew(num$Eve.Calls)
##########
ggplot(data = num) + geom_histogram(aes(x =Eve.Charge , fill = Target),colour = 'black') + 
  ggtitle('Eve.Charge  Histogram') + xlab('Eve.Charge') + theme(plot.title = element_text(hjust = 0.5))
qq_plot(num$Eve.Charge, y = 'Eve.Charge')
skew(num$Eve.Charge)
###########
ggplot(data = num) + geom_histogram(aes(x =Night.Mins , fill = Target),colour = 'black') + 
  ggtitle('Night.Mins  Histogram') + xlab('Night.Mins') + theme(plot.title = element_text(hjust = 0.5))
qq_plot(num$Night.Mins, y = 'Night.Mins')
skew(num$Night.Mins)
#########3
ggplot(data = num) + geom_histogram(aes(x =Night.Calls , fill = Target),colour = 'black') + 
  ggtitle('Night.Calls Histogram') + xlab('Night.Calls') + theme(plot.title = element_text(hjust = 0.5))
qq_plot(num$Night.Calls, y = 'Night.Calls')
skew(num$Night.Calls)
#########3
ggplot(data = num) + geom_histogram(aes(x =Night.Charge , fill = Target),colour = 'black') + 
  ggtitle('Night.Charge Histogram') + xlab('Night.Charge') + theme(plot.title = element_text(hjust = 0.5))
qq_plot(num$Night.Charge, y = 'Night.Charge')
skew(num$Night.Charge)
#################

ggplot(data = num) + geom_histogram(aes(x =Intl.Mins , fill = Target),colour = 'black') + 
  ggtitle('Intl.Mins Histogram') + xlab('Intl.Mins') + theme(plot.title = element_text(hjust = 0.5))
qq_plot(num$Intl.Mins, y = 'Intl.Mins')
skew(num$Intl.Mins)

###########
ggplot(data = num) + geom_histogram(aes(x =Intl.Calls , fill = Target),colour = 'black') + 
  ggtitle('Intl.Calls Histogram') + xlab('Intl.Calls') + theme(plot.title = element_text(hjust = 0.5))
qq_plot(num$Intl.Calls, y = 'Intl.Calls')
skew(num$Intl.Calls)

sq_im <- sqrt(num$Intl.Calls)
skew(sq_im)
hist(sq_im, breaks = 30, col = 'blue', border = 'black',xlab = 'Intl.Calls', ylab = 'Counts', main = ' Square Root Histograms')
box(which = 'plot')
qq_plot(sq_im, y = ' Square Root Intl.Calls')
##########
ggplot(data = num) + geom_histogram(aes(x =Intl.Charge , fill = Target),colour = 'black') + 
  ggtitle('Intl.Charge Histogram') + xlab('Intl.Charge') + theme(plot.title = element_text(hjust = 0.5))
qq_plot(num$Intl.Charge, y = 'Intl.Charge')
skew(num$Intl.Charge)
##########
ggplot(data = num) + geom_histogram(aes(x =CustServ.Calls , fill = Target),colour = 'black') + 
  ggtitle('CustServ.Calls Histogram') + xlab('CustServ.Calls') + theme(plot.title = element_text(hjust = 0.5))
qq_plot(num$CustServ.Calls, y = 'CustServ.Calls')
skew(num$CustServ.Calls)

sq_csc <- sqrt(num$CustServ.Calls)
skew(sq_csc)
hist(sq_csc, breaks = 30, col = 'blue', border = 'black',xlab = 'Intl.Calls', ylab = 'Counts', main = ' Square Root Histograms')
box(which = 'plot')
qq_plot(sq_csc, y = ' Square Root CustServ.Calls')
######################################################
corr1 <- cor(num[, !names(num) %in% c("Target")])
corrplot(corr1, method="number", type="upper")
#######################
ggplot(data = num) + geom_point(aes(x =Day.Mins, y = Day.Charge , col = Target)) + 
  ggtitle('Days Mins vs. Day Charge') + xlab('Day Mins') + ylab('Day Charge') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = num) + geom_point(aes(x =Eve.Mins, y = Eve.Charge , col = Target)) + 
  ggtitle('Eve Mins vs. Eve Charge') + xlab('Eve Mins') + ylab('Eve Charge') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = num) + geom_point(aes(x =Night.Mins, y = Night.Charge , col = Target)) + 
  ggtitle('Night Mins vs. Night Charge') + xlab('Night Mins') + ylab('Night Charge') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = num) + geom_point(aes(x =Intl.Mins, y = Intl.Charge , col = Target)) + 
  ggtitle('Intl Mins vs. Intl Charge') + xlab('Intl Mins') + ylab('Intl Charge') +
  theme(plot.title = element_text(hjust = 0.5))
###############################################
xtabs(~ State + Churn., data = fact)
round(prop.table(xtabs(~ State + Churn., data = fact)),3)
rowPerc(xtabs(~ State + Churn., data = fact))
##############
xtabs(~ Area.Code + Churn., data = fact)
round(prop.table(xtabs(~ Area.Code + Churn., data = fact)),3)
rowPerc(xtabs(~ Area.Code + Churn., data = fact))
############
xtabs(~ Int.l.Plan + Churn., data = fact)
round(prop.table(xtabs(~ Int.l.Plan + Churn., data = fact)),3)
rowPerc(xtabs(~ Int.l.Plan + Churn., data = fact))
############
xtabs(~ VMail.Plan + Churn., data = fact)
round(prop.table(xtabs(~ VMail.Plan + Churn., data = fact)),3)
rowPerc(xtabs(~ VMail.Plan + Churn., data = fact))
###########################################################################
ggplot(data = fact) + geom_bar(aes(x = State, fill = Churn.), position = "fill") + 
 ggtitle('Bar Chart for State') + xlab('State') + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = fact) + geom_bar(aes(x = Area.Code, fill = Churn.), position = "dodge", colour ='black') + 
  ggtitle('Barchart for Area.Code') + xlab('Area.Code') + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = fact) + geom_bar(aes(x = Area.Code, fill = Churn.), position = "fill", colour ='black') + 
  ggtitle('Barchart for Area.Code') + xlab('Area.Code') + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = fact) + geom_bar(aes(x = Int.l.Plan, fill = Churn.), position = "fill",colour ='black') + 
  ggtitle('Barchart for Int.l.Plan') + xlab('Int.l.Plan') + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = fact) + geom_bar(aes(x = Int.l.Plan, fill = Churn.),position = "dodge", colour ='black') + 
  ggtitle('Barchart for Int.l.Plan') + xlab('Int.l.Plan') + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = fact) + geom_bar(aes(x = VMail.Plan, fill = Churn.), position = "fill", colour ='black') + 
  ggtitle('Barchart for VMail.Plan') + xlab('VMail.Plan') + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = fact) + geom_bar(aes(x = VMail.Plan, fill = Churn.), position = "dodge", colour ='black') + 
  ggtitle('Barchart for VMail.Plan') + xlab('VMail.Plan') + theme(plot.title = element_text(hjust = 0.5))



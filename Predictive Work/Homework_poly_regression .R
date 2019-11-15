library(tidyverse)
library(ggplot2)
library(gam)
#################################3
setwd("~/Desktop")
df <- read.csv('Position_Salary.csv')
df1 <- df
head(df1)
cor(df$Level, df$Salary)

ggplot(df, aes(x = Level, y =Salary)) + geom_point() +
  labs(title = "Level vs. Salary") +  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 14)) + 
  scale_x_continuous(limits = c(1,10), breaks = c(1,2,3,4,5,6,7,8,9,10)) + 
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))
 
######################################

model <- lm(Salary ~ Level, data = df)
coef(model)
data.frame(Adj_R2 = summary(model)$adj.r.squared, R2=summary(model)$r.squared)

df1$pred = predict(model, newdata = df)
ggplot(df1, aes(x = Level, y =Salary)) + geom_point() + 
  geom_line(aes(x = Level, y = pred)) + labs(title = "Linear Model ") +  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 14)) + 
  scale_x_continuous(limits = c(1,10), breaks = c(1,2,3,4,5,6,7,8,9,10)) + 
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))
pred.dat=data.frame(Level = 5)
round(predict(model, newdata = pred.dat),0)

####################
model1  <- lm(Salary ~ poly(Level,2), data = df)
coef(model1)
data.frame(Adj_R2 = summary(model1)$adj.r.squared, R2=summary(model1)$r.squared)

df1$pred_2 = predict(model1, newdata = df)
ggplot(df1, aes(x = Level, y =Salary)) + geom_point() + 
  geom_line(aes(x = Level, y = pred_2)) + labs(title = "2 Degree Polynomial ") +  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 14)) + 
  scale_x_continuous(limits = c(1,10), breaks = c(1,2,3,4,5,6,7,8,9,10)) + 
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))
pred.dat=data.frame(Level = 5)
round(predict(model1, newdata = pred.dat),0)


model3  <- lm(Salary ~ poly(Level,3), data = df)
coef(model3)
data.frame(Adj_R2 = summary(model3)$adj.r.squared, R2=summary(model3)$r.squared)

df1$pred_3 = predict(model3, newdata = df)
ggplot(df1, aes(x = Level, y =Salary)) + geom_point() + 
  geom_line(aes(x = Level, y = pred_3)) + labs(title = "3 Degree Polynomial ") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 14)) + 
  scale_x_continuous(limits = c(1,10), breaks = c(1,2,3,4,5,6,7,8,9,10)) + 
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

round(predict(model3, newdata = pred.dat),0)


model4  <- lm(Salary ~ poly(Level,4), data = df)
data.frame(Adj_R2 = summary(model4)$adj.r.squared, R2=summary(model4)$r.squared)
coef(model4)

df1$pred_4 = predict(model4 ,newdata = df)

ggplot(df1, aes(x = Level, y =Salary)) + geom_point() + 
  geom_line(aes(x = Level, y = pred_4)) + labs(title = "4 Degree Polynomial ") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 14)) + 
  scale_x_continuous(limits = c(1,10), breaks = c(1,2,3,4,5,6,7,8,9,10)) + 
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

round(predict(model4, newdata = pred.dat),0)

#####################
anova(model3, model4)
########################### Spline #########################3

knots <- quantile(df$Level, p = c(0.25, 0.5, 0.75))
knots 
spline1  <- lm(Salary ~ ns(Level, knots = knots), data = df)
data.frame(Adj_R2 = summary(spline1)$adj.r.squared, R2=summary(spline1)$r.squared)
coef(spline1)
df1$spline_pred_3 = predict(spline1 ,newdata = df)
ggplot(df1, aes(x = Level, y =Salary)) + geom_point() + 
  geom_line(aes(x = Level, y = spline_pred_3)) + labs(title = "Natural Spline") + 
  geom_vline(xintercept = knots, , linetype='dashed', col = 'darkgreen') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 14)) + 
  scale_x_continuous(limits = c(1,10), breaks = c(1,2,3,4,5,6,7,8,9,10)) + 
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

round(predict(spline1 , newdata = pred.dat),0)


spline2  <- lm(Salary ~ bs(Level ,knots = knots), data = df)
data.frame(Adj_R2 = summary(spline2)$adj.r.squared, R2=summary(spline2)$r.squared)
coef(spline2)
df1$spline_pred_4 = predict(spline2 ,newdata = df)
ggplot(df1, aes(x = Level, y =Salary)) + geom_point() + 
  geom_line(aes(x = Level, y = spline_pred_4)) + labs(title = "B-Spline") + 
  geom_vline(xintercept = knots, , linetype='dashed', col = 'darkgreen') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 14)) + 
  scale_x_continuous(limits = c(1,10), breaks = c(1,2,3,4,5,6,7,8,9,10)) + 
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

round(predict(spline2 , newdata = pred.dat),0)


################### Splines smooth #############################3
plotfn = function(spar){
  plot(df$Level,df$Salary,main=spar)
  lines(smooth.spline(df$Level,df$Salary,spar=spar),col='red')
}
spars = seq(.1,1,by=.1)
par(mfrow=c(5,2),mar=c(2,4,2,1)+.1)
sapply(spars,plotfn)
smooth1<-smooth.spline(df$Salary,df$Level, spar = 0.4)
smooth1
predict(smooth1, df)
############################## GAM ################################
library(compositions)
gam1 <- gam(Salary ~  s(Level,5), data = df)
coef(gam1)

data.frame(Adj_R2 = 'NA',R2= R2(gam1))
df1$gam_pred = predict(gam1 ,newdata = df)

ggplot(df1, aes(x = Level, y =Salary)) + geom_point() + 
  geom_line(aes(x = Level, y = gam_pred)) + labs(title = "Generalised Additive Models") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 14)) + 
  scale_x_continuous(limits = c(1,10), breaks = c(1,2,3,4,5,6,7,8,9,10)) + 
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))

round(predict(gam1 , newdata = pred.dat),0)
#######################


df10 <- data.frame(Model = 'Poly Degree = 2',
                   Adj_R2 = summary(model1)$adj.r.squared, R2=summary(model1)$r.squared,
                   Prediction = round(predict(model1 , newdata = pred.dat),0))
df11 <- data.frame(Model = 'Poly Degree = 3',
                   Adj_R2 = summary(model3)$adj.r.squared, R2=summary(model3)$r.squared,
                   Prediction = round(predict(model3 , newdata = pred.dat),0))
all_models <- rbind(df10,df11)
df12 <- data.frame(Model = 'Poly Degree = 4',
                   Adj_R2 = summary(model4)$adj.r.squared, R2=summary(model4)$r.squared,
                   Prediction = round(predict(model4 , newdata = pred.dat),0))
all_models <- rbind(all_models,df12)
df13 <- data.frame(Model = 'Natural Spline',
                   Adj_R2 = summary(spline1)$adj.r.squared, R2=summary(spline1)$r.squared,
                   Prediction = round(predict(spline1 , newdata = pred.dat),0))
all_models <- rbind(all_models,df13)
df14 <- data.frame(Model = 'B Spline',
                   Adj_R2 = summary(spline2)$adj.r.squared, R2=summary(spline2)$r.squared,
                   Prediction = round(predict(spline2 , newdata = pred.dat),0))
all_models <- rbind(all_models,df14)
df15 <- data.frame(Model = 'GAM',
                   Adj_R2 = 'NA',R2= R2(gam1),
                   Prediction = round(predict(gam1 , newdata = pred.dat),0))
all_models <- rbind(all_models,df15)
all_models
  


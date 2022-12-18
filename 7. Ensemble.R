rm(list=ls())
data_df = read.csv("Dataset_1step.csv")

data_df1 = data_df[,-1]

tr = 1:578
train = data_df1[tr,]   
test = data_df1[-tr,]

library(glmnet)
MSE <- function(pred, truth){ 
  return(mean((truth - pred)^2)) 
}

oosy = test$INDPRO

#Simple average ensemble

predict_df = read.csv("Predicted values.csv")

predict_1s_df = predict_df[,c(1:6)]
View(predict_1s_df)
avgpred1 = (predict_1s_df[,3] + predict_1s_df[,2])/2
avgmse1 = MSE(avgpred1,oosy) #0.613949

#date = as.Date(data_df$sasdate[579:723],format = "%d/%m/%Y")
date=c(1:50)
xticks = c('01/2016','11/2016','09/2017','07/2018','5/2019','3/2020')
predict_1s_df2=cbind(predict_1s_df1[c(1:100),],date,oosy=oosy[1:100])
library(ggplot2)

predict_1s_df1 = cbind(predict_1s_df[c(96:145),],date,oosy=oosy[96:145],avgpred1=avgpred1[96:145])

ggplot(predict_1s_df1,aes(x=date)) +
  geom_line(aes(y=Lasso..aic.bic..1,color='Lasso')) +
  geom_line(aes(y=oosy,color='Actual')) +
  geom_line(aes(y=RW.1,color='Random Walk')) +
  geom_line(aes(y=AR.12..1,color='AR(12)')) +
  geom_line(aes(y=RF.1,color='RF')) +
  scale_color_manual(values=c("Lasso"="blue","Actual"="black","Random Walk"='red',"AR(12)"='green','RF'='skyblue'))+
  labs(x='Time',y='Predicted',title='1-Step Forecasts') +
  scale_x_continuous(breaks = c(1,10,20,30,40,50)
                     , labels = xticks)

predict_3s_df = predict_df[,c(7:11)]
View(predict_3s_df)
avgpred3 = (predict_3s_df[,1] + predict_3s_df[,2])/2
avgmse3 = MSE(avgpred3,oosy) #1.923141

predict_3s_df1 = cbind(predict_3s_df[c(96:145),],date,oosy=oosy[96:145],avgpred3=avgpred3[96:145])

ggplot(predict_3s_df1,aes(x=date)) +
  geom_line(aes(y=Lasso..aic.bic..3,color='Lasso')) +
  geom_line(aes(y=oosy,color='Actual')) +
  geom_line(aes(y=RW.3,color='Random Walk')) +
  geom_line(aes(y=AR.12..3,color='AR(12)')) +
  geom_line(aes(y=RF.3,color='RF')) +
  scale_color_manual(values=c("Lasso"="blue","Actual"="black","Random Walk"='red',"AR(12)"='green','RF'='skyblue'))+
  labs(x='Time',y='Predicted',title='3-Step Forecasts') +
  scale_x_continuous(breaks = c(1,10,20,30,40,50)
                     , labels = xticks)


predict_6s_df = predict_df[,c(12:17,23)]
View(predict_6s_df)
avgpred6 = (predict_6s_df[,2] + predict_6s_df[,7])/2
avgmse6 = MSE(avgpred6,oosy) #5.74955

predict_6s_df1 = cbind(predict_6s_df[c(96:145),],date,oosy=oosy[96:145],avgpred6=avgpred6[96:145])

ggplot(predict_6s_df1,aes(x=date)) +
  geom_line(aes(y=oosy,color='Actual')) +
  geom_line(aes(y=RW.6,color='Random Walk')) +
  geom_line(aes(y=AR.12..6,color='AR(12)')) +
  geom_line(aes(y=avgpred6,color='Ensemble')) +
  scale_color_manual(values=c("Actual"="black","Random Walk"='red',"AR(12)"='green','Ensemble'='blue'))+
  labs(x='Time',y='Predicted',title='6-Step Forecasts') +
  scale_x_continuous(breaks = c(1,10,20,30,40,50)
                     , labels = xticks)

predict_12s_df = predict_df[,c(18:22)]
View(predict_12s_df)
avgpred12 = (predict_12s_df[,1] + predict_12s_df[,2])/2
avgmse12 = MSE(avgpred12,oosy) #18.71736

predict_12s_df1 = cbind(predict_12s_df[c(96:145),],date,oosy=oosy[96:145],avgpred12=avgpred12[96:145])

ggplot(predict_12s_df1,aes(x=date)) +
  geom_line(aes(y=oosy,color='Actual')) +
  geom_line(aes(y=RW.12,color='Random Walk')) +
  geom_line(aes(y=AR.12..12,color='AR(12)')) +
  geom_line(aes(y=avgpred12,color='Ensemble')) +
  scale_color_manual(values=c("Actual"="black","Random Walk"='red',"AR(12)"='green','Ensemble'='blue'))+
  labs(x='Time',y='Predicted',title='12-Step Forecasts') +
  scale_x_continuous(breaks = c(1,10,20,30,40,50)
                     , labels = xticks)

simple_average = c(0.7895653,2.402582,7.577207,19.87514)
results_df = data.frame(simple_average)
rownames(results_df) = c('1-step','3-step','6-step','12-step')
View(results_df)

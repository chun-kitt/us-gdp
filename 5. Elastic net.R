setwd("C:\\Users\\User\\Desktop\\Lecture notes\\AY202021 Sem 1\\EC4308\\Project")
rm(list = ls())

install.packages("hdm")
install.packages("glmnet")
install.packages("tidyverse")

library(hdm)
library(glmnet)
library(tidyverse)

#Ridge regression and Elastic net

db = read.csv(file = 'C:\\Users\\User\\Desktop\\Lecture notes\\AY202021 Sem 1\\EC4308\\Project\\Dataset_1step.csv') #1step forecast
db = read.csv(file = 'C:\\Users\\User\\Desktop\\Lecture notes\\AY202021 Sem 1\\EC4308\\Project\\Dataset_3step.csv') #3step forecast
db = read.csv(file = 'C:\\Users\\User\\Desktop\\Lecture notes\\AY202021 Sem 1\\EC4308\\Project\\Dataset_6step.csv') #6step forecast
db = read.csv(file = 'C:\\Users\\User\\Desktop\\Lecture notes\\AY202021 Sem 1\\EC4308\\Project\\Dataset_12step.csv') #12step forecast

#run this line for 3,6,12 step forecasts
db = db %>% select(-X)

MSE <- function(pred, truth){  
  return(mean((truth - pred)^2))
}

#run this line for 1 step forecast
#x = model.matrix(INDPRO~. -sasdate, data = db)

#run this line instead for 3,6,12 step forecasts
x = model.matrix(INDPRO~., data = db)

ncol(x)

x = x[,2:1405] #omit the intercept

y = db$INDPRO

#test set last 145 obs, train set use the rest

ntrain = nrow(db) - 145 #training sample size

set.seed(9386)
train = 1:ntrain  # sample ntrain indices of observations
test = (-train)  # testing sample indices

grid = 10^seq(0, -3, length = 100)

#set aa to 0.5 for elastic net
aa = 0.5

#Running the ridge model first to get the set of lambdas
ridge.mod <- glmnet(x[train,],
                    y[train],
                    lambda = grid,
                    alpha = aa)

#assigning lambdas and creating vectors to store the values of aic/bic/aicc
lambda.ridge = ridge.mod$lambda
aic = c()
bic = c()
aicc = c() 

#for loop to iterate over all the lambdas and obtain the different measures 
for (i in seq(lambda.ridge)) {
  model <- glmnet(x[train,], y[train], alpha = aa, lambda = lambda.ridge[i])
  betas = as.vector((as.matrix(coef(model))[-1,]))
  resid = db$INDPRO - (x %*% betas)
  resid.var = (t(resid) %*% resid)/length(train)
  z = length(betas[betas != 0])
  aic[i] = log(resid.var) + 2*z/length(train)
  bic[i] = log(resid.var) + log(length(train))*z/length(train)
  aicc[i] = log(resid.var) + 2*(z+1)/(length(train)-z-2)
}

which.min(aic)
which.min(bic)
which.min(aicc)

bestlam_aic = lambda.ridge[which.min(aic)]
bestlam_aic
bestlam_bic = lambda.ridge[which.min(bic)]
bestlam_bic
bestlam_aicc = lambda.ridge[which.min(aicc)]
bestlam_aicc

ridge.pred <- predict(ridge.mod, s = bestlam_aic, newx = x[test,])
ridge.pred <- predict(ridge.mod, s = bestlam_bic, newx = x[test,])
ridge.pred <- predict(ridge.mod, s = bestlam_aicc, newx = x[test,])

MSE(ridge.pred, y[test])

values = predict(ridge.mod,
                 s = bestlam_bic, #change s here to whichever bestlam
                 newx = x[test,])
values
values.df = as.data.frame(values)
write.csv(values.df,
          "C:\\Users\\User\\Desktop\\Lecture notes\\AY202021 Sem 1\\EC4308\\Project\\6stepENdefaultvalidAICBICAICc.csv",
          row.names = TRUE)

ridge.coefs = as.matrix(predict(ridge.mod,
                        s = bestlam_aic,
                        type = "coef"))

ridge.coefs1=as.data.frame(ridge.coefs[ridge.coefs[,1] != 0,])
ridge.coefs1

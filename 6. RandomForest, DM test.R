rm(list = ls())

library(randomForest)
library(caret)
library(sandwich)

MSE <- function(pred, truth){ 
  return(mean((truth - pred)^2))
}

data = read.csv("Dataset_1step.csv", header = TRUE, fileEncoding = "UTF-8")
data = data[,-c(1)]
ptrain = data[c(1:578),]
ptest = data[c(579:723),]
nprev = 145 #test obs

runrf=function(Y,indice,lag){
  xx = Y[,-c(1)]
  X.out = tail(xx, 1)
  model=randomForest(INDPRO~.,data=Y,ntree=500,mtry=468,importance=TRUE)
  pred=predict(model,c(X.out,0)) #generate forecast
  
  return(list("model"=model,"pred"=pred)) #return the estimated model and h-step forecast
}


rf.rolling.window=function(Y,nprev,indice=1,lag){
  save.importance=list() #blank for saving variable importance
  save.pred=matrix(NA,nprev,1) ##blank for forecasts
  for(i in nprev:1){#NB: backwards FOR loop: going from 145 down to 1
    Y.window=Y[(1+nprev-i):(nrow(Y)-i-lag+1),] #define the estimation window (first one: 1 to 578, then 2 to 579 etc.)
    lasso=runrf(Y.window,indice,lag)#call the function to fit the Random Forest and generate h-step forecast
    save.pred[(1+nprev-i),]=lasso$pred #save the forecast
    save.importance[[i]]=importance(lasso$model) #save variable importance
    cat("window from",(1+nprev-i), "to", (nrow(Y)-i-lag+1),"\n")
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  #Some helpful stuff:
  real=Y[,indice]#get actual values
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2)) #compute RMSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("rmse"=rmse,"mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"errors"=errors,"save.importance"=save.importance)) #return forecasts, history of variable importance, and RMSE and MAE for the period.
}

rf1step = rf.rolling.window(data,nprev,1,1) #1 step
rf3steps = rf.rolling.window(data,nprev,1,3) #3 steps
rf6steps = rf.rolling.window(data,nprev,1,6) # 6 steps
rf12steps=rf.rolling.window(data,nprev,1,12) #12 steps

#####DM Test#####
final_predicted_values = read.csv("Predicted values.csv", header = TRUE, fileEncoding = "UTF-8")
oosy = ptest$INDPRO

######Compute squared loss for different horizons (AR)#####
loss_ar_1step=(oosy-final_predicted_values$AR.12..1)^2
loss_ar_3step=(oosy-final_predicted_values$AR.12..3)^2
loss_ar_6step=(oosy-final_predicted_values$AR.12..6)^2
loss_ar_12step=(oosy-final_predicted_values$AR.12..12)^2

#####Squared loss for different horizons(Ensemble)#####
loss_simple_1step = (oosy-final_predicted_values$Ensemble.1)^2
loss_simple_3step = (oosy-final_predicted_values$Ensemble.3)^2
loss_simple_6step = (oosy-final_predicted_values$Ensemble.6)^2
loss_simple_12step = (oosy-final_predicted_values$Ensemble.12)^2

#####loss differential between AR and Ensemble#####
loss_diff_ar_simple_1 = loss_ar_1step - loss_simple_1step
loss_diff_ar_simple_3 = loss_ar_3step - loss_simple_3step
loss_diff_ar_simple_6 = loss_ar_6step - loss_simple_6step
loss_diff_ar_simple_12 = loss_ar_12step - loss_simple_12step

#####Create TS object#####
dtarsimple.ts=ts(cbind(loss_diff_ar_simple_1,loss_diff_ar_simple_3,loss_diff_ar_simple_6,loss_diff_ar_simple_12), start=c(2008,2), end=c(2020,3), freq=12)
colnames(dtarsimple.ts)=c("1-step dt","3-step dt","6-step dt","12-step dt")
#Plot them to examine stationarity:
plot.ts(dtarsimple.ts, main="Loss differential AR-ENSEMBLE",cex.axis=1.2)

#####DM T-STAT #####
dmarensemble1=lm(loss_diff_ar_simple_1~1)#regression
acf(dmarensemble1$residuals) #check serial correlation of residuals - number of significant autocorrelations is a good guess for number lags included in the HAC variance estimator
dmarensemble1$coefficients/sqrt(NeweyWest(dmarensemble1,lag=5)) #form the DM t-statistic -1.0995

dmarensemble3=lm(loss_diff_ar_simple_3~1)#regression
acf(dmarensemble3$residuals) #check serial correlation of residuals - number of significant autocorrelations is a good guess for number lags included in the HAC variance estimator
dmarensemble1$coefficients/sqrt(NeweyWest(dmarensemble3,lag=5)) #form the DM t-statistic -0.2065

dmarensemble6=lm(loss_diff_ar_simple_6~1)#regression
acf(dmarensemble6$residuals) #check serial correlation of residuals - number of significant autocorrelations is a good guess for number lags included in the HAC variance estimator
dmarensemble6$coefficients/sqrt(NeweyWest(dmarensemble6,lag=5)) #form the DM t-statistic 0.5875

dmarensemble12=lm(loss_diff_ar_simple_12~1)#regression
acf(dmarensemble12$residuals) #check serial correlation of residuals - number of significant autocorrelations is a good guess for number lags included in the HAC variance estimator
dmarensemble12$coefficients/sqrt(NeweyWest(dmarensemble12,lag=5)) #form the DM t-statistic 1.0457


#####Compute squared loss for different horizons (LASSO)#####
loss_lasso_1step=(oosy-final_predicted_values$Lasso..aic.bic..1)^2 
loss_lasso_3step=(oosy-final_predicted_values$Lasso..aic.bic..3)^2 
loss_lasso_6step=(oosy-final_predicted_values$Lasso..aic.bic.aicc..6)^2 
loss_lasso_12step=(oosy-final_predicted_values$Lasso..aic.bic..12)^2 

#####AR-LASSO#####
#####Compute loss differentials (d_t) for different horizons (AR-LASSO)#####
loss_diff_ar_lasso_1= loss_ar_1step - loss_lasso_1step
loss_diff_ar_lasso_3= loss_ar_3step - loss_lasso_3step
loss_diff_ar_lasso_6= loss_ar_6step - loss_lasso_6step
loss_diff_ar_lasso_12= loss_ar_12step - loss_lasso_12step

dtarlasso.ts=ts(cbind(loss_diff_ar_lasso_1,loss_diff_ar_lasso_3,loss_diff_ar_lasso_6,loss_diff_ar_lasso_12), start=c(2008,2), end=c(2020,3), freq=12)
colnames(dtarlasso.ts)=c("1-step dt","3-step dt","6-step dt","12-step dt")
#Plot them to examine stationarity:
plot.ts(dtarlasso.ts, main="Loss differential AR-LASSO",cex.axis=1.2)

#1-step forecast test
dmarlasso1=lm(loss_diff_ar_lasso_1~1)
acf(dmarlasso1$residuals)
dmarlasso1$coefficients/sqrt(NeweyWest(dmarlasso1,lag=5)) #-1.925986

#3-step forecast test
dmarlasso3=lm(loss_diff_ar_lasso_3~1)
acf(dmarlasso3$residuals)
dmarlasso3$coefficients/sqrt(NeweyWest(dmarlasso3,lag=5)) #-1.689001

#6-step forecast test
dmarlasso6=lm(loss_diff_ar_lasso_6~1)
acf(dmarlasso6$residuals)
dmarlasso6$coefficients/sqrt(NeweyWest(dmarlasso6,lag=5)) #-1.90147

#12-step forecast test
dmarlasso12=lm(loss_diff_ar_lasso_12~1)
acf(dmarlasso12$residuals)
dmarlasso12$coefficients/sqrt(NeweyWest(dmarlasso12,lag=5)) # 0.3400227

loss_diff_ensem_lasso_1= loss_simple_1step - loss_lasso_1step
loss_diff_ensem_lasso_3= loss_simple_3step - loss_lasso_3step
loss_diff_ensem_lasso_6= loss_simple_6step - loss_lasso_6step
loss_diff_ensem_lasso_12= loss_simple_12step - loss_lasso_12step

dtensemlaso.ts=ts(cbind(loss_diff_ensem_lasso_1,loss_diff_ensem_lasso_3,loss_diff_ensem_lasso_6,loss_diff_ensem_lasso_12), start=c(2008,2), end=c(2020,3), freq=12)
colnames(dtensemlaso.ts)=c("1-step dt","3-step dt","6-step dt","12-step dt")
#Plot them to examine stationarity:
plot.ts(dtensemlaso.ts, main="Loss differential ENSEMBLE-LASSO",cex.axis=1.2)

#####ENSEMBLE-LASSO######
#1-step forecast test
dmenlasso1=lm(loss_diff_ensem_lasso_1~1)
acf(dmenlasso1$residuals)
dmenlasso1$coefficients/sqrt(NeweyWest(dmenlasso1,lag=5)) 

#3-step forecast test
dmenlasso3=lm(loss_diff_ensem_lasso_3~1)
acf(dmenlasso1$residuals)
dmenlasso3$coefficients/sqrt(NeweyWest(dmenlasso3,lag=5)) 

#6-step forecast test
dmenlasso6=lm(loss_diff_ensem_lasso_6~1)
acf(dmenlasso6$residuals)
dmenlasso6$coefficients/sqrt(NeweyWest(dmenlasso6,lag=5)) 

#12-step forecast test
dmenlasso12=lm(loss_diff_ensem_lasso_12~1)
acf(dmenlasso12$residuals)
dmenlasso12$coefficients/sqrt(NeweyWest(dmenlasso12,lag=5)) 
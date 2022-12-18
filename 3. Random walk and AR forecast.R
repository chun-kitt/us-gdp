rm(list=ls())

#set working directory

#################################
#Benchmark model: Random Walk
#################################
#Auxiliary function to compute root MSE (same as MSE before, but with square root):
MSE <- function(pred, truth){ #start and end body of the function by { } - same as a loop 
  return(mean((truth - pred)^2))
} 
RMSE <- function(pred, truth){ #start and end body of the function by { } - same as a loop 
  return(sqrt(mean((truth - pred)^2)))
}
df = read.csv('Dataset_1step.csv')
Y=df[,-1] #drop dates in first column

yy=Y[,1] #get the y variable - INDPRO
  
nprev=145 #number of out-of-sample observations (test window )
  
oosy=tail(yy,nprev) #auxiliary:get the out-of-sample true values (last 145 obs. using tail())
  
#create 12 lags
rwtemp=embed(yy,13)

#Simple RW forecast:
rw1c=tail(rwtemp[,2],nprev)
rw3c=tail(rwtemp[,4],nprev)
rw6c=tail(rwtemp[,7],nprev)
rw12c=tail(rwtemp[,13],nprev)

#Collect MSE's for random walk:
rw.mse1=MSE(oosy,rw1c) #0.6892966
rw.mse3=MSE(oosy,rw3c) #2.653759
rw.mse6=MSE(oosy,rw6c) #8.192238
rw.mse12=MSE(oosy,rw12c) #23.85235

#Combine results into a dataframe
results.rw <- data.frame(rw1c, rw3c, rw6c, rw12c)

#Export results into csv 

###################################
#Benchmark 2: AR(p) forecast
###################################

#Perform AR(p) forecasts using rolling window.
#See the file func-ar.R for the forecast construction details there

#Add the functions  in func-ar(bic).R (must be in your working directory)
#Or simply open up func-ar(bic).R and execute the function commands there
#setwd("C:\\Users\\ck_00\\OneDrive\\Desktop\\EC4308\\R Materials\\EC4308_Lecture5_time_series")

source("func-AR.R")

#AR forecast chosen on BIC
bar1c=ar.rolling.window(Y,nprev,1,1,type="bic") #1-step AR forecast
bar3c=ar.rolling.window(Y,nprev,1,3,type="bic") #3-step AR forecast
bar6c=ar.rolling.window(Y,nprev,1,6,type="bic") #6-step AR forecast
bar12c=ar.rolling.window(Y,nprev,1,12,type="bic") #12-step AR forecast

#AR forecasts RMSE:
ar.mse1=bar1c$errors[1] #0.5911804 
ar.mse3=bar3c$errors[1] #1.853526 
ar.mse6=bar6c$errors[1] #6.063372 
ar.mse12=bar12c$errors[1] #21.22212 

#Combine results into dataframe
results.ar <- data.frame(bar1c[1], bar3c[1], bar6c[1], bar12c[1])

#Export results into csv                                   
#write.csv(results.ar,"C:\\Users\\ck_00\\OneDrive\\Desktop\\EC4308\\Project\\results_ar.csv")

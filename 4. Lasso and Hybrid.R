rm(list=ls())
data_df = read.csv("Dataset_1step.csv")
#data_df = read.csv("Dataset_3step.csv")
#data_df = read.csv("Dataset_6step.csv")
#data_df = read.csv("Dataset_12step.csv")

data_df1 = data_df[,-1]

tr = 1:578
train = data_df1[tr,] 
test = data_df1[-tr,]

library(glmnet)
MSE <- function(pred, truth){ 
  return(mean((truth - pred)^2)) 
}

train_mat = as.matrix(train[,-1])
test_mat = as.matrix(test[,-1])

#Lasso with default grid
lasso.mod <- glmnet(train_mat, train$INDPRO, alpha = 1)

lambdas_to_try <- lasso.mod$lambda
aic <- c()
bic <- c()
aicc = c()
for (lambda in seq(lambdas_to_try)) {
  model <- glmnet(train_mat, train$INDPRO, alpha = 1, lambda = lambdas_to_try[lambda])
  betas <- as.vector((as.matrix(coef(model))[-1, ]))
  resid <- train$INDPRO - (train_mat %*% betas)
  resid_var = (t(resid) %*% resid )/nrow(train)
  df = length(betas[betas!=0])
  aic[lambda] <- log(resid_var) + 2*df/nrow(train)
  bic[lambda] <- log(resid_var) + log(nrow(train))*df/nrow(train)
  aicc[lambda] <- log(resid_var) + 2*(df+1)/(nrow(train)-df-2)
}
which.min(aic) #86
which.min(bic) #86
which.min(aicc) #86

bestlam_d = lambdas_to_try[86]
bestlam_d #0.260564

lasso_d.pred <- predict(lasso.mod, s = bestlam_d, newx = test_mat)
lasso_d = as.matrix(predict(lasso.mod, s = bestlam_d, type = "coef"))
lasso_d.coef = as.data.frame(lasso_d[lasso_d[,1] != 0,])
MSE(lasso_d.pred, test$INDPRO) #2.717698

#Lasso with user-defined grid
grid = 10^seq(0, -3, length = 100)

lasso.mod <- glmnet(train_mat, train$INDPRO, alpha = 1,lambda=grid)

lambdas_to_try <- lasso.mod$lambda
aic <- c()
bic <- c()
aicc = c()
for (lambda in seq(lambdas_to_try)) {
  model <- glmnet(train_mat, train$INDPRO, alpha = 1, lambda = lambdas_to_try[lambda])
  betas <- as.vector((as.matrix(coef(model))[-1, ]))
  resid <- train$INDPRO - (train_mat %*% betas)
  resid_var = (t(resid) %*% resid )/nrow(train)
  df = length(betas[betas!=0])
  aic[lambda] <- log(resid_var) + 2*df/nrow(train)
  bic[lambda] <- log(resid_var) + log(nrow(train))*df/nrow(train)
  aicc[lambda] <- log(resid_var) + 2*(df+1)/(nrow(train)-df-2)
}
which.min(aic) #28
which.min(bic) #28
which.min(aicc) #88

bestlam_u = lambdas_to_try[28] #aic/bic
bestlam_u #0.1519911

lasso_u.pred <- predict(lasso.mod, s = bestlam_u, newx = test_mat)
#write.csv(lasso_u.pred_cv,'C:\\Users\\JC\\Desktop\\20-21 Sem 1\\EC4308\\Project\\lasso_aic_bic_1step_CV.csv')
lasso_u = as.matrix(predict(lasso.mod, s = bestlam_u, type = "coef"))
lasso_u.coef = as.data.frame(lasso_u[lasso_u[,1] != 0,])
MSE(lasso_u.pred, test$INDPRO) #0.6998594

#hybrid method
library(hdm)
library(tictoc)
library(randomForest)
rlasso.fit = rlasso(train$INDPRO~train_mat,  post=FALSE)
yhat.rlasso<- predict(rlasso.fit, newdata=test_mat)
write.csv(yhat.rlasso_cv,'C:\\Users\\JC\\Desktop\\20-21 Sem 1\\EC4308\\Project\\rlasso_1step_CV.csv')

MSE(yhat.rlasso, test$INDPRO) #0.9445325

rhat.rlasso=rlasso.fit$residuals

tic()
rffit.ht = tuneRF(train_mat, rhat.rlasso, mtryStart=floor(sqrt(ncol(train_mat))), stepFactor=2, improve=0.05, nodesize=5, ntree=5000, doBest=TRUE, plot=FALSE, trace=FALSE)
toc()
rfht.pred_cv = predict(rffit.ht, newdata=vad_mat)
hybridt_cv=rfht.pred_cv+yhat.rlasso_cv
write.csv(hybridt_cv,'C:\\Users\\JC\\Desktop\\20-21 Sem 1\\EC4308\\Project\\hybrid_1step_CV.csv')
MSE(hybridt, test$INDPRO) #23.91987
##

#Lasso (user-defined grid) using AIC/BIC gave better performance than AICC.

#Lasso with user-defined grid gave better performance (test MSE). 
#The predictors and coeffs are stored in lasso_u.coef

#P-Lasso (2 methods)

#Method 1 (Run Lasso on a grid of lambdas --> run OLS--> choose best lambda by BIC)
grid_plasso = grid[1:80]
lasso.mod <- glmnet(train_mat, train$INDPRO, alpha = 1,lambda=grid_plasso)

lambdas_to_try <- lasso.mod$lambda
aic <- c()
bic <- c()

for (lambda in seq(lambdas_to_try)) {
  temp_min.coef =abs(predict(lasso.mod, s = lambdas_to_try[lambda], type = "coef"))
  temp_min.use = which(temp_min.coef > 0)-1
  temp_min.df = data.frame(out = train$INDPRO, W = train_mat[,temp_min.use])
  temp_min_n.ols = lm(out~., data = temp_min.df)
  aic[lambda] = AIC(temp_min_n.ols)
  bic[lambda] = BIC(temp_min_n.ols)
}

which.min(aic) #80
which.min(bic) #44

bestlam_post1 = lambdas_to_try[80] #aic
bestlam_post1 #0.004037017

temp_min.coef =abs(predict(lasso.mod, s = bestlam_post1, type = "coef"))
temp_min.use = which(temp_min.coef > 0)-1
temp_min.df = data.frame(out = train$INDPRO, W = train_mat[,temp_min.use])
temp_min_n.ols = lm(out~., data = temp_min.df)
temp_min.testdf = data.frame(out = test$INDPRO, W = test_mat[,temp_min.use])
MSE_min = mean((test$INDPRO - predict(temp_min_n.ols, newdata = temp_min.testdf))^2)
options(scipen = 0)
MSE_min #498.9692

bestlam_post1 = lambdas_to_try[44] #bic
bestlam_post1 #0.04977024

temp_min.coef =abs(predict(lasso.mod, s = bestlam_post1, type = "coef"))
temp_min.use = which(temp_min.coef > 0)-1
temp_min.df = data.frame(out = train$INDPRO, W = train_mat[,temp_min.use])
temp_min_n.ols = lm(out~., data = temp_min.df)
temp_min.testdf = data.frame(out = test$INDPRO, W = test_mat[,temp_min.use])
MSE_min = mean((test$INDPRO - predict(temp_min_n.ols, newdata = temp_min.testdf))^2)
options(scipen = 0)
MSE_min #50.59534

#Method 2 (Run Lasso on a grid of lambdas --> choose best lambda by BIC --> run OLS)
lasso.mod <- glmnet(train_mat, train$INDPRO, alpha = 1,lambda=grid)

temp_min.coef =abs(predict(lasso.mod, s = bestlam_u, type = "coef"))
temp_min.use = which(temp_min.coef > 0)-1
temp_min.df = data.frame(out = train$INDPRO, W = train_mat[,temp_min.use])
temp_min.ols = lm(out~., data = temp_min.df)
temp_min.testdf = data.frame(out = test$INDPRO, W = test_mat[,temp_min.use])
temp_min.vad_df = data.frame(out = valset$INDPRO, W = vad_mat[,temp_min.use])
MSE_min = mean((test$INDPRO - predict(temp_min.ols, newdata = temp_min.testdf))^2)
MSE_min #0.8331832

plasso_u_pred = predict(temp_min.ols, newdata = temp_min.vad_df)
write.csv(plasso_u_pred,'C:\\Users\\JC\\Desktop\\20-21 Sem 1\\EC4308\\Project\\plasso_aic_bic_1step_CV.csv')

PLasso_bic = as.data.frame(temp_min.ols$coefficients)

#PLasso_bic gives us the predictors and coefficients of model.

#P-Lasso using Method 2 gives a better performance than Method 1.
#P-LASSO using Method 2 gives a better performance than LASSO.

methods = c('Lasso(default grid)-AIC/BIC/AICC','Lasso(user-defined grid)-AIC/BIC','Lasso(user-defined grid)-AICC','P-Lasso(Method 1)-AIC','P-Lasso(Method 1)-BIC','P-Lasso(Method 2)-AIC/BIC')
values = c(1.051,0.6998594,4.179424,0.863655,2.720331,0.6953953)
results_df = data.frame(methods,test_MSE=values)
ordered = order(-results_df$test_MSE)
results_df_ordered = results_df[ordered,]
row.names(results_df_ordered) <- NULL
View(results_df_ordered)

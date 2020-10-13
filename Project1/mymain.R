options(warn=-1,message=-1)
set.seed(8742)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(caret)
  library(tidyr)
  library(xgboost)
  library(glmnet)
  library(e1071)
})


hotEncoding = function(dataset, testFlag) {
  if(testFlag == TRUE) {
    dummies_model =  dummyVars(" ~ .", data=dataset)
  }
  else {
    dummies_model= dummyVars(Sale_Price ~ ., data=dataset)
  }
  dataset = predict(dummies_model, newdata = dataset)
  dataset = data.frame(dataset)
  return(dataset)
}

RMSE = function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}


removeVars = function(data,remove.var) {
  
  data[,!names(data) %in% remove.var]
}


winsorize = function(data, winsor.vars) {
  
  quan.value <- 0.95
  for(var in winsor.vars){
    tmp <- data[, var]
    myquan <- quantile(tmp, probs = quan.value, na.rm = TRUE)
    tmp[tmp > myquan] <- myquan
    data[, var] <- tmp
  }
  data
}

remove.var <- c('Street', 'Utilities',  'Condition_2', 'Roof_Matl', 'Heating', 'Pool_QC', 'Misc_Feature', 'Low_Qual_Fin_SF', 'Pool_Area', 'Longitude','Latitude')

winsor.vars <- c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_2", "Bsmt_Unf_SF", "Total_Bsmt_SF", "Second_Flr_SF", 'First_Flr_SF', "Gr_Liv_Area", "Garage_Area", "Wood_Deck_SF", "Open_Porch_SF", "Enclosed_Porch", "Three_season_porch", "Screen_Porch", "Misc_Val")


#Data Preprocessing 
myPreProcess = function(data,testFlag) {

  d <- preProcess(data, "medianImpute")
  data = predict(d,data)
  data = winsorize(data, winsor.vars)
  data_encoded = hotEncoding(data,testFlag)
  data_encoded = removeVars(data_encoded,remove.var)
  data_encoded$PID = data$PID
  if("Sale_Price" %in% colnames(data))
  {
    data_encoded$Sale_Price = data$Sale_Price
    data_encoded$Sale_Price_Log = log(data$Sale_Price)
  }
  data_encoded <- data_encoded %>% 
    fill(
      dplyr::everything()
    )
  data_encoded
}


#Read Dataset

trainData <- read.csv("train.csv")
testData <- read.csv("test.csv")

trainData = myPreProcess(trainData,FALSE)
testData = myPreProcess(testData,TRUE)

dim(trainData)
dim(testData)

#Model Building

# Model 1: Lasso and Ridge mixed regression

set.seed(8742)

X = trainData[,!names(trainData) %in% c("Sale_Price","Sale_Price_Log","PID")]

cv_lasso=cv.glmnet(as.matrix(X),trainData$Sale_Price_Log, nfolds = 10, alpha = 1)

# select variables with Lasso
sel.vars <- predict(cv_lasso, type="nonzero", s = cv_lasso$lambda.min)$X1
sel.vars = names(X[,sel.vars])

lasso_mod = cv.glmnet(as.matrix(X[,sel.vars]), trainData$Sale_Price_Log, alpha = 1)

# ## Predictions with Ridge
preds_train<-predict(lasso_mod,newx=as.matrix(X[,sel.vars]),s=cv_lasso$lambda.min, alpha = 0)
print(paste("Model1 Lasso Train RMSE:",RMSE(trainData$Sale_Price_Log,preds_train)))
#trainData$pred_sale_price = preds_trai

missing = setdiff(sel.vars,names(testData))
if(length(missing) > 0)
{
  testData[missing] = 0
}
preds<-predict(lasso_mod,newx=as.matrix(testData[,sel.vars]),s=cv_lasso$lambda.min, alpha = 0)
#paste("Model1:Lasso Test RMSE:",RMSE(testData$Sale_Price_Log,preds))

mysubm1 = data.frame(PID=testData[,"PID"],Sale_Price=round(exp(preds),1))
colnames(mysubm1) = c("PID","Sale_Price")
write.csv(mysubm1, file = "mysubmission1.txt", row.names = FALSE,quote = FALSE)


# Model 2: Boost model

set.seed(8742)

X = trainData[,!names(trainData) %in% c("Sale_Price","Sale_Price_Log","PID")]

xgbFit = xgboost(data = as.matrix(X), label = as.matrix(trainData$Sale_Price_Log), 
                 nrounds = 1000, verbose = FALSE, objective = "reg:squarederror", eval_metric = "rmse", 
                 eta = 0.0474239, max_depth = 4, subsample = 0.541795)
## Predictions
# rmse of training data
predict_rf_train = predict(xgbFit, newdata = as.matrix(X))
print(paste("Model2 XGBoost Train RMSE:",RMSE(trainData$Sale_Price_Log, predict_rf_train)))
# rmse of testing data
missing = setdiff(names(trainData),names(testData))
if(length(missing) > 0)
{
    testData[missing] = 0
}
preds2 <- predict(xgbFit, newdata = as.matrix(testData[,names(X)]))
#paste("Model2 XGBoost Test RMSE:",RMSE(testData$Sale_Price_Log, preds2))

mysubm2 = data.frame(PID=testData[,c("PID")],Sale_Price=round(exp(preds2),1))
colnames(mysubm2) = c("PID","Sale_Price")
write.csv(mysubm2, file = "mysubmission2.txt", row.names = FALSE, quote = FALSE)

# comment the following before submit since there will be no test_y
#test.y <- read.csv("test_y.csv")
#pred <- read.csv("mysubmission1.txt")
#names(test.y)[2] <- "True_Sale_Price"
#pred <- merge(pred, test.y, by="PID")
#print(paste("My Submission1 RMSE:",sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2))))

#pred <- read.csv("mysubmission2.txt")
#names(test.y)[2] <- "True_Sale_Price"
#pred <- merge(pred, test.y, by="PID")
#print(paste("My Submission2 RMSE:",sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2))))



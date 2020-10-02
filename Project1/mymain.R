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

hotEncoding = function(dataset) {
  dummies_model = dummyVars(Sale_Price ~ . -PID, data=dataset)
  dataset = predict(dummies_model, newdata = dataset)
  dataset = data.frame(dataset)
}

RMSE = function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}


removeVars = function(data) {
  
  remove.var <- c('Street', 'Utilities',  'Condition_2', 'Roof_Matl', 'Heating', 'Pool_QC', 'Misc_Feature', 'Low_Qual_Fin_SF', 'Pool_Area', 'Longitude','Latitude',"Lot_Frontage")
  data[,!names(data) %in% remove.var]
}

getSkewedVars = function(data) {
  skewedVars <- NA
  
  for(i in names(data)){
    if(is.numeric(data[,i])){
      if((i != "Sale_Price") & (i != "PID")){
        if(length(levels(as.factor(data[,i]))) > 10){
          # Enters this block if variable is non-categorical
          skewVal <- skewness(data[,i])
#          print(paste(i, skewVal, sep = ": "))
          if(abs(skewVal) > 0.5){
            skewedVars <- c(skewedVars, i)
          }
        }
      }
    }
  }
  skewedVars[-1]
}

#Read Dataset

data = read.csv("Ames_data.csv")

#Data Preprocessing 
data = removeVars(data)
d <- preProcess(data, "medianImpute")
data = predict(d,data)

skewed.vars = getSkewedVars(data)
data_encoded = hotEncoding(data)

for(i in skewed.vars){
  if(0 %in% data_encoded[, i]){
    data_encoded[,i] <- log(1+data_encoded[,i])
  }
  else{
    data_encoded[,i] <- log(data_encoded[,i])
  }
}

data_encoded$PID = data$PID
data_encoded$Sale_Price = data$Sale_Price
data_encoded$Sale_Price_Log = log(data$Sale_Price)

fit = lm(Sale_Price_Log ~ Lot_Area + Mas_Vnr_Area + BsmtFin_SF_2 + Bsmt_Unf_SF + Total_Bsmt_SF + Second_Flr_SF + First_Flr_SF + Gr_Liv_Area + Garage_Area + Wood_Deck_SF + Open_Porch_SF + Enclosed_Porch + Three_season_porch + Screen_Porch + Misc_Val, data = data_encoded)
fit_cd = cooks.distance(fit)
data_encoded = data_encoded[fit_cd < 4 / length(fit_cd),]


paste("Number of Missing Values", sum(is.na(data_encoded)))
data_encoded <- data_encoded %>% 
  fill(
    dplyr::everything()
  )
# counting number of missing values
paste("Number of Missing Values", sum(is.na(data_encoded)))


testIDs <- read.table("project1_testIDs.dat")
j <- 2
train <- data_encoded[-testIDs[,j], ]
test <- data_encoded[testIDs[,j], ]
#test.y <- test[, c(1, 136,137)]
#test <- test[, -c(136,137)]
test.y <- test[, c("PID", "Sale_Price","Sale_Price_Log")]
test <- test[, !names(test) %in% c("Sale_Price","Sale_Price_Log")]

write.csv(train,"train.csv",row.names=FALSE)
write.csv(test, "test.csv",row.names=FALSE)
write.csv(test.y,"test_y.csv",row.names=FALSE)

trainData <- read.csv("train.csv")
testData <- read.csv("test.csv")
testData = testData[complete.cases(testData),]
test.y = test.y[complete.cases(test.y),]

#Model Building

set.seed(8742)

X = trainData[,!names(trainData) %in% c("Sale_Price","Sale_Price_Log","PID")]

cv_lasso=cv.glmnet(as.matrix(X),trainData$Sale_Price_Log, nfolds = 10, alpha = 1)
cv_lasso$lambda.min

# select variables
sel.vars <- predict(cv_lasso, type="nonzero", s = cv_lasso$lambda.min)$X1

lasso_mod = cv.glmnet(as.matrix(X[,sel.vars]), trainData$Sale_Price_Log, alpha = 1)

# ## Predictions
preds_train<-predict(lasso_mod,newx=as.matrix(X[,sel.vars]),s=cv_lasso$lambda.min, alpha = 1)
paste("Model1 Lasso Train RMSE:",RMSE(trainData$Sale_Price_Log,preds_train))
#trainData$pred_sale_price = preds_train

preds<-predict(lasso_mod,newx=as.matrix(testData[,sel.vars]),s=cv_lasso$lambda.min, alpha = 1)
paste("Model1:Lasso Test RMSE:",RMSE(test.y$Sale_Price_Log,preds))
test.y$pred_sale_price = round(exp(preds),1)

mysubm1 = data.frame(test.y[,c("PID","pred_sale_price")])
colnames(mysubm1) = c("PID","Sale_Price")
write.csv(mysubm1, file = "mysubmission1.txt", row.names = FALSE)

set.seed(8742)
xgbFit = xgboost(data = as.matrix(trainData[,!names(trainData) %in% c("Sale_Price","Sale_Price_Log","pred_sale_price")]),
                 label = as.matrix(trainData$Sale_Price_Log), 
                 nrounds = 1000, verbose = FALSE, objective = "reg:squarederror", eval_metric = "rmse", 
                 eta = 0.04864, max_depth = 4, subsample = 0.5959)
## Predictions
# rmse of training data
predict_rf_train = predict(xgbFit, newdata = as.matrix(trainData[,!names(trainData) %in% c("Sale_Price","Sale_Price_Log","pred_sale_price")]))
paste("Model2 XGBoost Train RMSE:",RMSE(trainData$Sale_Price_Log, predict_rf_train))
# rmse of testing data
preds2 <- predict(xgbFit, newdata = as.matrix(testData))
paste("Model2 XGBoost Test RMSE:",RMSE(test.y$Sale_Price_Log, preds2))
test.y$pred_sale_price = round(exp(preds2),1)

mysubm2 = data.frame(test.y[,c("PID","pred_sale_price")])
colnames(mysubm2) = c("PID","Sale_Price")
write.csv(mysubm2, file = "mysubmission2.txt", row.names = FALSE)


pred <- read.csv("mysubmission1.txt")
names(test.y)[2] <- "True_Sale_Price"
pred <- merge(pred, test.y, by="PID")
paste("My Submission1 RMSE:",sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2)))

pred <- read.csv("mysubmission2.txt")
names(test.y)[2] <- "True_Sale_Price"
pred <- merge(pred, test.y, by="PID")
paste("My Submission2 RMSE:",sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2)))

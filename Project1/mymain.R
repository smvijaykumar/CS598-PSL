library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(xgboost)
library(glmnet)

hotEncoding = function(dataset) {
  
  dummies_model = dummyVars(Sale_Price ~ ., data=dataset)
  
  dataset = predict(dummies_model, newdata = dataset)
  dataset = data.frame(dataset)
  # Find zero varience factors using caret
  nzv.data = nearZeroVar(dataset, saveMetrics = TRUE)
  
  # Remove zero variance factors
  drop.cols = rownames(nzv.data)[nzv.data$nzv == TRUE]
  dataset = dataset[,!names(dataset) %in% drop.cols]
}

RMSE = function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

data = read.csv("Ames_data.csv")
data_encoded = hotEncoding(data)
data_encoded$Sale_Price = data$Sale_Price
data_encoded$Sale_Price_Log = log(data$Sale_Price)

testIDs <- read.table("project1_testIDs.dat")
j <- 2
train <- data_encoded[-testIDs[,j], ]
test <- data_encoded[testIDs[,j], ]
test.y <- data_encoded[, c(1, 136,137)]
test <- data_encoded[, -c(136,137)]
write.csv(train,"train.csv",row.names=FALSE)
write.csv(test, "test.csv",row.names=FALSE)
write.csv(test.y,"test_y.csv",row.names=FALSE)

train <- read.csv("train.csv")

test <- read.csv("test.csv")

paste("Number of Missing Values", sum(is.na(train)))
trainData <- train %>% 
  fill(
    dplyr::everything()
  )

testData <- test %>% 
  fill(
    dplyr::everything()
  )
# counting number of missing values
paste("Number of Missing Values", sum(is.na(trainData)))

set.seed(8742)
X = trainData[,!names(trainData) %in% c("Sale_Price")]

# # oob = trainControl(method = "oob")
# cv_10 = trainControl(method = "cv", number = 10)
# # 
# # 
# rf_grid =  data.frame(mtry = seq(12,36,6))
# bank_rf_tune = train(Sale_Price_Log ~ ., data = X,
#                       method = "rf",
#                       trControl = cv_10,
#                       preProcess=c("center","scale"),
#                       verbose = TRUE,
#                       tuneGrid = rf_grid,ntree = 500, metric="RMSE")
#  bank_rf_tune

rf_mtry = ceiling(sqrt(ncol(X)))
model_rf = randomForest(Sale_Price_Log ~ ., 
                        data = X,
                        mtry = 30,
                        importance = T,ntree = 500)
# plot of random forest
summary(model_rf)

# rmse of training data
predict_rf_train = predict(model_rf,mtry =30, ntree =500)
RMSE(trainData$Sale_Price_Log, predict_rf_train)

# rmse of testing data
preds2 <- predict(model_rf, newdata = as.matrix(testData), mtry =30, ntree =500 )
RMSE(test.y$Sale_Price_Log, preds2)

test.y$pred_sale_price = round(exp(preds2),1)
mysubm1 = data.frame(test.y[,c("PID","pred_sale_price")])
colnames(mysubm1) = c("PID","Sale_Price")
write.csv(mysubm1, file = "mysubmission1.txt", row.names = FALSE,quote = FALSE)

set.seed(8742)
xgbFit = xgboost(data = as.matrix(trainData[,!names(trainData) %in% c("Sale_Price","Sale_Price_Log")]), nfold = 5, label = as.matrix(train$Sale_Price_Log), 
                 nrounds = 2200, verbose = FALSE, objective = "reg:linear", eval_metric = "rmse", 
                 nthread = 8, eta = 0.01, gamma = 0.0468, max_depth = 6, min_child_weight = 1.7817, 
                 subsample = 0.5213, colsample_bytree = 0.4603)
## print(xgbFit)

## Predictions
# rmse of training data
predict_rf_train = predict(xgbFit, newdata = as.matrix(trainData[,!names(trainData) %in% c("Sale_Price","Sale_Price_Log")]))
RMSE(trainData$Sale_Price_Log, predict_rf_train)
# rmse of testing data
preds2 <- predict(xgbFit, newdata = as.matrix(testData))
RMSE(test.y$Sale_Price_Log, preds2)

test.y$pred_sale_price = round(exp(preds2),1)
mysubm2 = data.frame(test.y[,c("PID","pred_sale_price")])
colnames(mysubm2) = c("PID","Sale_Price")
write.csv(mysubm2, file = "mysubmission2.txt", row.names = FALSE, quote = FALSE)

pred <- read.csv("mysubmission1.txt")
names(test.y)[2] <- "True_Sale_Price"
pred <- merge(pred, test.y, by="PID")
sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2))

pred <- read.csv("mysubmission2.txt")
names(test.y)[2] <- "True_Sale_Price"
pred <- merge(pred, test.y, by="PID")
sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2))

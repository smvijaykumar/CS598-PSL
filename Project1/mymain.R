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
  dummies_model = dummyVars(Sale_Price ~ ., data=dataset)
  dataset = predict(dummies_model, newdata = dataset)
  dataset = data.frame(dataset)
}

RMSE = function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}


removeVars = function(data,remove.var) {
  
  #remove.var <- c('Street', 'Utilities',  'Condition_2', 'Roof_Matl', 'Heating', 'Pool_QC', 'Misc_Feature', 'Low_Qual_Fin_SF', 'Pool_Area', 'Longitude','Latitude')
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
          paste(names(data[,i]),skewVal)
          if(abs(skewVal) > 0.5){
            skewedVars <- c(skewedVars, i)
            
          }
        }
      }
    }
  }
  skewedVars[-1]
}

winsorize = function(data) {
  winsor.vars <- c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_2", "Bsmt_Unf_SF", "Total_Bsmt_SF", "Second_Flr_SF", 'First_Flr_SF', "Gr_Liv_Area", "Garage_Area", "Wood_Deck_SF", "Open_Porch_SF", "Enclosed_Porch", "Three_season_porch", "Screen_Porch", "Misc_Val")
  quan.value <- 0.95
  for(var in winsor.vars){
    tmp <- data[, var]
    myquan <- quantile(tmp, probs = quan.value, na.rm = TRUE)
    tmp[tmp > myquan] <- myquan
    data[, var] <- tmp
  }
  data
}


#Data Preprocessing 
# Find zero varience factors using caret
myPreProcess = function(data) {

  
  d <- preProcess(data, "medianImpute")
  data = predict(d,data)
  data = winsorize(data)
  skewed.vars = getSkewedVars(data)
  data_encoded = hotEncoding(data)
  # for(i in skewed.vars){
  #   if(0 %in% data_encoded[, i]){
  #     data_encoded[,i] <- log(1+data_encoded[,i])
  #   }
  #   else{
  #     data_encoded[,i] <- log(data_encoded[,i])
  #   }
  # }
  nzv.data = nearZeroVar(data_encoded, saveMetrics = TRUE)
  
  # Remove zero variance factors
  drop.cols = rownames(nzv.data)[nzv.data$nzv == TRUE]
  remove.var = c(drop.cols, "Longitude","Latitude")
  data_encoded = removeVars(data_encoded,remove.var)
  
  data_encoded$PID = data$PID
  data_encoded$Sale_Price = data$Sale_Price
  data_encoded$Sale_Price_Log = log(data$Sale_Price)
  
  fit = lm(Sale_Price_Log ~ Lot_Area + Mas_Vnr_Area  + Bsmt_Unf_SF + Total_Bsmt_SF + Second_Flr_SF + First_Flr_SF + Gr_Liv_Area + Garage_Area + Wood_Deck_SF   , data = data_encoded)
  fit_cd = cooks.distance(fit)
  data_encoded = data_encoded[fit_cd < 4 / length(fit_cd),]
  
  data_encoded <- data_encoded %>% 
    fill(
      dplyr::everything()
    )
  data_encoded
}


#Read Dataset

data = read.csv("Ames_data.csv")
testIDs <- read.table("project1_testIDs.dat")
j <- 4
train <- data[-testIDs[,j], ]
test <- data[testIDs[,j], ]

test.y <- test[, c("PID", "Sale_Price")]
test <- test[, !names(test) %in% c("Sale_Price")]

write.csv(train,"train.csv",row.names=FALSE)
write.csv(test, "test.csv",row.names=FALSE)
write.csv(test.y,"test_y.csv",row.names=FALSE)

trainData <- read.csv("train.csv")
testData <- read.csv("test.csv")

trainData = myPreProcess(trainData)
testData1 = merge(testData, test.y, by = "PID")
testData = myPreProcess(testData1)

dim(trainData)
dim(testData)
#testData = testData[complete.cases(testData),]
#test.y = test.y[complete.cases(test.y),]

#Model Building

set.seed(8742)

X = trainData[,!names(trainData) %in% c("Sale_Price","Sale_Price_Log","PID")]

cv_lasso=cv.glmnet(as.matrix(X),trainData$Sale_Price_Log, nfolds = 10, alpha = 1)

# select variables
sel.vars <- predict(cv_lasso, type="nonzero", s = cv_lasso$lambda.min)$X1
sel.vars = names(X[,sel.vars])

lasso_mod = cv.glmnet(as.matrix(X[,sel.vars]), trainData$Sale_Price_Log, alpha = 1)

# ## Predictions
preds_train<-predict(lasso_mod,newx=as.matrix(X[,sel.vars]),s=cv_lasso$lambda.min, alpha = 1)
paste("Model1 Lasso Train RMSE:",RMSE(trainData$Sale_Price_Log,preds_train))
#trainData$pred_sale_price = preds_trai

missing = setdiff(sel.vars,names(testData))
if(length(missing) > 0)
  testData[missing] = 0
preds<-predict(lasso_mod,newx=as.matrix(testData[,sel.vars]),s=cv_lasso$lambda.min, alpha = 1)
paste("Model1:Lasso Test RMSE:",RMSE(testData$Sale_Price_Log,preds))

mysubm1 = data.frame(PID=testData[,"PID"],Sale_Price=round(exp(preds),1))
colnames(mysubm1) = c("PID","Sale_Price")
write.csv(mysubm1, file = "mysubmission1.txt", row.names = FALSE,quote = FALSE)

# set.seed(8742)
# start.time <- Sys.time()
# X = trainData[,!names(trainData) %in% c("Sale_Price","Sale_Price_Log")]
# dtrain <- xgb.DMatrix(data = as.matrix(X), label = as.matrix(trainData$Sale_Price_Log)) 
# dtest <- xgb.DMatrix(data = as.matrix(testData[,names(X)]), label=testData$Sale_Price_Log)
# 
# # Create empty lists
# lowest_error_list = list()
# parameters_list = list()
# 
# # Create 10,000 rows with random hyperparameters
# set.seed(8742)
# for (iter in 1:10){
#   param <- list(booster = "gbtree",
#                 objective = "reg:squarederror",
#                 max_depth = sample(4:6, 1),
#                 eta = runif(1, .04, .05),
#                 subsample = runif(1, .5, 0.6)
#   )
#   parameters <- as.data.frame(param)
#   parameters_list[[iter]] <- parameters
# }
# 
# # Create object that contains all randomly created hyperparameters
# parameters_df = do.call(rbind, parameters_list)
# 
# 
# # Use randomly created parameters to create 10,000 XGBoost-models
# for (row in 1:nrow(parameters_df)){
#   set.seed(8742)
#   mdcv <- xgb.train(data = dtrain, 
#                     booster = "gbtree",
#                     objective = "reg:squarederror",
#                     max_depth = parameters_df$max_depth[row],
#                     eta = parameters_df$eta[row],
#                     subsample = parameters_df$subsample[row],
#                     nrounds= 1000,
#                     eval_metric = "rmse",
#                     early_stopping_rounds= 30,
#                     verbose = FALSE,
#                     watchlist = list(train= dtrain, val= dtest)
#   )
#   lowest_error = mdcv$evaluation_log[mdcv$best_iteration,]
#   lowest_error_list[[row]] = lowest_error
# }
# 
# # Create object that contains all accuracy's
# lowest_error_df = do.call(rbind, lowest_error_list)
# 
# # Bind columns of accuracy values and random hyperparameter values
# randomsearch = cbind(lowest_error_df, parameters_df)
# 
# # Quickly display highest accuracy
# best_xg_param = randomsearch[which(randomsearch$val_rmse == min(randomsearch$val_rmse)),]
# best_xg_param
# # Stop time and calculate difference
# end.time = Sys.time()
# time.taken = end.time - start.time
# time.taken



set.seed(8742)
X = trainData[,!names(trainData) %in% c("Sale_Price","Sale_Price_Log","PID")]

xgbFit = xgboost(data = as.matrix(X), label = as.matrix(trainData$Sale_Price_Log), 
                 nrounds = 1000, verbose = FALSE, objective = "reg:squarederror", eval_metric = "rmse", 
                 eta = 0.0474239, max_depth = 4, subsample = 0.541795)
## Predictions
# rmse of training data
predict_rf_train = predict(xgbFit, newdata = as.matrix(X))
paste("Model2 XGBoost Train RMSE:",RMSE(trainData$Sale_Price_Log, predict_rf_train))
# rmse of testing data
missing = setdiff(names(trainData),names(testData))
testData[missing] = 0
preds2 <- predict(xgbFit, newdata = as.matrix(testData[,names(X)]))
paste("Model2 XGBoost Test RMSE:",RMSE(testData$Sale_Price_Log, preds2))

mysubm2 = data.frame(PID=testData[,c("PID")],Sale_Price=round(exp(preds2),1))
colnames(mysubm2) = c("PID","Sale_Price")
write.csv(mysubm2, file = "mysubmission2.txt", row.names = FALSE, quote = FALSE)


pred <- read.csv("mysubmission1.txt")
names(test.y)[2] <- "True_Sale_Price"
pred <- merge(pred, test.y, by="PID")
paste("My Submission1 RMSE:",sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2)))

pred <- read.csv("mysubmission2.txt")
names(test.y)[2] <- "True_Sale_Price"
pred <- merge(pred, test.y, by="PID")
paste("My Submission2 RMSE:",sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2)))



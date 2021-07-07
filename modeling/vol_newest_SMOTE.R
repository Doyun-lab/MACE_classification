library(randomForest)
library(xgboost)
library(tidyverse)
library(caret)
library(e1071)
library(dummies)
library(keras)

setwd("/Users/kwondoyun/Downloads/vol_newest")

d <- getwd()
fls <- dir(d, recursive = T)
fls_train <- fls[grep("train", fls)]
fls_test <- fls[grep("test", fls)]

i <- 1
vol_new_train <- list()
for (f in fls_train){
  path <- file.path(str_c(d, "/", f)) 
  temp <- read.csv(path)
  temp <- temp %>% select(-X)
  
  temp$Class <- as.factor(temp$Class)
  
  vol_new_train[[i]] <- temp
  i <- i + 1
}

i <- 1
vol_new_test <- list()
for (f in fls_test){
  path <- file.path(str_c(d, "/", f)) 
  temp <- read.csv(path)
  temp <- temp %>% select(-X)
  
  temp$Class <- as.factor(temp$Class)
  
  vol_new_test[[i]] <- temp
  i <- i + 1
}

## XGBoost
set.seed(234)
conf_vol_new_xgb <- list()
for (i in 1:10){
  train <- vol_new_train[[i]]
  test <- vol_new_test[[i]]
  
  train_x <- train %>% select(-Class) %>% data.matrix
  train_y <- train$Class
  
  test_x <- test %>% select(-Class) %>% data.matrix
  test_y <- test$Class
  
  xgb.fit = xgboost(data = train_x, label = as.numeric(train_y)-1, eta = 0.01, nrounds = 1000,
                    objective = "binary:logistic")
  
  y_pred_xgb = predict(xgb.fit, test_x)
  prediction_xgb = as.numeric(y_pred_xgb > 0.5)
  prediction_xgb = as.factor(prediction_xgb)
  
  y = as.factor(ifelse(test_y == "True", 1, 0))
  conf_mat <- confusionMatrix(prediction_xgb, y)
  conf_vol_new_xgb[[i]] <- conf_mat$table
}

## Random Forest
set.seed(234)
conf_vol_new_rf <- list()
for (i in 1:10){
  train <- vol_new_train[[i]]
  test <- vol_new_test[[i]]
  
  test_x <- test %>% select(-Class) 
  test_y <- test$Class
  
  rf.fit = randomForest(Class ~ ., data=train, mtry=floor(sqrt(length(train)-1)), ntree=500, importance=T)
  
  y_pred = predict(rf.fit, test_x)
  
  conf_mat <- confusionMatrix(y_pred, test_y)
  conf_vol_new_rf[[i]] <- conf_mat$table
  cat("Fold", i, "complete \n", sep = " ")
}

## DNN
set.seed(234)
conf_vol_new_dnn <- list()
for (i in 1:10){
  train <- vol_new_train[[i]]
  test <- vol_new_test[[i]]
  
  #inTrain = createDataPartition(1:nrow(train), p=0.8, list=FALSE)
  
  #train <- train[inTrain,]
  #val <- train[-inTrain,]
  
  train_x <- train %>% 
    data.frame() %>% 
    select(-Class) %>% 
    as.matrix()
  
  #val_x <- val %>%
  #  data.frame() %>% 
  #  select(-Class) %>% 
  #  as.matrix()
  
  test_x <- test %>% 
    data.frame() %>% 
    select(-Class) %>% 
    as.matrix()
  
  normalize <- function(x) {
    return((x-min(x))/(max(x)-min(x)))
  }
  
  train_x <- apply(train_x, 2, function(x){normalize(x)})
  #val_x <- apply(val_x, 2, function(x){normalize(x)})
  test_x <- apply(test_x, 2, function(x){normalize(x)})
  
  train_x[is.na(train_x)] <- 0
  #val_x[is.na(val_x)] <- 0
  test_x[is.na(test_x)] <- 0
  
  train$Class <- as.factor(train$Class)
  #val$Class <- as.factor(val$Class)
  test$Class <- as.factor(test$Class)
  
  train_y <- train$Class %>% as.numeric()-1
  #val_y <- val$Class %>% as.numeric()-1
  test_y <- test$Class %>% as.numeric()-1
  
  model <- keras_model_sequential()
  
  model %>% 
    layer_dense(units = 512, activation = 'relu', input_shape = ncol(train)-1) %>% 
    layer_dropout(rate = 0.4) %>% 
    layer_dense(units = 256, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 128, activation = 'relu') %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = 25, activation = 'relu') %>%
    layer_dense(units = 1, activation = 'sigmoid')
  
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer=optimizer_adam(lr = 0.005),
    metrics = c("accuracy")
  )
  
  history <- model %>% fit(
    train_x,
    train_y,
    epochs=100,
    batch_size=50,
    validation_split=0.1
  )
  
  pred <- predict(model, test_x) %>% round()
  
  conf_mat <- confusionMatrix(factor(pred), factor(test_y))
  conf_vol_new_dnn[[i]] <- conf_mat$table
  cat("Fold", i, "complete \n", sep = " ")
}

## Confusion Matrix
confusion_matrix(conf_vol_new_xgb)
confusion_matrix(conf_vol_new_rf)
confusion_matrix(conf_vol_new_dnn)

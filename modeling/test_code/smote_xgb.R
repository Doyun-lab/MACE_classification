library(randomForest)
library(xgboost)
library(tidyverse)
library(caret)
library(e1071)
library(dummies)
library(ROSE)
library(keras)

setwd("C:/Users/MY/Desktop/research/MACE/tiva_smote")

tiva_name <- read.csv("C:/Users/MY/Desktop/research/MACE/data/tiva_col_name.csv", header = F)
tiva_col <- as.character(tiva_name[1,])

d <- getwd()
fls <- dir(d, recursive = T)
fls_train <- fls[grep("train", fls)]
fls_test <- fls[grep("test", fls)]

i <- 1
tiva_train_list <- list()
for (f in fls_train){
  path <- file.path(str_c(d, "/", f)) 
  temp <- read.csv(path, header = F)
  temp <- temp %>% select(-V1)
  colnames(temp) <- tiva_col
  temp <- temp[-1,]
  temp$class <- as.factor(temp$class)
  
  tiva_train_list[[i]] <- temp
  i <- i + 1
}

i <- 1
tiva_test_list <- list()
for (f in fls_test){
  path <- file.path(str_c(d, "/", f)) 
  temp <- read.csv(path, header = F)
  temp <- temp %>% select(-V1)
  colnames(temp) <- tiva_col
  temp <- temp[-1,]
  temp$class <- as.factor(temp$class)
  
  tiva_test_list[[i]] <- temp
  i <- i + 1
}

set.seed(234)
conf_mat_list <- list()
for (i in 1:10){
  train <- tiva_train_list[[i]]
  test <- tiva_test_list[[i]]

  train_x <- train %>% select(-class) %>% data.matrix
  train_y <- train$class
  
  test_x <- test %>% select(-class) %>% data.matrix
  test_y <- test$class
  
  xgb.fit = xgboost(data = train_x, label = as.numeric(train_y)-1, eta = 0.01, nrounds = 1000,
                    objective = "binary:logistic")
  
  y_pred_xgb = predict(xgb.fit, test_x)
  prediction_xgb = as.numeric(y_pred_xgb > 0.5)
  prediction_xgb = as.factor(prediction_xgb)
  
  y = as.factor(ifelse(test_y == "True", 1, 0))
  conf_mat <- confusionMatrix(prediction_xgb, y)
  conf_mat_xgb[[i]] <- conf_mat$table
}
confusion_matrix(conf_mat_xgb)

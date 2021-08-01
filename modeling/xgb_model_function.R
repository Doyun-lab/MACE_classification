library(randomForest)
library(xgboost)
library(tidyverse)
library(caret)
library(e1071)
library(dummies)

setwd("/Users/doyun/Downloads/")

emr_tiva = readRDS("mace_data/emr_TIVA_0517.rds")
bp_tiva = read.csv("mace_data/TIVA_record_expBP.csv", fileEncoding = "euc-kr") %>% select(-X)
record_tiva = read.csv("mace_data/TIVA_record_onlyBP.csv", fileEncoding = "euc-kr") %>% select(-X)

emr_vol = readRDS("mace_data/emr_vol_0517.rds")
bp_vol = read.csv("mace_data/vol_record_onlyBP.csv", fileEncoding = "euc-kr") %>% select(-X)
record_vol = read.csv("mace_data/vol_record_expBP.csv", fileEncoding = "euc-kr") %>% select(-X)

xgb_fit = function(input_data){
  
  set.seed(234)
  
  input_data$class <- as.factor(input_data$class)
  
  conf_mat_list <- list()
  
  cv_list <- createFolds(input_data$class, k = 10)
  for (i in 1:length(cv_list)){
    inTest <- cv_list[[i]]
    
    train <- input_data[-inTest,]
    test <- input_data[inTest,]
    
    x <- upSample(subset(train, select=-class), train$class)
    
    train_x <- x %>% select(-Class) %>% data.matrix
    train_y <- x$Class
    
    test_x <- test %>% select(-class) %>% data.matrix
    test_y <- test$class
    
    xgb.fit = xgboost(data = train_x, label = as.numeric(train_y)-1, eta = 0.01, nrounds = 1000,
                      objective = "binary:logistic")
    
    y_pred_xgb = predict(xgb.fit, test_x)
    prediction_xgb = as.numeric(y_pred_xgb > 0.5)
    prediction_xgb = as.factor(prediction_xgb)
    
    y = as.factor(ifelse(test_y == "TRUE", 1, 0))
    conf_mat <- confusionMatrix(prediction_xgb, y)
    conf_mat_list[[i]] <- conf_mat$table
  }
  
  return(conf_mat_list)
}

confusion_matrix = function(result_list){
  accuracy <- c()
  precision <- c()
  recall <- c()
  accuracy_normal <- c()
  precision_normal <- c()
  recall_normal <- c()
  for(mat in result_list){
    TP <- mat[1]
    FN <- mat[2]
    FP <- mat[3]
    TN <- mat[4]
    
    accuracy <- c(accuracy, (TP+TN)/(TP+FP+FN+TN))
    precision <- c(precision, TP/(TP+FP))
    recall <- c(recall, TP/(TP+FN))
    
    accuracy_normal <- c(accuracy_normal, (TP+TN)/(TP+FP+FN+TN))
    precision_normal <- c(precision_normal, TN/(TN+FN))
    recall_normal <- c(recall_normal, TN/(TN+FP))
  }
  
  acc_mean <- mean(accuracy)
  pre_mean <- mean(precision)
  rec_mean <- mean(recall)
  
  acc_mean_normal <- mean(accuracy_normal)
  pre_mean_normal <- mean(precision_normal)
  rec_mean_normal <- mean(recall_normal)
  
  result <- data.frame(mace = c(acc_mean, pre_mean, rec_mean),
                       normal = c(acc_mean_normal, pre_mean_normal, rec_mean_normal))
  
  rownames(result) <- c("accuracy", "precision", "recall")
  return(result)
}

tiva_emr_xgb <- xgb_fit(emr_tiva)
tiva_bp_xgb <- xgb_fit(bp_tiva)
tiva_record_xgb <- xgb_fit(record_tiva)

vol_emr_xgb <- xgb_fit(emr_vol)
vol_bp_xgb <- xgb_fit(bp_vol)
vol_record_xgb <- xgb_fit(record_vol)

confusion_matrix(tiva_emr_xgb)  
confusion_matrix(tiva_bp_xgb)
confusion_matrix(tiva_record_xgb)

confusion_matrix(vol_emr_xgb)  
confusion_matrix(vol_bp_xgb)
confusion_matrix(vol_record_xgb)

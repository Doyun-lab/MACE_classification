library(randomForest)
library(xgboost)
library(tidyverse)
library(caret)
library(e1071)
library(dummies)

setwd("/Users/doyun/Downloads/")

bp_tiva = read.csv("mace_data/TIVA_record_expBP.csv", fileEncoding = "euc-kr")
record_tiva = read.csv("mace_data/TIVA_record_onlyBP.csv", fileEncoding = "euc-kr")

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
  TP <- c()
  FN <- c()
  FP <- c()
  TN <- c()
  for(mat in result_list){
    TP <- c(TP, mat[1])
    FN <- c(FN, mat[2])
    FP <- c(FP, mat[3])
    TN <- c(TN, mat[4])
  }
  
  TP_sum <- sum(TP)
  FN_sum <- sum(FN)
  FP_sum <- sum(FP)
  TN_sum <- sum(TN)
  
  accuracy <- (TP_sum+TN_sum)/(TP_sum+FP_sum+FN_sum+TN_sum)
  precision <- TP_sum/(TP_sum+FP_sum)
  recall <- TP_sum/(TP_sum+FN_sum)
  
  accuracy_normal <- (TP_sum+TN_sum)/(TP_sum+FP_sum+FN_sum+TN_sum)
  precision_normal <- TN_sum/(TN_sum+FN_sum)
  recall_normal <- TN_sum/(TN_sum+FP_sum)
  
  result <- data.frame(mace = c(accuracy, precision, recall),
                       normal = c(accuracy_normal, precision_normal, recall_normal))
  
  rownames(result) <- c("accuracy", "precision", "recall")
  return(result)
}

tiva_bp_xgb <- xgb_fit(bp_tiva)
tiva_record_xgb <- xgb_fit(record_tiva)

confusion_matrix(tiva_bp_xgb)
confusion_matrix(tiva_record_xgb)

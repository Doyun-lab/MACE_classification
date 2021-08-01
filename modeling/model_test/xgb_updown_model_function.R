library(randomForest)
library(xgboost)
library(tidyverse)
library(caret)
library(e1071)
library(dummies)

setwd("C:/Users/MY/Downloads")

xgb_fit_updown = function(input_data){
  
  set.seed(234)
  
  input_data$class <- as.factor(input_data$class)
  input_data <- input_data %>% select(-X)
  
  conf_mat_list <- list()
  
  cv_list <- createFolds(input_data$class, k = 10)
  for (i in 1:length(cv_list)){
    inTest <- cv_list[[i]]
    
    train <- input_data[-inTest,]
    test <- input_data[inTest,]
    
    data_true <- subset(train, train$class == "TRUE")
    data_false <- subset(train, train$class == "FALSE")
    
    downTrue <- createDataPartition(1:nrow(data_true), p=0.5, list=FALSE)
    
    data_T_down <- data_true[downTrue,]
    data <- rbind(data_T_down, data_false)
    
    x <- upSample(subset(data, select=-class), data$class)
    
    rand <- sample(nrow(x))
    x_shuf <- x[rand,]
    
    train_x <- x_shuf %>% select(-Class) %>% data.matrix
    train_y <- x_shuf$Class
    
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

tiva_emr <- read.csv("tiva_emr.csv")
tiva_bp <- read.csv("tiva_bp.csv")
tiva_record <- read.csv("tiva_record.csv")
tiva_red <- read.csv("tiva_red.csv")
tiva_new <- read.csv("tiva_record_newest.csv")

vol_emr <- read.csv("vol_emr.csv")
vol_bp <- read.csv("vol_bp.csv")
vol_record <- read.csv("vol_record.csv")
vol_red <- read.csv("vol_red.csv")
vol_new <- read.csv("vol_record_newest.csv")

tiva_emr_xgb <- xgb_fit_updown(tiva_emr)
tiva_bp_xgb <- xgb_fit_updown(tiva_bp)
tiva_record_xgb <- xgb_fit_updown(tiva_record)
tiva_red_xgb <- xgb_fit_updown(tiva_red)
tiva_new_xgb <- xgb_fit_updown(tiva_new)

vol_emr_xgb <- xgb_fit_updown(vol_emr)
vol_bp_xgb <- xgb_fit_updown(vol_bp)
vol_record_xgb <- xgb_fit_updown(vol_record)
vol_red_xgb <- xgb_fit_updown(vol_red)
vol_new_xgb <- xgb_fit_updown(vol_new)

confusion_matrix(tiva_emr_xgb)  
confusion_matrix(tiva_bp_xgb)
confusion_matrix(tiva_record_xgb)
confusion_matrix(tiva_red_xgb)
confusion_matrix(tiva_new_xgb)

confusion_matrix(vol_emr_xgb)  
confusion_matrix(vol_bp_xgb)
confusion_matrix(vol_record_xgb)
confusion_matrix(vol_red_xgb)
confusion_matrix(vol_new_xgb)

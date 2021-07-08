## Up + Down-sampling
library(randomForest)
library(xgboost)
library(tidyverse)
library(caret)
library(e1071)
library(dummies)

setwd("C:/Users/MY/Downloads")

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

rf_fit_updown = function(input_data){
  
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
    
    train_x <- x_shuf %>% select(-Class) 
    train_y <- x_shuf$Class
    
    test_x <- test %>% select(-class) 
    test_y <- test$class
    
    rf.fit = randomForest(Class ~ ., data=x, mtry=floor(sqrt(length(x)-1)), ntree=500, importance=T)
    
    y_pred = predict(rf.fit, test_x)
    
    conf_mat <- confusionMatrix(y_pred, test_y)
    conf_mat_list[[i]] <- conf_mat$table
  }
  
  return(conf_mat_list)
}

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

dnn_fit_updown = function(input_data){
  
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
    
    train_x <- x_shuf %>% 
      data.frame() %>% 
      select(-Class) %>% 
      as.matrix()
    
    #val_x <- val %>%
    #  data.frame() %>% 
    #  select(-Class) %>% 
    #  as.matrix()
    
    test_x <- test %>% 
      data.frame() %>% 
      select(-class) %>% 
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
    
    x_shuf$Class <- as.factor(x_shuf$Class)
    #val$Class <- as.factor(val$Class)
    test$Class <- as.factor(test$class)
    
    train_y <- x_shuf$Class %>% as.numeric()-1
    #val_y <- val$Class %>% as.numeric()-1
    test_y <- test$class %>% as.numeric()-1
    
    model <- keras_model_sequential()
    
    model %>% 
      layer_dense(units = 512, activation = 'relu', input_shape = ncol(x_shuf)-1) %>% 
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
    conf_mat_list[[i]] <- conf_mat$table
  }
  
  return(conf_mat_list)
}


# RF 결과 확인
tiva_emr_rf <- rf_fit_updown(tiva_emr)
tiva_bp_rf <- rf_fit_updown(tiva_bp)
tiva_record_rf <- rf_fit_updown(tiva_record)
tiva_red_rf <- rf_fit_updown(tiva_red)
tiva_new_rf <- rf_fit_updown(tiva_new)

vol_emr_rf <- rf_fit_updown(vol_emr)
vol_bp_rf <- rf_fit_updown(vol_bp)
vol_record_rf <- rf_fit_updown(vol_record)
vol_red_rf <- rf_fit_updown(vol_red)
vol_new_rf <- rf_fit_updown(vol_new)

confusion_matrix(tiva_emr_rf)  
confusion_matrix(tiva_bp_rf)
confusion_matrix(tiva_record_rf)
confusion_matrix(tiva_red_rf)
confusion_matrix(tiva_new_rf)

confusion_matrix(vol_emr_rf)  
confusion_matrix(vol_bp_rf)
confusion_matrix(vol_record_rf)
confusion_matrix(vol_red_rf)
confusion_matrix(vol_new_rf)

# XGBoost 결과 확인
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

# dnn 결과 확인
tiva_emr_dnn <- dnn_fit_updown(tiva_emr)
tiva_bp_dnn <- dnn_fit_updown(tiva_bp)
tiva_record_dnn <- dnn_fit_updown(tiva_record)
tiva_red_dnn <- dnn_fit_updown(tiva_red)
tiva_new_dnn <- dnn_fit_updown(tiva_new)

vol_emr_dnn <- dnn_fit_down(vol_emr)
vol_bp_dnn <- dnn_fit_down(vol_bp)
vol_record_dnn <- dnn_fit_down(vol_record)
vol_red_dnn <- dnn_fit_updown(vol_red)
vol_new_dnn <- dnn_fit_updown(vol_new)

confusion_matrix(tiva_emr_dnn)  
confusion_matrix(tiva_bp_dnn)
confusion_matrix(tiva_record_dnn)
confusion_matrix(tiva_red_dnn)
confusion_matrix(tiva_new_dnn)

confusion_matrix(vol_emr_dnn)  
confusion_matrix(vol_bp_dnn)
confusion_matrix(vol_record_dnn)
confusion_matrix(vol_red_dnn)
confusion_matrix(vol_new_dnn)


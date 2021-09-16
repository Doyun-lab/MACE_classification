# 아직 완성되지 않은 App (version 0.0)
library(shiny)
library(tidyverse)
library(caret)
library(xgboost)
library(randomForest)
library(keras)

ui <- fluidPage(
  titlePanel(title = "Cardiovascular Anomaly Prediction"),
  sidebarPanel(
    fileInput(inputId = "file",
              label = "Please select csv file or txt file.",
              multiple = FALSE,
              buttonLabel = icon(name = "file"),
              placeholder = "No files have been selected yet."),
    
    radioButtons(input = 'sep',
                 label = 'Please select a separator.',
                 choices = c('Comma' = ',', 'Semi-colon' = ';',
                             'Tab' = '\t', 'Space' = ' '),
                 selected = ',',
                 inline = TRUE),
    
    checkboxInput(inputId = "header",
                  label = "The first row is the header.",
                  value = T),
    
    selectInput(inputId = "var",
                label = "Please select a variable.",
                choices = NULL,
                multiple = T), 
    
    radioButtons(inputId = "algorithm",
                 label = "Select an algorithm.",
                 choices = c("XGBoost (Up-sampling)"="XGB_up",
                             "XGBoost (Down-sampling)"="XGB_down",
                             "XGBoost (Up + Down-sampling)"="XGB_updown",
                             "Random Forest (Up-sampling)"="RF_up",
                             "Random Forest (Down-sampling)"="RF_down",
                             "Random Forest (Up + Down-sampling)"="RF_updown",
                             "DNN (Up-sampling)"="DNN_up",
                             "DNN (Down-sampling)"="DNN_down",
                             "DNN (Up + Down-sampling)"="DNN_updown")),
    
    submitButton(text = "Applying Changes",
                 icon = icon(name = "sync"))
  ),
  
  mainPanel(
    uiOutput(outputId = "mainUI")
  )
)

server <- function(input, output, session){
  df <- reactive({
    if(is.null(x = input$file)) return()
    read.csv(file = input$file$datapath, header = input$header,
             sep = input$sep, stringsAsFactors = TRUE)
  })
  
  observe({
    cols <- colnames(x=df())
    updateSelectInput(session = session, inputId = "var", choices = cols)
  })
  
  output$xgb_up <- renderPrint({
    if(is.null(x=df())) {
      return()
      
    }else{
      library(xgboost)
      library(caret)
      library(e1071)
      cat(" Please wait ...\n")
      
      xgb_data <- df()[c(input$var,"class")]
      
      set.seed(234)
      
      tiva_pc <- xgb_data
      tiva_pc$class <- ifelse(tiva_pc$class == "TRUE", 0, 1)
      tiva_pc$class <- as.factor(tiva_pc$class)
      
      cv_list <- createFolds(tiva_pc$class, k = 5)
      inTest <- cv_list[[4]]
      
      train <- tiva_pc[-inTest,]
      test <- tiva_pc[inTest,]
      
      up <- upSample(subset(train, select=-class), train$class)
      
      train_x <- up %>% select(-Class) %>% data.matrix
      train_y <- up$Class
      
      test_x <- test %>% select(-class) %>% data.matrix
      test_y <- test$class
      
      xgb.fit = xgboost(data = train_x, label = as.numeric(train_y)-1, eta = 0.1,
                        gamma = 0, nrounds = 1000, objective = "binary:logistic")
      
      y_pred_xgb = predict(xgb.fit, test_x)
      prediction_xgb = as.numeric(y_pred_xgb > 0.5)
      prediction_xgb = as.factor(prediction_xgb)
      
      conf_mat <- confusionMatrix(prediction_xgb, test_y)$table
      precision <- conf_mat[4]/(conf_mat[4]+conf_mat[3])
      recall <- conf_mat[4]/(conf_mat[4]+conf_mat[2])
      f1 <- 2*(precision*recall)/(precision+recall)
      
      precision_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[2])
      recall_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[3])
      f1_normal <- 2*(precision_normal*recall_normal)/(precision_normal+recall_normal)
      
      xgb_up <- data.frame(precision=precision, recall=recall, f1=f1)
      xgb_up_normal <- data.frame(precision=precision_normal, recall=recall_normal, f1=f1_normal)
      
      cat("    - XGBoost (Up-sampling) result -     \n\n")
      cat("\n\n     - Confusion Matrix - \n")
      print(xgb_up)
    }
  })
  
  output$xgb_down <- renderPrint({
    if(is.null(x=df())) {
      return()
      
    }else{
      library(xgboost)
      library(caret)
      library(e1071)
      cat(" Please wait ...\n")
      
      xgb_data <- df()[c(input$var,"class")]
      
      set.seed(234)
      
      tiva_pc <- xgb_data
      tiva_pc$class <- ifelse(tiva_pc$class == "TRUE", 0, 1)
      tiva_pc$class <- as.factor(tiva_pc$class)
      
      cv_list <- createFolds(tiva_pc$class, k = 5)
      inTest <- cv_list[[4]]
      
      train <- tiva_pc[-inTest,]
      test <- tiva_pc[inTest,]
      
      up <- downSample(subset(train, select=-class), train$class)
      
      train_x <- up %>% select(-Class) %>% data.matrix
      train_y <- up$Class
      
      test_x <- test %>% select(-class) %>% data.matrix
      test_y <- test$class
      
      xgb.fit = xgboost(data = train_x, label = as.numeric(train_y)-1, eta = 0.1,
                        gamma = 0, nrounds = 1000, objective = "binary:logistic")
      
      y_pred_xgb = predict(xgb.fit, test_x)
      prediction_xgb = as.numeric(y_pred_xgb > 0.5)
      prediction_xgb = as.factor(prediction_xgb)
      
      conf_mat <- confusionMatrix(prediction_xgb, test_y)$table
      precision <- conf_mat[4]/(conf_mat[4]+conf_mat[3])
      recall <- conf_mat[4]/(conf_mat[4]+conf_mat[2])
      f1 <- 2*(precision*recall)/(precision+recall)
      
      precision_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[2])
      recall_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[3])
      f1_normal <- 2*(precision_normal*recall_normal)/(precision_normal+recall_normal)
      
      xgb_down <- data.frame(precision=precision, recall=recall, f1=f1)
      xgb_down_normal <- data.frame(precision=precision_normal, recall=recall_normal, f1=f1_normal)
      
      cat("    - XGBoost (Down-sampling) result -     \n\n")
      cat("\n\n     - Confusion Matrix - \n")
      print(xgb_down)
    }
  })
  
  output$xgb_updown <- renderPrint({
    if(is.null(x=df())) {
      return()
      
    }else{
      library(xgboost)
      library(caret)
      library(e1071)
      cat(" Please wait ...\n")
      
      xgb_data <- df()[c(input$var,"class")]
      
      set.seed(234)
      
      tiva_pc <- xgb_data
      tiva_pc$class <- ifelse(tiva_pc$class == "TRUE", 0, 1)
      tiva_pc$class <- as.factor(tiva_pc$class)
      
      cv_list <- createFolds(tiva_pc$class, k = 5)
      inTest <- cv_list[[4]]
      
      train <- tiva_pc[-inTest,]
      test <- tiva_pc[inTest,]
      
      data_true <- subset(train, train$class == 0)
      data_false <- subset(train, train$class == 1)
      
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
      
      xgb.fit = xgboost(data = train_x, label = as.numeric(train_y)-1, eta = 0.1,
                        gamma = 0, nrounds = 1000, objective = "binary:logistic")
      
      y_pred_xgb = predict(xgb.fit, test_x)
      prediction_xgb = as.numeric(y_pred_xgb > 0.5)
      prediction_xgb = as.factor(prediction_xgb)
      
      conf_mat <- confusionMatrix(prediction_xgb, test_y)$table
      precision <- conf_mat[4]/(conf_mat[4]+conf_mat[3])
      recall <- conf_mat[4]/(conf_mat[4]+conf_mat[2])
      f1 <- 2*(precision*recall)/(precision+recall)
      
      precision_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[2])
      recall_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[3])
      f1_normal <- 2*(precision_normal*recall_normal)/(precision_normal+recall_normal)
      
      xgb_updown <- data.frame(precision=precision, recall=recall, f1=f1)
      xgb_updown_normal <- data.frame(precision=precision_normal, recall=recall_normal, f1=f1_normal)
      
      cat("    - XGBoost (Up + Down-sampling) result -     \n\n")
      cat("\n\n     - Confusion Matrix - \n")
      print(xgb_updown)
    }
  })
  
  output$rf_up <- renderPrint({
    if(is.null(x=df())) {
      return()
      
    }else{
      library(randomForest)
      library(caret)
      library(e1071)
      cat(" Please wait ...\n")
      
      rf_data <- df()[c(input$var,"class")]
      
      set.seed(234)
      
      tiva_pc <- rf_data
      tiva_pc$class <- ifelse(tiva_pc$class == "TRUE", 0, 1)
      tiva_pc$class <- as.factor(tiva_pc$class)
      
      cv_list <- createFolds(tiva_pc$class, k = 5)
      inTest <- cv_list[[4]]
      
      train <- tiva_pc[-inTest,]
      test <- tiva_pc[inTest,]
      
      up <- upSample(subset(train, select=-class), train$class)
      
      train_x <- up %>% select(-Class) 
      train_y <- up$Class
      
      test_x <- test %>% select(-class) 
      test_y <- test$class
      
      rf.fit = randomForest(Class ~ ., data=up, mtry=floor(sqrt(length(up)-1)), ntree=500, importance=T)
      
      y_pred_rf = predict(rf.fit, newdata = test_x, type = 'prob')[,2]
      prediction_rf = as.numeric(y_pred_rf > 0.5)
      prediction_rf = as.factor(prediction_rf)
      
      conf_mat <- confusionMatrix(prediction_rf, test_y)$table
      precision <- conf_mat[4]/(conf_mat[4]+conf_mat[3])
      recall <- conf_mat[4]/(conf_mat[4]+conf_mat[2])
      f1 <- 2*(precision*recall)/(precision+recall)
      
      precision_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[2])
      recall_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[3])
      f1_normal <- 2*(precision_normal*recall_normal)/(precision_normal+recall_normal)
      
      rf_up <- data.frame(precision=precision, recall=recall, f1=f1)
      rf_up_normal <- data.frame(precision=precision_normal, recall=recall_normal, f1=f1_normal)
      
      cat("    - RF (Up-sampling) result -     \n\n")
      cat("\n\n     - Confusion Matrix - \n")
      print(rf_up)
    }
  })
  
  output$rf_down <- renderPrint({
    if(is.null(x=df())) {
      return()
      
    }else{
      library(randomForest)
      library(caret)
      library(e1071)
      cat(" Please wait ...\n")
      
      rf_data <- df()[c(input$var,"class")]
      
      set.seed(234)
      
      tiva_pc <- rf_data
      tiva_pc$class <- ifelse(tiva_pc$class == "TRUE", 0, 1)
      tiva_pc$class <- as.factor(tiva_pc$class)
      
      cv_list <- createFolds(tiva_pc$class, k = 5)
      inTest <- cv_list[[4]]
      
      train <- tiva_pc[-inTest,]
      test <- tiva_pc[inTest,]
      
      up <- downSample(subset(train, select=-class), train$class)
      
      train_x <- up %>% select(-Class) 
      train_y <- up$Class
      
      test_x <- test %>% select(-class) 
      test_y <- test$class
      
      rf.fit = randomForest(Class ~ ., data=up, mtry=floor(sqrt(length(up)-1)), ntree=500, importance=T)
      
      y_pred_rf = predict(rf.fit, newdata = test_x, type = 'prob')[,2]
      prediction_rf = as.numeric(y_pred_rf > 0.5)
      prediction_rf = as.factor(prediction_rf)
      
      conf_mat <- confusionMatrix(prediction_rf, test_y)$table
      precision <- conf_mat[4]/(conf_mat[4]+conf_mat[3])
      recall <- conf_mat[4]/(conf_mat[4]+conf_mat[2])
      f1 <- 2*(precision*recall)/(precision+recall)
      
      precision_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[2])
      recall_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[3])
      f1_normal <- 2*(precision_normal*recall_normal)/(precision_normal+recall_normal)
      
      rf_down <- data.frame(precision=precision, recall=recall, f1=f1)
      rf_down_normal <- data.frame(precision=precision_normal, recall=recall_normal, f1=f1_normal)
      
      cat("    - RF (Down-sampling) result -     \n\n")
      cat("\n\n     - Confusion Matrix - \n")
      print(rf_down)
    }
  })
  
  output$rf_updown <- renderPrint({
    if(is.null(x=df())) {
      return()
      
    }else{
      library(randomForest)
      library(caret)
      library(e1071)
      cat(" Please wait ...\n")
      
      rf_data <- df()[c(input$var,"class")]
      
      set.seed(234)
      
      tiva_pc <- rf_data
      tiva_pc$class <- ifelse(tiva_pc$class == "TRUE", 0, 1)
      tiva_pc$class <- as.factor(tiva_pc$class)
      
      cv_list <- createFolds(tiva_pc$class, k = 5)
      inTest <- cv_list[[4]]
      
      train <- tiva_pc[-inTest,]
      test <- tiva_pc[inTest,]
      
      data_true <- subset(train, train$class == 0)
      data_false <- subset(train, train$class == 1)
      
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
      
      rf.fit = randomForest(Class ~ ., data=up, mtry=floor(sqrt(length(up)-1)), ntree=500, importance=T)
      
      y_pred_rf = predict(rf.fit, newdata = test_x, type = 'prob')[,2]
      prediction_rf = as.numeric(y_pred_rf > 0.5)
      prediction_rf = as.factor(prediction_rf)
      
      conf_mat <- confusionMatrix(prediction_rf, test_y)$table
      precision <- conf_mat[4]/(conf_mat[4]+conf_mat[3])
      recall <- conf_mat[4]/(conf_mat[4]+conf_mat[2])
      f1 <- 2*(precision*recall)/(precision+recall)
      
      precision_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[2])
      recall_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[3])
      f1_normal <- 2*(precision_normal*recall_normal)/(precision_normal+recall_normal)
      
      rf_updown <- data.frame(precision=precision, recall=recall, f1=f1)
      rf_updown_normal <- data.frame(precision=precision_normal, recall=recall_normal, f1=f1_normal)
      
      cat("    - RF (Up + Down-sampling) result -     \n\n")
      cat("\n\n     - Confusion Matrix - \n")
      print(rf_updown)
    }
  })
  
  
  output$dnn_up <- renderPrint({
    if(is.null(x=df())) {
      return()
      
    }else{
      library(keras)
      library(caret)
      library(e1071)
      cat(" Please wait ...\n\n")
      dnn_data <- df()[c(input$var,"class")]
      
      set.seed(234)
      
      tiva_pc <- dnn_data
      tiva_pc$class <- ifelse(tiva_pc$class == "TRUE", 0, 1)
      tiva_pc$class <- as.factor(tiva_pc$class)
      
      cv_list <- createFolds(tiva_pc$class, k = 5)
      inTest <- cv_list[[4]]
      
      train <- tiva_pc[-inTest,]
      test <- tiva_pc[inTest,]
      
      up <- upSample(subset(train, select=-class), train$class)
      
      train_x <- up %>% 
        data.frame() %>% 
        select(-Class) %>%
        as.matrix()
      
      test_x <- test %>% 
        data.frame() %>% 
        select(-class) %>% 
        as.matrix()
      
      train_x <- apply(train_x, 2, function(x){normalize(x)})
      test_x <- apply(test_x, 2, function(x){normalize(x)})
      
      train_x[is.na(train_x)] <- 0
      test_x[is.na(test_x)] <- 0
      
      train_y <- up$Class %>% as.numeric()-1
      test_y <- test$class %>% as.numeric()-1
      
      model <- keras_model_sequential()
      
      model %>% 
        layer_dense(units = 512, activation = 'relu', input_shape = ncol(train_x)) %>% 
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
      
      y_pred_dnn <- predict(model, test_x)
      prediction_dnn <- y_pred_dnn %>% round()
      prediction_dnn <- as.factor(prediction_dnn)
      
      conf_mat <- confusionMatrix(prediction_dnn, as.factor(test_y))$table
      precision <- conf_mat[4]/(conf_mat[4]+conf_mat[3])
      recall <- conf_mat[4]/(conf_mat[4]+conf_mat[2])
      f1 <- 2*(precision*recall)/(precision+recall)
      
      precision_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[2])
      recall_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[3])
      f1_normal <- 2*(precision_normal*recall_normal)/(precision_normal+recall_normal)
      
      dnn_up <- data.frame(precision=precision, recall=recall, f1=f1)
      dnn_up_normal <- data.frame(precision=precision_normal, recall=recall_normal, f1=f1_normal)
      
      cat("    - DNN (Up-sampling) result -     \n\n")
      cat("\n\n     - Confusion Matrix - \n")
      print(dnn_up)
    }
  })
  
  output$dnn_up <- renderPrint({
    if(is.null(x=df())) {
      return()
      
    }else{
      library(keras)
      library(caret)
      library(e1071)
      cat(" Please wait ...\n\n")
      dnn_data <- df()[c(input$var,"class")]
      
      set.seed(234)
      
      tiva_pc <- dnn_data
      tiva_pc$class <- ifelse(tiva_pc$class == "TRUE", 0, 1)
      tiva_pc$class <- as.factor(tiva_pc$class)
      
      cv_list <- createFolds(tiva_pc$class, k = 5)
      inTest <- cv_list[[4]]
      
      train <- tiva_pc[-inTest,]
      test <- tiva_pc[inTest,]
      
      up <- downSample(subset(train, select=-class), train$class)
      
      train_x <- up %>% 
        data.frame() %>% 
        select(-Class) %>%
        as.matrix()
      
      test_x <- test %>% 
        data.frame() %>% 
        select(-class) %>% 
        as.matrix()
      
      train_x <- apply(train_x, 2, function(x){normalize(x)})
      test_x <- apply(test_x, 2, function(x){normalize(x)})
      
      train_x[is.na(train_x)] <- 0
      test_x[is.na(test_x)] <- 0
      
      train_y <- up$Class %>% as.numeric()-1
      test_y <- test$class %>% as.numeric()-1
      
      model <- keras_model_sequential()
      
      model %>% 
        layer_dense(units = 512, activation = 'relu', input_shape = ncol(train_x)) %>% 
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
      
      y_pred_dnn <- predict(model, test_x)
      prediction_dnn <- y_pred_dnn %>% round()
      prediction_dnn <- as.factor(prediction_dnn)
      
      conf_mat <- confusionMatrix(prediction_dnn, as.factor(test_y))$table
      precision <- conf_mat[4]/(conf_mat[4]+conf_mat[3])
      recall <- conf_mat[4]/(conf_mat[4]+conf_mat[2])
      f1 <- 2*(precision*recall)/(precision+recall)
      
      precision_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[2])
      recall_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[3])
      f1_normal <- 2*(precision_normal*recall_normal)/(precision_normal+recall_normal)
      
      dnn_down <- data.frame(precision=precision, recall=recall, f1=f1)
      dnn_down_normal <- data.frame(precision=precision_normal, recall=recall_normal, f1=f1_normal)
      
      cat("    - DNN (Down-sampling) result -     \n\n")
      cat("\n\n     - Confusion Matrix - \n")
      print(dnn_down)
    }
  })
  
  output$dnn_updown <- renderPrint({
    if(is.null(x=df())) {
      return()
      
    }else{
      library(keras)
      library(caret)
      library(e1071)
      cat(" Please wait ...\n\n")
      dnn_data <- df()[c(input$var,"class")]
      
      set.seed(234)
      
      tiva_pc <- dnn_data
      tiva_pc$class <- ifelse(tiva_pc$class == "TRUE", 0, 1)
      tiva_pc$class <- as.factor(tiva_pc$class)
      
      cv_list <- createFolds(tiva_pc$class, k = 5)
      inTest <- cv_list[[4]]
      
      train <- tiva_pc[-inTest,]
      test <- tiva_pc[inTest,]
      
      data_true <- subset(train, train$class == 0)
      data_false <- subset(train, train$class == 1)
      
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
      
      test_x <- test %>% 
        data.frame() %>% 
        select(-class) %>% 
        as.matrix()
      
      
      train_x <- apply(train_x, 2, function(x){normalize(x)})
      test_x <- apply(test_x, 2, function(x){normalize(x)})
      
      train_x[is.na(train_x)] <- 0
      test_x[is.na(test_x)] <- 0
      
      train_y <- up$Class %>% as.numeric()-1
      test_y <- test$class %>% as.numeric()-1
      
      model <- keras_model_sequential()
      
      model %>% 
        layer_dense(units = 512, activation = 'relu', input_shape = ncol(train_x)) %>% 
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
      
      y_pred_dnn <- predict(model, test_x)
      prediction_dnn <- y_pred_dnn %>% round()
      prediction_dnn <- as.factor(prediction_dnn)
      
      conf_mat <- confusionMatrix(prediction_dnn, as.factor(test_y))$table
      precision <- conf_mat[4]/(conf_mat[4]+conf_mat[3])
      recall <- conf_mat[4]/(conf_mat[4]+conf_mat[2])
      f1 <- 2*(precision*recall)/(precision+recall)
      
      precision_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[2])
      recall_normal <- conf_mat[1]/(conf_mat[1]+conf_mat[3])
      f1_normal <- 2*(precision_normal*recall_normal)/(precision_normal+recall_normal)
      
      dnn_updown <- data.frame(precision=precision, recall=recall, f1=f1)
      dnn_updown_normal <- data.frame(precision=precision_normal, recall=recall_normal, f1=f1_normal)
      
      cat("    - DNN (Up + Down-sampling) result -     \n\n")
      cat("\n\n     - Confusion Matrix - \n")
      print(dnn_updown)
    }
  })
  
  
  output$table <- renderTable({
    if(is.null(x=df())) return() else df()[input$var] %>% head()
  })
  
  output$glimpse <- renderPrint({
    if(is.null(x=df())) return() else glimpse(x=df()[input$var])
  })
  
  output$mainUI <- renderUI({
    if(is.null(x=df())) h4("There is no content to display.")
    else tabsetPanel(
      tabPanel(title="Data",
               tableOutput(outputId = "table"), 
               verbatimTextOutput(outputId = "glimpse")),
      
      if(input$algorithm=="XGB_up"){
        tabPanel(title="XGBoost (Up-sampling)",
                 verbatimTextOutput(outputId = "xgb_up"))
      }
      else if(input$algorithm=="XGB_down"){
        tabPanel(title="XGBoost (Down-sampling)",
                 verbatimTextOutput(outputId = "xgb_down"))
      } 
      else if(input$algorithm=="XGB_updown"){
        tabPanel(title="XGBoost (Up + Down-sapling",
                 verbatimTextOutput(outputId = "xgb_updown"))
      } 
      else if(input$algorithm=="RF_up"){
        tabPanel(title="Random Forest (Up-sampling)",
                 verbatimTextOutput(outputId = "rf_up"))
      } 
      else if(input$algorithm=="RF_down"){
        tabPanel(title="Random Forest (Down-sampling)",
                 verbatimTextOutput(outputId = "rf_down"))
      } 
      else if(input$algorithm=="RF_updown"){
        tabPanel(title="Random Forest (Up + Down-sampling)",
                 verbatimTextOutput(outputId = "rf_updown"))
      } 
      else if(input$algorithm=="DNN_up"){
        tabPanel(title="DNN (Up-sampling)",
                 verbatimTextOutput(outputId = "dnn_up"))
      } 
      else if(input$algorithm=="DNN_down"){
        tabPanel(title="DNN (Down-sampling)",
                 verbatimTextOutput(outputId = "dnn_down"))
      } 
      else{
        tabPanel(title="DNN (Up + Down-sampling",
                 verbatimTextOutput(outputId = "dnn_updown"))
      }
    )
  })
}

shinyApp(ui, server)

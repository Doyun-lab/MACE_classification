d <- getwd()
fls <- dir(d, recursive = T)
fls_train <- fls[grep("train", fls)]
fls_test <- fls[grep("test", fls)]

i <- 1
imp_train <- list()
for (f in fls_train){
  path <- file.path(str_c(d, "/", f)) 
  temp <- read.csv(path)
  temp <- temp %>% select(-X)
  
  temp$Class <- as.factor(temp$Class)
  
  imp_train[[i]] <- temp
  i <- i + 1
}

i <- 1
imp_test <- list()
for (f in fls_test){
  path <- file.path(str_c(d, "/", f)) 
  temp <- read.csv(path)
  temp <- temp %>% select(-X)
  
  temp$Class <- as.factor(temp$Class)
  
  imp_test[[i]] <- temp
  i <- i + 1
}

train <- imp_train[[10]]
test <- imp_test[[10]]

train_x <- train %>% select(-Class) %>% data.matrix
train_y <- train$Class

test_x <- test %>% select(-Class) %>% data.matrix
test_y <- test$Class

xgb.fit = xgboost(data = train_x, label = as.numeric(train_y)-1, eta = 0.01, nrounds = 1000,
                  objective = "binary:logistic")

test_x <- test %>% select(-Class) 
test_y <- test$Class

rf.fit = randomForest(Class ~ ., data=train, mtry=floor(sqrt(length(train)-1)), ntree=500, importance=T)

# Importance var
imp = xgb.importance(model = xgb.fit)
xgb.plot.importance(imp)

varImpPlot(rf.fit)

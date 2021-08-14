library(caret)
library(xgboost)
library(PRROC)
library(MLeval)
library(MLmetrics)
library(yardstick)
library(tidymodels)

set.seed(234)

tiva_temp <- tiva_3
tiva_temp$class <- ifelse(tiva_temp$class == "TRUE", 0, 1)
tiva_temp$class <- as.factor(tiva_temp$class)

inTrain <- createDataPartition(tiva_temp$class, p = 0.8, list = FALSE)

train <- tiva_temp[inTrain,]
test <- tiva_temp[-inTrain,]

up <- upSample(subset(train, select=-class), train$class)

train_x <- up %>% select(-Class) %>% data.matrix
train_y <- up$Class

test_x <- test %>% select(-class) %>% data.matrix
test_y <- test$class

xgb.fit = xgboost(data = train_x, label = as.numeric(train_y)-1, eta = 0.01, nrounds = 1000,
                  objective = "binary:logistic")

y_pred_xgb = predict(xgb.fit, test_x)
prediction_xgb = as.numeric(y_pred_xgb > 0.5)
prediction_xgb = as.factor(prediction_xgb)

conf_mat <- confusionMatrix(prediction_xgb, test_y)

pr <- pr.curve(y_pred_xgb0, y_pred_xgb1, curve = T)
plot(pr)

y <- ifelse(test_y == 0, "negative", "positive")

dat <- data.frame(true_class = y, score = y_pred_xgb)
dat <- dat %>%
  mutate(pred_class = if_else(score >= 0.2, "positive", "negative")) %>%
  mutate_at(vars(true_class, pred_class), list(. %>% factor() %>% forcats::fct_relevel("positive")))

dat %>%
  sens(true_class, pred_class)

pr_dat <- dat %>%
  pr_curve(true_class, score)

pr_dat %>%
  arrange(.threshold) %>% 
  ggplot() +
  geom_path(aes(recall, precision)) + 
  coord_equal()

dat %>%
  pr_auc(true_class, score)

#
library(precrec)
sscurves <- evalmod(scores = y_pred_xgb, labels = y, cb_alpha = 0.05)
autoplot(sscurves, "PRC")

smmdat1 <- mmdata(y_pred_xgb, y)
smcurve <- evalmod(smmdat1)
mm_auc_ci <- auc_ci(smcurve, alpha=0.05, dtype='t')  

smcurves_cb1 <- evalmod(smmdat1, x_bins = 10, cb_alpha = 0.05)
autoplot(smcurves_cb1)

# Create bootstrapped scores
r1 <- 10
resampled_scores <- replicate(r1, sample(y_pred_xgb, replace=TRUE))

# Calculate curves (single model with multiple datasets)
smdat1 <- mmdata(resampled_scores, y, modnames=rep("m1", r1), dsids=1:r1)
smcurves1 <- evalmod(smdat1)
plot(smcurves1)
auc_ci(smcurves1)  

library(pROC)
r1 <- roc(Class ~ ., data=up, ci=T)
r1$Sex
plot(r1)


###
fitControl <- trainControl(method = "cv",summaryFunction=prSummary,
                           classProbs=T,savePredictions = T,verboseIter = F)

im_fit2 <- train(make.names(Class) ~ ., data = up, method = "xgbTree", metric = "AUC",
                 trControl = fitControl)

x <- evalm(im_fit2)
x$proc

pred <- predict(im_fit2, test_x)
pred <- ifelse(pred == "X0", "0", "1")
pred <- as.factor(pred)
confusionMatrix(pred, as.factor(test_y), positive = "0")

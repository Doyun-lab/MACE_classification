library(randomForest)
library(xgboost)
library(tidyverse)
library(caret)
library(e1071)
library(dummies)
library(ROSE)
library(keras)

setwd("C:/Users/MY/Desktop/research/MACE/data")

emr_tiva = readRDS("emr_TIVA_0629.rds") # 1701
emr_vol = readRDS("emr_vol_0629.rds") # 1508

tiva_record_medi = readRDS("TIVA_record_medi.rds") # 1658
vol_record_medi = readRDS("volatile_record_medi.rds") # 1508

tiva_record_cre = readRDS("tiva_record_cre.rds") # 1657
tiva_record_cpt = readRDS("tiva_record_cpt.rds") # 1657
tiva_record_sry = readRDS("tiva_record_summary.rds") # 1657

vol_record_cre = readRDS("volatile_record_cre.rds") # 1507
vol_record_cpt = readRDS("volatile_record_cpt.rds") # 1507
vol_record_sry = readRDS("volatile_record_summary.rds") # 1507

tiva_data = emr_tiva %>%
  inner_join(tiva_record_cre, by = "Case_ID") %>%
  inner_join(tiva_record_cpt, by = "Case_ID") %>%
  inner_join(tiva_record_sry, by = "Case_ID") %>%
  inner_join(tiva_record_medi, by = "Case_ID") %>%
  select(-Case_ID)

vol_data = emr_vol %>%
  inner_join(vol_record_cre, by = "Case_ID") %>%
  inner_join(vol_record_cpt, by = "Case_ID") %>%
  inner_join(vol_record_sry, by = "Case_ID") %>%
  inner_join(vol_record_medi, by = "Case_ID") %>%
  select(-Case_ID)

tiva_data$Sex = as.factor(tiva_data$Sex)
tiva_data$class = as.factor(tiva_data$class)

vol_data$Sex = as.factor(vol_data$Sex)
vol_data$class = as.factor(vol_data$class)

table(tiva_data$class)
table(vol_data$class)

up <- ROSE(class ~ ., data = tiva_data, seed = 234)$data
table(tiva_data$class)
table(up$class)


# xgb
tiva_xgb <- xgb_fit(tiva_data)
vol_xgb <- xgb_fit(vol_data)
confusion_matrix(tiva_xgb)
confusion_matrix(vol_xgb)

# rf
change_col_t <- colnames(tiva_data)[166:230]
change_col_t <- gsub("[[:punct:]]", "", change_col_t)
change_col_t <-  gsub(" ", "_", change_col_t)
change_col_t <- paste0("med_", change_col_t)
change_col_t <- gsub("㎎", "mg", change_col_t)
colnames(tiva_data)[166:230] <- change_col_t
tiva_rf <- rf_fit(tiva_data)
confusion_matrix(tiva_rf)

change_col_v <- colnames(vol_data)[166:244]
change_col_v <- gsub("[[:punct:]]", "", change_col_v)
change_col_v <-  gsub(" ", "_", change_col_v)
change_col_v <- paste0("med_", change_col_v)
change_col_v <- gsub("㎎", "mg", change_col_v)
colnames(vol_data)[166:244] <- change_col_v
vol_rf <- rf_fit(vol_data)
confusion_matrix(vol_rf)

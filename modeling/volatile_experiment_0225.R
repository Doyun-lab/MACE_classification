library(tidyverse)

setwd("C:\\Users\\MY\\Desktop\\research\\MACE\\data")

# --------------------------------------------------------------------------------------------------------------------------------
emr_volatile = readRDS("emr_volatile_0205.rds")
nrow(emr_volatile)

emr_volatile = subset(emr_volatile, emr_volatile$BUN.1 < 499)
nrow(emr_volatile)

names(emr_volatile)[1] = "Case_ID"

emr_volatile$PFT1_inspect = ifelse(emr_volatile$PFT_1==0,0,1)
emr_volatile$PFT2_inspect = ifelse(emr_volatile$PFT_2==0,0,1)
emr_volatile$PFT3_inspect = ifelse(emr_volatile$PFT_3==0,0,1)
emr_volatile$PFT_inspect = NULL

emr_volatile$PFT1_inspect = as.factor(emr_volatile$PFT1_inspect)
emr_volatile$PFT2_inspect = as.factor(emr_volatile$PFT2_inspect)
emr_volatile$PFT3_inspect = as.factor(emr_volatile$PFT3_inspect)

volatile_summary = readRDS("volatile_summary_0225.rds")
volatile_cpt = readRDS("volatile_cpt_0225.rds")
volatile_peak = readRDS("volatile_peak_mpd10_0225.rds")
volatile_cre = readRDS("volatile_cre_0225.rds")
volatile_freq = readRDS("volatile_freq_0225.rds")

names(volatile_freq)[1] = "Case_ID"

volatile_rc_summary = readRDS("volatile_record_summary.rds")
volatile_rc_cpt = readRDS("volatile_record_cpt.rds")
volatile_rc_cre = readRDS("volatile_record_cre.rds")

# --------------------------------------------------------------------------------------------------------------------------------

# 1
emr_ans = emr_volatile %>%
  inner_join(volatile_rc_summary, by ="Case_ID") %>%
  inner_join(volatile_rc_cpt, by = "Case_ID") %>%
  inner_join(volatile_rc_cre, by = "Case_ID")
nrow(emr_ans)
table(emr_ans$class)

# 2
except_col <- sort(c(grep("SBP", colnames(emr_ans)), grep("MBP", colnames(emr_ans)), grep("DBP", colnames(emr_ans))))
emr_ans_exceptBP <- emr_ans[,-except_col]
nrow(emr_ans_exceptBP)
table(emr_ans_exceptBP$class)


# 3
emr_ans_vital = emr_ans_exceptBP %>%
  inner_join(volatile_summary, by ="Case_ID") %>%
  inner_join(volatile_cpt, by = "Case_ID") %>%
  inner_join(volatile_peak, by = "Case_ID") %>%
  inner_join(volatile_cre, by ="Case_ID") %>%
  inner_join(volatile_freq, by = "Case_ID")

nrow(emr_ans_vital)
table(emr_ans_vital$class)

# 4
emr_ans_vital2 = emr_ans_exceptBP %>%
  inner_join(volatile_summary, by ="Case_ID") %>%
  inner_join(volatile_freq, by = "Case_ID")

# --------------------------------------------------------------------------------------------------------------------------------

library(e1071)
library(caret)
library(NLP)
library(tm)
library(dplyr)
library(dummies)
library(stringr)
library(readxl)

# 1
model(emr_ans)
model_mace(emr_ans)

model_kfold(emr_ans)
model_kfold_mace(emr_ans)

# 2
model(emr_ans_exceptBP)
model_mace(emr_ans_exceptBP)

model_kfold(emr_ans_exceptBP)
model_kfold_mace(emr_ans_exceptBP)

# 3
model(emr_ans_vital)
model_mace(emr_ans_vital)

model_kfold(emr_ans_vital)
model_kfold_mace(emr_ans_vital)

# 4
model(emr_ans_vital2)
model_mace(emr_ans_vital2)

model_kfold(emr_ans_vital2)
model_kfold_mace(emr_ans_vital2)


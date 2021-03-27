library(tidyverse)

setwd("C:\\Users\\MY\\Desktop\\research\\MACE\\data")

# --------------------------------------------------------------------------------------------------------------------------------
emr_TIVA = readRDS("emr_TIVA_0205_op_revise.rds")
emr_TIVA = as.data.frame(emr_TIVA)
nrow(emr_TIVA)

emr_TIVA$PFT1_inspect = ifelse(emr_TIVA$PFT_1==0,0,1)
emr_TIVA$PFT2_inspect = ifelse(emr_TIVA$PFT_2==0,0,1)
emr_TIVA$PFT3_inspect = ifelse(emr_TIVA$PFT_3==0,0,1)
emr_TIVA$PFT_inspect = NULL

emr_TIVA$PFT1_inspect = as.factor(emr_TIVA$PFT1_inspect)
emr_TIVA$PFT2_inspect = as.factor(emr_TIVA$PFT2_inspect)
emr_TIVA$PFT3_inspect = as.factor(emr_TIVA$PFT3_inspect)

TIVA_summary = readRDS("TIVA_summary_0225.rds")
TIVA_cpt = readRDS("TIVA_cpt_0225.rds")
TIVA_peak = readRDS("TIVA_peak_mpd10_0225.rds")
TIVA_cre = readRDS("TIVA_cre_0225.rds")
TIVA_freq = readRDS("TIVA_freq_0225.rds")

TIVA_rc_summary = readRDS("TIVA_record_summary.rds")
TIVA_rc_cpt = readRDS("TIVA_record_cpt.rds")
TIVA_rc_cre = readRDS("TIVA_record_cre.rds")

# --------------------------------------------------------------------------------------------------------------------------------

# 1
emr_ans = emr_TIVA %>%
  inner_join(TIVA_rc_summary, by ="Case_ID") %>%
  inner_join(TIVA_rc_cpt, by = "Case_ID") %>%
  inner_join(TIVA_rc_cre, by = "Case_ID")
nrow(emr_ans)
table(emr_ans$class)

# 2
except_col <- sort(c(grep("SBP", colnames(emr_ans)), grep("MBP", colnames(emr_ans)), grep("DBP", colnames(emr_ans))))
emr_ans_exceptBP <- emr_ans[,-except_col]
nrow(emr_ans_exceptBP)
table(emr_ans_exceptBP$class)


# 3
emr_ans_vital = emr_ans_exceptBP %>%
  inner_join(TIVA_summary, by ="Case_ID") %>%
  inner_join(TIVA_cpt, by = "Case_ID") %>%
  inner_join(TIVA_peak, by = "Case_ID") %>%
  inner_join(TIVA_cre, by ="Case_ID") %>%
  inner_join(TIVA_freq, by = "Case_ID")
nrow(emr_ans_vital)
table(emr_ans_vital$class)

# 4
emr_ans_vital2 = emr_ans_exceptBP %>%
  inner_join(TIVA_cpt, by ="Case_ID")

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


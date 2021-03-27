library(stringr)
library(dplyr)

setwd('E:\\')

vital_TIVA = readRDS('preprocessing\\vital_TIVA_NA_END_0225.rds')
vital_volatile = readRDS('preprocessing\\vital_volatile_NA_END_0225.rds')

# 수술시간이 5분 미만이라 제외한 케이스
vital_volatile = subset(vital_volatile, vital_volatile$Case_ID != "08_181012_1120")

nrow(vital_TIVA)
nrow(vital_volatile)

unique(vital_TIVA$Case_ID)
unique(vital_volatile$Case_ID)

TIVA_vital_5min <- data.frame()
for(case_id in unique(vital_TIVA$Case_ID)){
  
  data = data.frame(subset(vital_TIVA, vital_TIVA$Case_ID==case_id))
  
  data <- data[1:(nrow(data)-90),]
  
  TIVA_vital_5min = rbind(TIVA_vital_5min, data)
  
}

volatile_vital_5min <- data.frame()
for(case_id in unique(vital_volatile$Case_ID)){
  
  data = data.frame(subset(vital_volatile, vital_volatile$Case_ID==case_id))
  
  data <- data[1:(nrow(data)-90),]
  
  volatile_vital_5min = rbind(volatile_vital_5min, data)
  
}

nrow(TIVA_vital_5min)
nrow(volatile_vital_5min)

unique(TIVA_vital_5min$Case_ID)
unique(volatile_vital_5min$Case_ID)

saveRDS(TIVA_vital_5min, 'preprocessing\\vital_TIVA_ex5min_0225.rds')
saveRDS(volatile_vital_5min, 'preprocessing\\vital_volatile_ex5min_0225.rds')


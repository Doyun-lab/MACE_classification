library(stringr)
library(tidyverse)
library(signal)
library(fBasics)
library(seewave)
library(pracma)
library(dplyr)
library(data.table)
library(changepoint)
library(reshape)

setwd("C://Users//MY//Desktop//research//MACE//data")

record_tiva = readRDS("tiva_record_0520.rds") %>% select(-arecord_id)
record_vol = readRDS("vol_record_0520.rds") %>% select(-arecord_id)

record_tiva <- subset(record_tiva, record_tiva$date1 != "10007-06-11 04:3")
record_vol <- subset(record_vol, record_vol$date1 != "비고2 RA")


tiva_sum <- record_tiva %>% 
  group_by(Case_ID) %>%
  summarise(sum = n())


vol_sum <- record_vol %>% 
  group_by(Case_ID) %>%
  summarise(sum = n())

cnn_tiva <- subset(record_tiva, record_tiva$item == "SBP" | record_tiva$item == "DBP" | record_tiva$item == "MBP" |
                     record_tiva$item == "HR" | record_tiva$item == "SPO2")

emr_tiva = readRDS("emr_TIVA_0629.rds")
tiva_case <- unique(emr_tiva$Case_ID)

cnn_tiva_result <- list()
num <- 1
for(case in tiva_case){
  temp <- subset(cnn_tiva, cnn_tiva$Case_ID == case)
  
  temp2 <- temp[,2:3]
  
  temp_SBP <- subset(temp2, temp2$item == "SBP")
  temp_DBP <- subset(temp2, temp2$item == "DBP")
  temp_MBP <- subset(temp2, temp2$item == "MBP")
  temp_HR <- subset(temp2, temp2$item == "HR")
  temp_SPO2 <- subset(temp2, temp2$item == "SPO2")
  
  if (nrow(temp_SBP) >= 30){
    temp_SBP <- temp_SBP[1:30,]
  } else {
    while(nrow(temp_SBP) < 30){
      zero_SBP <- data.frame("SBP", 0)
      colnames(zero_SBP) <- c("item", "value1")
      temp_SBP <- rbind(temp_SBP, zero_SBP)
    }
  }
  
  if (nrow(temp_DBP) >= 30){
    temp_DBP <- temp_DBP[1:30,]
  } else {
    while(nrow(temp_DBP) < 30){
      zero_DBP <- data.frame("DBP", 0)
      colnames(zero_DBP) <- c("item", "value1")
      temp_DBP <- rbind(temp_DBP, zero_DBP)
    }
  }
  
  if (nrow(temp_MBP) >= 30){
    temp_MBP <- temp_MBP[1:30,]
  } else {
    while(nrow(temp_MBP) < 30){
      zero_MBP <- data.frame("MBP", 0)
      colnames(zero_MBP) <- c("item", "value1")
      temp_MBP <- rbind(temp_MBP, zero_MBP)
    }
  }
  
  if (nrow(temp_HR) >= 30){
    temp_HR <- temp_HR[1:30,]
  } else {
    while(nrow(temp_HR) < 30){
      zero_HR <- data.frame("HR", 0)
      colnames(zero_HR) <- c("item", "value1")
      temp_HR <- rbind(temp_HR, zero_HR)
    }
  }
  
  if (nrow(temp_SPO2) >= 30){
    temp_SPO2 <- temp_SPO2[1:30,]
  } else {
    while(nrow(temp_SPO2) < 30){
      zero_SPO2 <- data.frame("SPO2", 0)
      colnames(zero_SPO2) <- c("item", "value1")
      temp_SPO2 <- rbind(temp_SPO2, zero_SPO2)
    }
  }
  
  temp3 <- data.frame(SBP = c(temp_SBP$value1), DBP = c(temp_DBP$value1), MBP = c(temp_MBP$value1), 
                      HR = c(temp_HR$value1), SPO2 = c(temp_SPO2$value1))
  
  temp3$Case_ID <- case
  
  cnn_tiva_result[[num]] <- temp3
  num <- num + 1
}
cnn_tiva_result

emr_tiva = readRDS("emr_TIVA_0629.rds")
emr_tiva <- data.frame(subset(emr_tiva, emr_tiva$Case_ID %in% unique(cnn_tiva$Case_ID)))

tiva_class <- emr_tiva %>% select(Case_ID, class)

cnn_tiva_result2 <- list()
num <- 1
cnn_tiva_result_ad <- cnn_tiva_result[-48]
cnn_tiva_result_ad2 <- cnn_tiva_result_ad[-1298]
cnn_tiva_result_ad3 <- cnn_tiva_result_ad2[-1251]
cnn_tiva_result_ad4 <- cnn_tiva_result_ad3[-1317]
cnn_tiva_result_ad5 <- cnn_tiva_result_ad4[1:1641]
for(list in cnn_tiva_result_ad5){
  temp <- subset(tiva_class, tiva_class$Case_ID == list$Case_ID[1])
  
  temp2 <- list
  temp2$class <- temp$class
  
  cnn_tiva_result2[[num]] <- temp2
  num <- num + 1
}

for(list in cnn_tiva_result2){
  temp <- list
  temp2 <- list[,1:5]
  write.csv(temp2, paste0("C://Users//MY//Desktop//research//MACE//cnn_data_tiva//", temp$Case_ID[1], ".csv"))
  write.csv(temp$class[1], paste0("C://Users//MY//Desktop//research//MACE//cnn_label_tiva//", temp$Case_ID[1], "_label.csv"))
}

cnn_label <- data.frame()
for(list in cnn_tiva_result2){
  cnn_label <- rbind(cnn_label, list$class[1])
}

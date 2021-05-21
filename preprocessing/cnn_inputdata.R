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

temp <- subset(cnn_tiva, cnn_tiva$Case_ID == "01_180906_1025")
temp2 <- temp[,2:3]

temp_SBP <- subset(temp2, temp2$item == "SBP")
temp_DBP <- subset(temp2, temp2$item == "DBP")
temp_MBP <- subset(temp2, temp2$item == "MBP")
temp_HR <- subset(temp2, temp2$item == "HR")
temp_SPO2 <- subset(temp2, temp2$item == "SPO2")

data.frame(SBP = c(temp_SBP$value1), DBP = c(temp_DBP$value1), MBP = c(temp_MBP$value1),
           HR = c(temp_HR$value1), SPO2 = c(temp_SPO2$value1))

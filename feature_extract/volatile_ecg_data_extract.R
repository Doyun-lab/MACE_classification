setwd("E:/")

library(signal)
library(fBasics)
library(seewave)
library(pracma)
library(stringr)
library(dplyr)

emr_vol = readRDS('preprocessing/emr_volatile.rds')

names(emr_vol)[1] = "Case_ID"

# vital data load
file_list = list.files("seoul_ECG_pleth")
tail(file_list,20)
length(file_list)

file_0size = c("03_190228_1525.csv","06_190328_0830.csv","09_180716_125648.csv","10_180910_073244.csv","11_180907_141628.csv","11_181018_100502.csv","13_190507_0735.csv","13_191204_0943.csv")

file_list = file_list[!file_list%in%file_0size]
length(file_list)

# wave의 case_id
file_id =  unlist(lapply(str_split(file_list,".csv"),function(x){x[1]}))
file_id = data.frame(file_id[file_id%in%file_id[!str_detect(file_id, "[a-z]")]])

names(file_id) = 'Case_ID'
tail(file_id,8)

file_id$Case_ID = as.character(file_id$Case_ID)

# emr과 겹치는 케이스
file_case_vol = inner_join(emr_vol,file_id,by='Case_ID') 
length(unique(file_case_vol$Case_ID))  # 528

# 폴더 생성 
dir.create("vol_wave")

# 파일 복사 
copy_file_list = paste0(file_case_vol$Case_ID,".csv")

for (file_list in copy_file_list){
  file.copy(paste0("seoul_ECG_pleth/",file_list),"vol_wave")
}


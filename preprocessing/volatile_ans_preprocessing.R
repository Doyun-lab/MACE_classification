setwd('E:\\')

library(stringr)
library(readxl)
library(dplyr)
library(data.table)
library(dlookr)
library(stringi)

# 마취기록지 data load
file_list = list.files("Record_1803_2004")

# vital record의 case_id 추출
file_id =  data.frame(unlist(lapply(str_split(file_list, ".txt.csv"), function(x){x[1]})))
names(file_id) = 'Case_ID'
file_id$Case_ID = as.character(file_id$Case_ID)

# volatile
emr_volatile = readRDS('preprocessing/emr_volatile.rds')
names(emr_volatile)[1] = "Case_ID"
file_case_volatile = inner_join(emr_volatile, file_id, by = 'Case_ID') 
length(unique(file_case_volatile$Case_ID))  # 1510

# ans data (volatile)
ans_volatile = data.table()
count = 0

system.time(
  
  for (file_name in file_case_volatile$Case_ID){
    
    df = fread(paste0('Record_1803_2004/', file_name, '.txt.csv'))
    
    df$Case_ID = file_name
    
    df$value2 <- as.character(df$value2)
    
    ans_volatile = bind_rows(ans_volatile, df) 
    
    count = count + 1
    
    if(count %% 10 == 0){
      print(count)}
    
  })

ans_volatile$value1 <- as.numeric(ans_volatile$value1)
ans_volatile_bp <- subset(ans_volatile, str_sub(ans_volatile$item, 1, 3) == "SBP" | str_sub(ans_volatile$item, 1, 3) == "DBP" | str_sub(ans_volatile$item, 1, 3) == "MBP")
summary(ans_volatile_bp)

ans_volatile_bp2 <- subset(ans_volatile_bp, ans_volatile_bp$value1 >= 20 & ans_volatile_bp$value1 <= 200)
summary(ans_volatile_bp2)

length(unique(ans_volatile_bp2$Case_ID))

saveRDS(ans_volatile_bp2, "volatile_ans_bp")

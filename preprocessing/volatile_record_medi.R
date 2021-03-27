library(stringr)
library(dplyr)
library(signal)
library(fBasics)
library(seewave)
library(pracma)
library(dplyr)
library(changepoint)

setwd('E:\\')

emr_vol = readRDS('preprocessing/emr_volatile_0205.rds')
names(emr_vol)[1] = 'Case_ID'
nrow(emr_vol)

file_list_record = list.files("Revise_Record_1803_2004")

# 06_200313_1133
file_list_record = file_list_record[!file_list_record=='06_200313_1133.txt.csv']
file_list_record

# vital record의 case_id 추출
file_id =  unlist(lapply(str_split(file_list_record,".txt"),function(x){x[1]}))
length(file_id)  # 3519

# volatile과 겹치는 마취 기록지 케이스 
vol_record_case = emr_vol$Case_ID[emr_vol$Case_ID%in%file_id]
length(vol_record_case)   # 1510 

# volatile
file_case_vol = file_id[file_id%in%emr_vol$Case_ID]
length(file_case_vol)  # 1507

# wave data load (volatile)
record_vol = data.table()
count = 0

system.time(
  
  for (file_name in file_case_vol){
    
    df = data.table(read.csv(paste0('Revise_Record_1803_2004/',file_name,'.txt.csv'))[,c('arecord_id','group','item','value1','date1','value2')])
    
    names(df)
    
    # df = df[group=='V/S',]
    # df$item = sapply(str_split(df$item, ' '), function(x){x[1]})
    
    df$Case_ID = file_name
    
    record_vol = rbind(record_vol,df) 
    
    count = count+1
    
    if(count%%10==0){
      print(count)}
    
  })

str(record_vol)
head(record_vol)
length(unique(record_vol$Case_ID))

summary(record_vol)

unique(record_vol$value1)
table(record_vol$group)



# ---------------------------------------------------------------------------------------------------------------------------------------------------------------

volatile_medi <- record_vol[group=="Drug" | group=="Fluid" | group=="Inhalational Agent" | group=="Intravenous Agent" | group=='Local Anesthetic']

# Case : 1507개
length(unique(volatile_medi$Case_ID))

# 약품명 개수 확인
length(unique(volatile_medi$item))

# TIVA 약품 추출 Dataframe 생성
record_medi_name <- unique(volatile_medi$item)
record_medi_name <- record_medi_name[-9]
record_medi_vol <- data.frame(record_medi_name)
record_medi_vol <- data.frame(t(record_medi_vol))
colnames(record_medi_vol) <- record_medi_name

zero_vec <- c()
for (i in 1:length(record_medi_vol)){
  record_medi_vol[,i] <- 0
  zero_vec <- c(zero_vec, 0)
}

for (i in 1:length(unique(volatile_medi$Case_ID))){
  record_medi_vol <- rbind(record_medi_vol, zero_vec)
}
record_medi_vol <- record_medi_vol[1:length(vol_record_case),]

record_medi_vol$Case_ID <- vol_record_case
rownames(record_medi_vol) <- vol_record_case

# item에 있는 값과 열이름이 같을 때 그 열에 value1의 값을 넣는 코드를 짜기
volatile_medi$value1 <- as.numeric(volatile_medi$value1)
volatile_medi_use <- volatile_medi %>% 
  group_by(item, Case_ID) %>% 
  summarise(sum(value1)) %>%
  arrange(Case_ID)
volatile_medi_use <- na.omit(volatile_medi_use)

system.time(
  for (x in unique(volatile_medi_use$Case_ID)){
    for (y in rownames(record_medi_vol)){
      if (x == y){
        for (i in volatile_medi_use$item){
          for (j in colnames(record_medi_vol)){
            if (i == j){
              match_case <- subset(volatile_medi_use, volatile_medi_use$Case_ID == x)
              match_medi_name <- subset(match_case, match_case$item == i)
              if (nrow(match_medi_name) != 0){
                record_medi_vol[x, i] <- match_medi_name$`sum(value1)`
              }
            } 
          }
        }
      }
    }
    print(x)
  }
)
saveRDS(record_medi_vol, 'preprocessing/volatile_record_medi.rds')



library(stringr)
library(dplyr)

library(signal)
library(fBasics)
library(seewave)
library(pracma)
library(dplyr)

library(changepoint)

setwd('E:\\')

emr_TIVA = readRDS('preprocessing/emr_TIVA_0205.rds')
names(emr_TIVA)[1] = 'Case_ID'
nrow(emr_TIVA)

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

tiva_record_case = emr_TIVA$Case_ID[emr_TIVA$Case_ID%in%file_id]
length(tiva_record_case)   # 1668 # tiva와 겹치는 마취 기록지 케이스 

vol_record_case = emr_vol$Case_ID[emr_vol$Case_ID%in%file_id]
length(vol_record_case)   # 1510 # tiva와 겹치는 마취 기록지 케이스 

names(file_id) = 'Case_ID'
tail(file_id,8)

file_id$Case_ID = as.character(file_id$Case_ID)

# tiva
file_case_TIVA = file_id[file_id%in%emr_TIVA$Case_ID]
length(file_case_TIVA)  # 1662

# tiva
file_case_vol = file_id[file_id%in%emr_vol$Case_ID]
length(file_case_vol)  # 1508

# wave data load(TIVA)
record_TIVA = data.table()
count = 0

system.time(
  
  for (file_name in file_case_TIVA){
    
    df = data.table(read.csv(paste0('Revise_Record_1803_2004/',file_name,'.txt.csv'))[,c('arecord_id','group','item','value1','date1')])
    
    names(df)
    
    df = df[group=='V/S',]
    df$item = sapply(str_split(df$item, ' '), function(x){x[1]})
    
    df$Case_ID = file_name
    
    record_TIVA = bind_rows(record_TIVA,df) 
    
    count = count+1
    
    if(count%%10==0){
      print(count)}
    
  })

str(record_TIVA)
head(record_TIVA)
length(unique(record_TIVA$Case_ID))

# wave data load(TIVA)
record_vol = data.table()
count = 0

system.time(
  
  for (file_name in file_case_vol){
    
    df = data.table(read.csv(paste0('Revise_Record_1803_2004/',file_name,'.txt.csv'))[,c('arecord_id','group','item','value1','date1')])
    
    names(df)
    
    df = df[group=='V/S',]
    df$item = sapply(str_split(df$item, ' '), function(x){x[1]})
    
    df$Case_ID = file_name
    
    record_vol = bind_rows(record_vol,df) 
    
    count = count+1
    
    if(count%%10==0){
      print(count)}
    
  })

str(record_vol)
head(record_vol)
length(unique(record_vol$Case_ID))
summary(record_vol)
unique(record_vol$value1)

record_vol$value1 = as.numeric(record_vol$value1)

item_name = unique(record_vol[group=="V/S",item])[c(1:3,5,6,8,9)]

vol_data = record_vol[item%in%item_name,]
unique(vol_data$item)
unique(vol_data$value1)

# unique(vol_data[value1=="LA"|value1=="RL"|value1=="RR"|value1=="LL",item])
# vol_data[value1=="LA"|value1=="RL"|value1=="RR"|value1=="LL",date1]
# unique(vol_data[value1=="LA"|value1=="RL"|value1=="RR"|value1=="LL",Case_ID])  # 시간이 이상함. 

vol_data_da_revise = vol_data[date1!="10007-06-11 04:3"]
unique(vol_data_da_revise$value1)

vol_data_da_revise[value1==".",]  # 1
vol_data_da_revise[value1=="-",]  # 13

unique(record_check_data[,value1])

vol_data_da_revise$value1 = as.numeric(vol_data_da_revise$value1)
unique(vol_data_da_revise$value1)
table(is.na(vol_data_da_revise$value1))  

# 평균값으로 대체 
for(i in 1:nrow(vol_data_da_revise)){
  if(is.na(vol_data_da_revise$value1[i])){
    if(is.na(vol_data_da_revise$value1[i+1])){
      vol_data_da_revise$value1[i] = mean(vol_data_da_revise$value1[i-1],vol_data_da_revise$value1[i+2]) }
    else{
      vol_data_da_revise$value1[i] = mean(vol_data_da_revise$value1[i-1],vol_data_da_revise$value1[i+1])
    }
  }
}

table(is.na(vol_data_da_revise$value1))
str(vol_data_da_revise)

item_name_list = unique(vol_data_da_revise$item)
item_name_list

# 특징 추출 
rss = function(x) rms(x)*sqrt(length(x))
volatile_summary = data.frame()

for(case in unique(vol_data_da_revise$Case_ID)){ 
  
  data_sum = c()
  
  for(item_name in item_name_list){
  
  
    data = vol_data_da_revise[item == item_name&Case_ID == case,]
  

    if(nrow(data)==0){
      
      data_summmary = data.frame(mean = 0, sd = 0, rms = 0, rss = 0, IQR = 0)
    }
    
    else{
      
      data_summary = data.frame(data$value1) %>% summarise_each(funs(mean, sd, rms, rss, IQR))
    }
    
    names(data_summary) = paste0(item_name,"_",names(data_summary))
    print(data_summary)
    
    data_sum = append(data_sum, unlist(data_summary))
  }
  
  volatile_summary = rbind(volatile_summary,data.frame(matrix(data_sum,nrow=1),Case_ID = case))
}

names(volatile_summary) = c(names(data_sum),"Case_ID")
length(volatile_summary$Case_ID)  # 1508
saveRDS(volatile_summary,"preprocessing/volatile_record_summary.rds")

#######################################################
#----------------변화하는 구간 추출--------------------
#######################################################

cpt_f = function(data){
  
  cpt_mean = cpts(cpt.mean(data))
  cpt_var = cpts(cpt.var(data))
  cpt_meanvar = cpts(cpt.meanvar(data))
  
  if(length(cpt_var)==0){
    cpt_var = 0
  }
  
  if(length(cpt_mean)==0){
    cpt_mean = 0
  }
  
  if(length(cpt_meanvar)==0){
    cpt_meanvar = 0
  }
  
  cpts_data = c(cpt_var = cpt_var, cpt_mean = cpt_mean, cpt_meanvar = cpt_meanvar)
  
  return(cpts_data)
  
}

volatile_cpt = data.frame()
check = c()

for(case in unique(vol_data_da_revise$Case_ID)[1:5]){
  
  print(case)
  
  data = data.table(subset(vol_data_da_revise, vol_data_da_revise$Case_ID==case))
  
  data_cpt = c()
  
  for(item_name in item_name_list){

    if(length(data[item == item_name,value1])<4){  
      
      check = rbind(check,c(case, item_name))
      
      data_cpt_sum = data.frame(cpt_var = 0, cpt_mean = 0, cpt_meanvar = 0)
      
    }
    
    
    else{
      
      data_cpt_sum = data[item == item_name,value1] %>% cpt_f()
      names(data_cpt_sum) = paste0(item_name,"_",names(data_cpt_sum))
      names(data_cpt_sum)
      
    }
    
    data_cpt = append(data_cpt, unlist(data_cpt_sum))
  
  }
    volatile_cpt = rbind(volatile_cpt,data.frame(matrix(data_cpt,nrow=1),Case_ID = case))
  
}

names(volatile_cpt) = c(names(data_cpt),'case')
saveRDS(volatile_cpt, "preprocessing/volatile_record_cpt.rds")



#######################################################
#----------------파고율 특징 추출-----------------------
#######################################################
volatile_cre = data.frame()

for(case in unique(vol_data_da_revise$Case_ID)){
  
  data = data.table(subset(vol_data_da_revise, vol_data_da_revise$Case_ID==case))
  
  data_cre = c()
  
  for(item_name in item_name_list){
    
    if(length(data[item == item_name,value1])==0){
      
      data_cre_sum = 0
    }
    
    else{
      
      data_cre_sum = crest(data[item == item_name, value1],1)$C
      
    }
    
    data_cre = append(data_cre, unlist(data_cre_sum))
    
  }
  
  volatile_cre = rbind(volatile_cre,data.frame(matrix(data_cre,nrow=1),Case_ID = case))
  
}

names(volatile_cre) = c(paste0(item_name_list,'_cre'),"Case_ID")
nrow(volatile_cre)
head(volatile_cre)
names(volatile_cre)

saveRDS(volatile_cre, "preprocessing/volatile_record_cre.rds")

library(data.table)
library(stringr)
library(stringi)
library(dplyr)

#파일별 결측치 확인 함수 정의 
# 파일별로 전체 결측치가 100% 인 것들의 수 파악 
a_100 = function(col){
  a=0
  for (i in col){
    if (i == 100)
      a = a + 1
  }
  return(a)
}

file_ENDTOEND_NA = function(data){
  
  vital_NA = data.frame()
  
  for (id in unique(data$Case_ID)){
    df = subset(data, data$Case_ID==id)
    
    df_na = cbind(data.frame(matrix(find_na(df,rate=TRUE),nrow = 1)))
    names(df_na) = names(df)
    df_na$Case_ID = id
    
    vital_NA = rbind(vital_NA,df_na) 
  }
  
  return(apply(vital_NA,2,a_100))
}

setwd("E:/")
emr_volatile = readRDS('preprocessing/emr_volatile.rds')
names(emr_volatile)[1] = "Case_ID"
nrow(emr_volatile) # 1514

# vital data load
file_list = list.files("seoul_1803_2012\\seoul_1803_2012")

# vital record의 case_id 추출
file_id =  data.frame(unlist(lapply(str_split(file_list,".csv_tag"),function(x){x[1]})))
names(file_id) = 'Case_ID'
file_id$Case_ID = as.character(file_id$Case_ID)

# volatile
file_case_volatile = inner_join(emr_volatile,file_id,by='Case_ID')  
nrow(file_case_volatile)  # 583

vital_volatile = data.frame()

system.time(
  
  for (file_id in file_case_volatile$Case_ID){
    
    df = fread(paste0('seoul_1803_2012\\seoul_1803_2012/',file_id,'.csv_tag_filled_del.csv'))
    
    df$Case_ID = file_id

    vital_volatile = bind_rows(vital_volatile, df) 
    
  })

str(vital_volatile)
# saveRDS(vital_volatile,'preprocessing\\vital_volatile.rds')

vital_volatile = readRDS("preprocessing\\vital_volatile.rds")
length(unique(vital_volatile$Case_ID)) # 546 -> 589 -> 583
names(vital_volatile)

except_cols = c("AMB_PRES","AGENT_NAME","MV","MAWP","PEEP_TOTAL","EVENT","Hypotension","Hypertension","BIS/SEF","BIS/SQI","BIS/EMG","BIS/TOTPOW","REMIFENTANIL_RATE","REMIFENTANIL_VOL","RR_TOTAL")
vital_volatile = vital_volatile[,!names(vital_volatile)%in%except_cols]

# optime 확인 -> 0 인 경우 없음. 
emr_volatile_data = emr_volatile%>%inner_join(vital_volatile,"Case_ID")
table(emr_volatile_data$OP.Time)  


# AGENT 관련 변수 결측치 처리 
# volatile preprocessing
vital_volatile = as.data.table(vital_volatile)
names(vital_volatile)

# AGENT에 기록이 안된 경우 삭제 
a = as.data.frame(vital_volatile)%>%select(Case_ID,AGENT_ET, AGENT_FI, AGENT_MAC)%>%group_by(Case_ID)%>%summarise_all(funs(sum))

a[is.na(a)] = 0
a$check = apply(a[,c(2:4)],1,sum)

case_except = a[a$check==0,]$Case_ID
length(case_except)   # 110개 -> 330개 

vital_volatile_total = vital_volatile[!vital_volatile$Case_ID%in%case_except,]
length(unique(vital_volatile_total$Case_ID)) # 436개 -> 253개

# 필요한 변수 추출 (프로포폴 변수 제외)
propofol_name = names(vital_volatile_total)[str_detect(names(vital_volatile_total), "PROPOFOL")]
vital_volatile_total = data.frame(vital_volatile_total)[,!names(vital_volatile_total)%in%propofol_name]
names(vital_volatile_total)

AGENT_RE_col = names(vital_volatile_total)[str_detect(names(vital_volatile_total),paste0(c("AGENT","REMIFENTANIL"),collapse = "|"))]
vital_volatile_total = data.frame(vital_volatile_total)
vital_volatile_total[,AGENT_RE_col][is.na(vital_volatile_total[,AGENT_RE_col])] = 0 
length(unique(vital_volatile_total$Case_ID)) # 255

names(vital_volatile_total)
find_na(vital_volatile_total,rate=TRUE)

# HR, SBP 이상치 
summary(vital_volatile_total$HR)
summary(vital_volatile_total$NIBP_SBP)

vital_volatile_total$HR = ifelse(vital_volatile_total$HR>200|vital_volatile_total$HR<20,NA,vital_volatile_total$HR)
vital_volatile_total$NIBP_SBP = ifelse(vital_volatile_total$NIBP_SBP>300|vital_volatile_total$NIBP_SBP<20,NA,vital_volatile_total$NIBP_SBP)

summary(vital_volatile_total$HR)
summary(vital_volatile_total$NIBP_SBP)

unique(vital_volatile_total$Case_ID)
length(unique(vital_volatile_total$Case_ID))


# 함수 적용(개수 반환)
NA_ENDTOEND = file_ENDTOEND_NA(vital_volatile_total)
NA_ENDTOEND

# BIS.BIS 
total_copy = data.table(vital_volatile_total)

bis_na_case = c()

for(case in unique(total_copy$Case_ID)){
  if(sum(is.na(total_copy[Case_ID==case,BIS.BIS]))/nrow(total_copy[Case_ID==case,])==1){
    bis_na_case = c(bis_na_case, case)
  }
}

length(bis_na_case)  # 147

file_list_record = list.files("Record_1803_2004")

# vital record의 case_id 추출
file_id =  unlist(lapply(str_split(file_list_record,".txt"),function(x){x[1]}))
length(file_id)  # 3519
vital_record_case = unique(vital_volatile_total$Case_ID)[unique(vital_volatile_total$Case_ID)%in%file_id]
length(vital_record_case)  

table(bis_na_case%in%file_id)  # 1건 빼고 모두 있음. 

volatile_record = data.frame()

system.time(
  
  for (file_id_record in vital_record_case){
    
    df = fread(paste0('Record_1803_2004/',file_id_record,'.txt.csv'))[,c("arecord_id", "group", "item", "value1", "date1")]
    
    df$Case_ID = file_id_record
    
    volatile_record = bind_rows(volatile_record,df) 
    
})

length(unique(volatile_record$Case_ID))  # 252

volatile_record = data.table(volatile_record)
item_bis = volatile_record[item=="BIS"]
length(unique(item_bis$Case_ID))  # 210건   # RR과 37건이 안겹침 

item_TV = volatile_record[item=="TV"]
length(unique(item_TV$Case_ID))  # 237 건

item_RR = volatile_record[item=="RR"]
length(unique(item_RR$Case_ID)) # 237건

# BIS 기준으로 코드 돌림. 
commom_case = unique(item_RR$Case_ID)[unique(item_RR$Case_ID)%in%unique(item_bis$Case_ID)]
length(commom_case)  # 200건 

head(vital_volatile_total$Time)
tail(vital_volatile_total$Time)


# 마취기록지에서 대체 
total_BIS = data.table()
total_TV = data.table()
total_RR = data.table()

count = 1

Sys.time()

for(case in commom_case){
  
  print(count)
  
  df = vital_volatile_total[Case_ID==case,c("Case_ID","Time","BIS.BIS","TV")]
  
  df_item_bis = item_bis[Case_ID == case,]
  df_item_TV = item_TV[Case_ID == case,]
  df_item_RR = item_RR[Case_ID == case,]
  
  df$RR = NA
  
  for (i in 1:nrow(df_item_RR)){
    
    for(j in 1:nrow(df)){
      
      if(df_item_RR[i,date1]<=df[j,Time]){
        
        df$RR[j] = df_item_RR$value1[i]
        
      }
    }
    
    if(is.na(df$RR[1])==TRUE){
      idx_na_end = which(is.na(df$RR))
      df[idx_na_end,]$RR = df[idx_na_end[length(idx_na_end)]+1,RR]
    }
    
  }
  
  total_RR = rbind(total_RR, df)
  
  if(sum(is.na(df$BIS.BIS))==nrow(df)){
    
    for (i in 1:nrow(df_item_bis)){
      
      for(j in 1:nrow(df)){
        
        if(df_item_bis[i,date1]<=df[j,Time]){
          
          df$BIS.BIS[j] = df_item_bis$value1[i]
          
        }
      }
    }
    
    if(is.na(df$BIS.BIS[1])==TRUE){
      idx_na_end = which(is.na(df$BIS.BIS))
      df[idx_na_end,]$BIS.BIS = df[idx_na_end[length(idx_na_end)]+1,BIS.BIS]
    }
    
    total_BIS = rbind(total_BIS, df)
    
  }
  
  else if (sum(is.na(df$BIS.BIS))!=nrow(df)) {
    total_BIS = rbind(total_BIS, df)
  }  
  
  if(sum(is.na(df$TV))==nrow(df)){
    
    for (i in 1:nrow(df_item_TV)){
      
      for(j in 1:nrow(df)){
        
        if(df_item_TV[i,date1]<=df[j,Time]){
          
          df$TV[j] = df_item_TV$value1[i]
          
        }
      }
    }
    
    if(is.na(df$TV[1])==TRUE){
      idx_na_end = which(is.na(df$TV))
      df[idx_na_end,]$TV = df[idx_na_end[length(idx_na_end)]+1,TV]
    }
    
    total_TV = rbind(total_TV, df)
    
  }
  
  else if (sum(is.na(df$TV))!=nrow(df)) {
    total_TV = rbind(total_TV, df)
  }
  
  count = count + 1
}

Sys.time()  # # 2시간 20분 소요 

nrow(total_BIS) 
colSums(is.na(total_BIS))
tail(total_BIS)
head(total_BIS)

head(item_bis)
nrow(total_TV)
nrow(total_RR)


total = cbind(total_BIS[,c("Case_ID","Time","BIS.BIS")],total_TV[,"TV"], total_RR[,"RR"])
length(unique(total$Case_ID))  # 200개
colSums(is.na(total))

# which(is.na(total$TV))
# total_BIS[367279,]
# nrow(total_BIS[Case_ID=='08_180910_1150',])
# table(is.na((total_BIS[Case_ID=='08_180910_1150',TV])))

# total_TV[Case_ID=='08_180910_1150',]
# item_TV[Case_ID=='08_180910_1150',]
# item_bis[Case_ID=='08_180910_1150',]

# saveRDS(total,"total_na_preprocessing.rds")
total = readRDS("total_na_preprocessing.rds")
colSums(is.na(total))


na_case = unique(total[is.na(BIS.BIS)]$Case_ID)
na_case = c(na_case,unique(total[is.na(RR)]$Case_ID),"08_180829_1310")

vital_record_data = data.table(vital_record_data)

# 마취기록지와 vital 레코드의 시간이 다른 경우
vital_time = data.table()

for(case in na_case){
  print(case)
  vital_time = rbind(vital_time, cbind(case,vital_start_time = vital_record_data[Case_ID==case,]$Time[1],vital_end_time = vital_record_data[Case_ID==case,]$Time[nrow(vital_record_data[Case_ID==case,])],record_start_time = item_RR[Case_ID==case,]$date1[1],record_end_time = item_RR[Case_ID==case,]$date1[nrow(item_RR[Case_ID==case,])]))

}

vital_time
tt = data.table(emr_volatile)[Case_ID%in%na_case,]
vital_time$MACE = tt$class
write.csv(vital_time, "vital_time.csv")
total = na.omit(total)

length(unique(total$Case_ID))  # 198건 

vital_record_data = subset(vital_volatile_total,vital_volatile_total$Case_ID%in%unique(total$Case_ID))
not_record_vital = subset(vital_volatile_total,!vital_volatile_total$Case_ID%in%unique(total$Case_ID))  # RR변수가 없으므로 사용 불가 


vital_time = c()
vital_record_data = data.table(vital_record_data)

for(case in unique(vital_record_data$Case_ID)){
  Time_record = vital_record_data[Case_ID==case,]$Time
  if(Time_record[1]>item_RR[Case_ID==case,]$date1[nrow(item_RR[Case_ID==case,])]){
    
    vital_time = rbind(vital_time, 
                       cbind(case,vital_start_time = Time_record[1],
                             vital_end_time = Time_record[length(Time_record)],
                             record_start_time = item_RR[Case_ID==case,]$date1[1],
                             record_end_time = item_RR[Case_ID==case,]$date1[nrow(item_RR[Case_ID==case,])],
                             diff = difftime(Time_record[length(Time_record)],item_RR[Case_ID==case,]$date1[1],units = 'mins')))
  }
}

vital_time

length(unique(vital_volatile_total$Case_ID)) # 253

length(unique(not_record_vital$Case_ID))
length(unique(vital_record_data$Case_ID))

# 같은 행인지 확인 
head(total)
head(vital_record_data)

# 값 대체 
vital_record_data$RR = total$RR
vital_record_data$TV = total$TV
vital_record_data$BIS.BIS = total$TV


# 200건 사용 가능 
colSums(is.na(vital_record_data))
find_na(vital_record_data,rate= TRUE)


# 파일별 처음부터 끝까지 결측치 - 확인 함수 적용(개수 반환)
NA_ENDTOEND_record = file_ENDTOEND_NA(vital_record_data)
except_100_col = names(vital_record_data)[round(NA_ENDTOEND_record/length(unique(vital_record_data$Case_ID)),2)!=0]

# 수정된 vital 데이터의 BT, CO2, NMT_TOF_CNT, PIP, COMPLIANCE 변수 제거 
vital_record_NACOL = vital_record_data[,!names(vital_record_data)%in%except_100_col]
names(vital_record_NACOL)

length(names(vital_record_NACOL))
length(names(vital_record_data))

# 특정 열들의 결측치 존재하는 행 제거 
# except_case_id_total = c()
# 
# for (col_ in except_100_col){
#   except_case_id = vital_record_NACOL$Case_ID[vital_record_NACOL[col_]==100]
#   except_case_id_total = c(except_case_id_total, except_case_id)
# }
# 
# except_case_id_total = unique(except_case_id_total)  # 228
# 
# vital_volatile_total = vital_volatile_total[!vital_volatile_total$Case_ID%in%except_case_id_total,]
# 
# length(unique(vital_volatile_total$Case_ID)) # 56

find_na(vital_record_NACOL,rate = TRUE)  # HR에 결측치 존재 

# HR 결측치 처리
library(zoo)

HR_NA = subset(vital_record_NACOL, is.na(vital_record_NACOL$HR))

HR_df = data.frame()

# HR의 앞뒤 결측치 처리 
for (id in unique(HR_NA$Case_ID)){
  
  df = subset(vital_record_NACOL, vital_record_NACOL$Case_ID == id)
  df2 = df[which(!is.na(df$HR))[1]:which(!is.na(df$HR))[length(which(!is.na(df$HR)))],]
  
  df2$HR =na.approx(df2$HR)
  
  HR_df = rbind(HR_df, df2) 
}

summary(HR_df$HR)

find_na(HR_df, rate = TRUE)

Total_vital_record_NACOL = rbind(vital_record_NACOL[!vital_record_NACOL$Case_ID%in%unique(HR_NA$Case_ID),],HR_df)
length(unique(Total_vital_record_NACOL$Case_ID))
find_na(Total_vital_record_NACOL,rate = TRUE)

saveRDS(Total_vital_record_NACOL,'preprocessing/Total_vital_record_NACOL_0121.rds')



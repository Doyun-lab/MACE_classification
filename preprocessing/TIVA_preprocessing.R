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



setwd('E:\\')

emr_TIVA = readRDS('preprocessing/emr_TIVA.rds')
names(emr_TIVA)[1] = 'Case_ID'

# vital data load
file_list = list.files("seoul_1803_2012\\seoul_1803_2012")

# vital record의 case_id 추출
file_id =  data.frame(unlist(lapply(str_split(file_list,".csv_tag"),function(x){x[1]})))
names(file_id) = 'Case_ID'
file_id$Case_ID = as.character(file_id$Case_ID)

# tiva
file_case_TIVA = inner_join(emr_TIVA,file_id,by='Case_ID') 
nrow(file_case_TIVA)  # 1312

# vitalrecord load(TIVA)
vital_TIVA = data.frame()

system.time(
  
  for (file_id in file_case_TIVA$Case_ID){
    
    df = fread(paste0('seoul_1803_2012/seoul_1803_2012/',file_id,'.csv_tag_filled_del.csv'))
    
    df$Case_ID = file_id
    
    vital_TIVA = bind_rows(vital_TIVA,df) 
    
  })

str(vital_TIVA)

# saveRDS(vital_TIVA,"/preprocessing/vital_TIVA")

vital_TIVA = readRDS("/preprocessing/vital_TIVA")
names(vital_TIVA)
length(unique(vital_TIVA$Case_ID))  # 1312

# 필요한 변수만 추출 
names(vital_TIVA)
except_cols = c("AMB_PRES","AGENT_NAME","AGENT_MAC","AGENT_FI","MV","MAWP","PEEP_TOTAL","EVENT","Hypotension","Hypertension","BIS/SEF","BIS/SQI","BIS/EMG","BIS/TOTPOW","PROPOFOL_RATE","PROPOFOL_VOL","REMIFENTANIL_RATE","REMIFENTANIL_VOL","RR_TOTAL")

vital_TIVA = vital_TIVA[,!names(vital_TIVA)%in%except_cols]
names(vital_TIVA) # 변수 확인 

# optime 확인 -> 0 인 경우 존재. 
emr_tiva_data = emr_TIVA%>%inner_join(vital_TIVA,"Case_ID")
table(emr_tiva_data$OP.Time)  

# OP.Time 확인 
OP_0_case = unique(data.table(emr_tiva_data)[OP.Time=='0',Case_ID]) # 3건 

emr_TIVA = data.table(emr_TIVA)

for (case in OP_0_case){
  
  df = subset(emr_tiva_data,emr_tiva_data$Case_ID == case)
  emr_TIVA[Case_ID==case,]$OP.Time = round(difftime(df$Time[nrow(df)],df$Time[1],units = 'mins'))
  print(round(difftime(df$Time[nrow(df)],df$Time[1],units = 'mins')))
}

# saveRDS(emr_TIVA, 'preprocessing/emr_TIVA.rds')

# tiva preprocessing
vital_TIVA = as.data.table(vital_TIVA)
names(vital_TIVA)

# 파일별 결측치 수를 확인하기 전에 레미판타닐과 프로포폴 모두에 없는 케이스 제거 
# TIVA: AGENT 관련 결측치는 모두 0으로 처리
# AGENT_ET변수는 max 값이 1미만인 경우 모두0 으로 처리 
vital_TIVA$AGENT_ET[is.na(vital_TIVA$AGENT_ET)] = 0
agent_max = vital_TIVA[,max(AGENT_ET),by='Case_ID']   # data.table에서만 가능 
agent_ID = agent_max[agent_max$V1 < 1,Case_ID]         # max값이 1미만인 경우 추출 

vital_TIVA2 = vital_TIVA[Case_ID%in%agent_ID,] 
vital_TIVA2$AGENT_ET = 0                      # 0 대입

vital_TIVA_total = rbind(vital_TIVA[!Case_ID%in%agent_ID,],vital_TIVA2)      
length(unique(vital_TIVA_total$Case_ID))  

RE_PR_0 = as.data.frame(vital_TIVA_total)%>%select(Case_ID,REMIFENTANIL_CP,REMIFENTANIL_CE,REMIFENTANIL_CT,PROPOFOL_CP, PROPOFOL_CE, PROPOFOL_CT)%>%group_by(Case_ID)%>%summarise_all(funs(sum))

RE_PR_0[is.na(RE_PR_0)] = 0
RE_PR_0$sum_0 = apply(RE_PR_0[,c(2:7)],1,sum)

case_except = RE_PR_0[RE_PR_0$sum_0==0,]$Case_ID
length(case_except)  # 204  -> 269

vital_TIVA_total = vital_TIVA_total[!vital_TIVA_total$Case_ID%in%case_except,]
length(unique(vital_TIVA_total$Case_ID)) # 1045건

RE_PR_col = c("REMIFENTANIL_CP","REMIFENTANIL_CE","REMIFENTANIL_CT","PROPOFOL_CP", "PROPOFOL_CE", "PROPOFOL_CT")

vital_TIVA_total = data.frame(vital_TIVA_total)
vital_TIVA_total[,RE_PR_col][is.na(vital_TIVA_total[,RE_PR_col])] = 0 
length(unique(vital_TIVA_total$Case_ID)) # 827  -> 1043

# HR, SBP 이상치 
summary(vital_TIVA_total$HR)
summary(vital_TIVA_total$NIBP_SBP)

vital_TIVA_total$HR = ifelse(vital_TIVA_total$HR>200|vital_TIVA_total$HR<20,NA,vital_TIVA_total$HR)
vital_TIVA_total$NIBP_SBP = ifelse(vital_TIVA_total$NIBP_SBP>300|vital_TIVA_total$NIBP_SBP<20,NA,vital_TIVA_total$NIBP_SBP)

summary(vital_TIVA_total$HR)
summary(vital_TIVA_total$NIBP_SBP)

unique(vital_TIVA_total$Case_ID)

library(dlookr)
find_na(vital_TIVA_total,rate = TRUE)

NA_ENDTOEND = apply(file_ENDTOEND_NA(vital_TIVA_total),2,a_100)
NA_ENDTOEND

# # BIS.BIS 결측치 건수 확인 
# total_copy = data.table(vital_TIVA_total)
# 
# bis_na_case = c()
# 
# for(case in unique(total_copy$Case_ID)){
#   if(sum(is.na(total_copy[Case_ID==case,BIS.BIS]))/nrow(total_copy[Case_ID==case,])==1){
#     bis_na_case = c(bis_na_case, case)
#   }
# }
# 
# length(bis_na_case)  # 222

file_list_record = list.files("Record_1803_2004")

# vital record의 case_id 추출
file_id =  unlist(lapply(str_split(file_list_record,".txt"),function(x){x[1]}))
length(file_id)  # 3519

vital_record_case = unique(vital_TIVA_total$Case_ID)[unique(vital_TIVA_total$Case_ID)%in%file_id]
length(vital_record_case)   # 1042 # tiva와 겹치는 마취 기록지 케이스 

TIVA_record = data.frame()

system.time(
  
  for (file_id_record in vital_record_case){
    
    df = fread(paste0('Record_1803_2004/',file_id_record,'.txt.csv'))[,c("arecord_id", "group", "item", "value1", "date1")]
    
    df$Case_ID = file_id_record
    
    TIVA_record = bind_rows(TIVA_record,df) 
    
  })

length(unique(TIVA_record$Case_ID))  # 1042 확인 

TIVA_record = data.table(TIVA_record)
item_bis = TIVA_record[item=="BIS"]
length(unique(item_bis$Case_ID))  # 998 

item_TV = TIVA_record[item=="TV"]
length(unique(item_TV$Case_ID))  # 1017 건

item_RR = TIVA_record[item=="RR"]
length(unique(item_RR$Case_ID)) # 1017건

# 공통되는 케이스 기준으로 코드 돌림. 
commom_case = unique(item_RR$Case_ID)[unique(item_RR$Case_ID)%in%unique(item_bis$Case_ID)]
length(commom_case)  # 975건 겹침 

head(vital_TIVA_total$Time)
tail(vital_TIVA_total$Time)


# 마취기록지에서 대체 
total_BIS = data.table()
total_TV = data.table()
total_RR = data.table()

count = 1

Sys.time()

for(case in commom_case){
  
  print(count)
  
  df = total_copy[Case_ID==case,c("Case_ID","Time","BIS.BIS","TV")]
  
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

Sys.time() 

nrow(total_BIS) 
colSums(is.na(total_BIS))
tail(total_BIS)
head(total_BIS)

head(item_bis)
nrow(total_TV)
nrow(total_RR)

total = cbind(total_BIS[,c("Case_ID","Time","BIS.BIS")],total_TV[,"TV"], total_RR[,"RR"])
length(unique(total$Case_ID)) # 975
colSums(is.na(total))

# saveRDS(total,"total_na_preprocessing_tiva_0121.rds")

total = readRDS("total_na_preprocessing_tiva_0121.rds")
colSums(is.na(total))

# NA인 케이스 수 
tv_na_case = c()
rr_na_case = c()

for (case in unique(total$Case_ID)){
  if(is.na(total[Case_ID==case,TV][1])==TRUE){
    tv_na_case = c(tv_na_case,case)
  }
  if(is.na(total[Case_ID==case,RR][1])==TRUE){
    rr_na_case = c(rr_na_case,case)
  }
}

rr_na_case
tv_na_case

emr_rr = data.table(emr_TIVA)[Case_ID%in%rr_na_case,]
emr_rr[emr_rr$class=="MACE"]$Case_ID

# 마취기록지와 시간이 달라서 생기는 결측치 (RR 13건 기준)
# 나머지 TIVA는 원래 값들이 기록되어 있는 것 

length(unique(total$Case_ID))  # 975건 

vital_record_data = data.table(subset(vital_TIVA_total,vital_TIVA_total$Case_ID%in%unique(total$Case_ID)))

length(unique(vital_TIVA_total$Case_ID)) # 1043
length(unique(vital_record_data$Case_ID))

# 같은 행인지 확인 
head(total)
head(vital_record_data)

# 마취기록지와 vital 레코드의 시간이 다른 경우 
vital_time = data.table()

for(case in rr_na_case){
  vital_time = rbind(vital_time, cbind(case,vital_start_time = vital_record_data[Case_ID==case,]$Time[1],vital_end_time = vital_record_data[Case_ID==case,]$Time[nrow(vital_record_data[Case_ID==case,])],record_start_time = item_RR[Case_ID==case,]$date1[1],record_end_time = item_RR[Case_ID==case,]$date1[nrow(item_RR[Case_ID==case,])],RR = "RR"))
  
}

vital_time$TV = ifelse(vital_time$case %in% tv_na_case, "TV","NOT_NA")
vital_time
names(vital_time)

total_Check_time = rbind(vital_time[,c(1:5)],vital_time_af[,c(1:5)])
emr_re = emr_TIVA[Case_ID%in%total_Check_time$case,]
emr_re$class
total_Check_time$Mace = emr_re$class
table(total_Check_time$Mace)
write.csv(total_Check_time, 'tiva_time.csv')

# TV_vital_time
# RR_vital_time
# 
# Time_check = c()
# 
# for (case in commom_case){
#   
#   Time_check = data.table(rbind(Time_check, cbind(RR_TIME = item_RR[Case_ID==case,]$date1[1],TV_TIME = item_TV[Case_ID==case,]$date1[1],BIS_TIME = item_bis[Case_ID==case,]$date1[1],vital_op.time = difftime(vital_record_data[Case_ID==case,Time][nrow(vital_record_data[Case_ID==case,])],vital_record_data[Case_ID==case,Time][1],units= "mins"))))
# }
# 
# Time_check = cbind(commom_case,Time_check)
# 
# head(Time_check)
# 
# Time_check[(RR_TIME!=TV_TIME)|(RR_TIME!=BIS_TIME)|(TV_TIME!=BIS_TIME),]  # 608건 
# Time_check[(RR_TIME!=BIS_TIME),] # BIS의 시간만 다름. 
# Time_check[(RR_TIME!=TV_TIME),]  # RR 타임과 TV타임이 다른 경우는 없음. 

# RR 타임과 TV타임의 시간이 같음에도 대체되는 값이 다른 이유는 RR의 경우 원래 데이터에 값이 없고 TV는 있기 때문 

# Time_check$diff_RRBIS = difftime(Time_check[,RR_TIME], Time_check[,BIS_TIME],units = "mins")  # 양수: RR 타임 시간 늦은 행. 
# head(Time_check)
# 
# table(Time_check$diff_RRBIS)
# write.csv(Time_check,"Time_check.csv")

vital_time_af = c()

for(case in unique(vital_record_data$Case_ID)){
  Time_record = vital_record_data[Case_ID==case,]$Time
  if(Time_record[1]>item_RR[Case_ID==case,]$date1[nrow(item_RR[Case_ID==case,])]){
    
    vital_time_af = rbind(vital_time_af, 
                       cbind(case,vital_start_time = Time_record[1],
                             vital_end_time = Time_record[length(Time_record)],
                             record_start_time = item_RR[Case_ID==case,]$date1[1],
                             record_end_time = item_RR[Case_ID==case,]$date1[nrow(item_RR[Case_ID==case,])],
                            diff = difftime(Time_record[length(Time_record)],item_RR[Case_ID==case,]$date1[1],units = 'mins')))
  }
}

data.table(vital_time_af)$case[data.table(vital_time_af)$case%in%rr_na_case] # 겹치는 것 없음. (3건)

total = na.omit(total)

vital_record_data = subset(vital_TIVA_total,vital_TIVA_total$Case_ID%in%unique(total$Case_ID))

length(unique(vital_record_data$Case_ID))

vital_record_data[Case_ID==data.table(vital_time)$case[1],]
item_RR[Case_ID==data.table(vital_time)$case[1],]
item_TV[Case_ID==data.table(vital_time)$case[1],]

# 값 대체 
vital_record_data$RR = total$RR
vital_record_data$TV = total$TV
vital_record_data$BIS.BIS = total$TV
length(unique(vital_record_data$Case_ID))  # 962

colSums(is.na(vital_record_data))
find_na(vital_record_data,rate= TRUE)

# 파일별 처음부터 끝까지 결측치 - 확인 함수 적용(개수 반환)
NA_ENDTOEND_record = apply(file_ENDTOEND_NA(vital_record_data),2,a_100)
NA_ENDTOEND_record

length(unique(vital_record_data$Case_ID))

except_100_col = names(vital_record_data)[round(NA_ENDTOEND_record/length(unique(vital_record_data$Case_ID)),2)>0.1]
except_100_col

# 수정된 vital 데이터의 BT, CO2, NMT_TOF_CNT, PIP, COMPLIANCE 변수 제거 
vital_record_NACOL = vital_record_data[,!names(vital_record_data)%in%except_100_col]

length(names(vital_record_NACOL))
length(names(vital_record_data))
names(vital_record_NACOL)

## 변수 삭제 후 파일별 해당 컬럼이 모두 결측치인 case를 제외하기 위해 결측치가 존재하는 변수 확인 
vital_record_NACOL$NMT_TOF_CNT = NULL

except_100_col2 = apply(file_ENDTOEND_NA(vital_record_data),2,a_100)
names(except_100_col2[except_100_col2!=0])
 
# 케이스 삭제 
except_case_id_total = c()

for(col_ in names(except_100_col2[except_100_col2!=0])){

  except_case_id = file_ENDTOEND_NA(vital_record_data)$Case_ID[file_ENDTOEND_NA(vital_record_data)[col_]==100]
  except_case_id_total = c(except_case_id_total, except_case_id)
  
}

except_case_id_total = unique(except_case_id_total) # 121건 제외
length(except_case_id_total)

vital_TIVA_total = vital_record_NACOL[!vital_record_NACOL$Case_ID%in%except_case_id_total,]
length(unique(vital_TIVA_total$Case_ID)) # 841건 남음. 

find_na(vital_TIVA_total,rate = TRUE)

HR_NA = subset(vital_TIVA_total, is.na(vital_TIVA_total$HR))

library(zoo)
HR_df = data.frame()

# HR의 앞뒤 결측치 처리 
for (id in unique(HR_NA$Case_ID)){
  df = subset(vital_TIVA_total, vital_TIVA_total$Case_ID == id)
  df2 = df[which(!is.na(df$HR))[1]:which(!is.na(df$HR))[length(which(!is.na(df$HR)))],]
  
  df2$HR =na.approx(df2$HR)
  
  HR_df = rbind(HR_df, df2) 
}

summary(HR_df$HR)

find_na(HR_df, rate = TRUE)

vital_TIVA_total2 = rbind(vital_TIVA_total[!vital_TIVA_total$Case_ID%in%unique(HR_NA$Case_ID),],HR_df)

length(unique(vital_TIVA_total2$Case_ID))
find_na(vital_TIVA_total2,rate = TRUE)

saveRDS(vital_TIVA_total2,'preprocessing\\vital_TIVA_NA_END_0122.rds')
str(vital_TIVA_total2)

Tt = readRDS('preprocessing\\vital_TIVA_NA_END_0122.rds')
names(Tt)

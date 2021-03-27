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
  
  return(vital_NA)
}



setwd('E:\\')

emr_TIVA = readRDS('preprocessing/emr_TIVA_0205.rds')
names(emr_TIVA)[1] = 'Case_ID'
nrow(emr_TIVA)

# vital data load
file_list = list.files("seoul_1803_2012\\seoul_1803_2012")

# vital record의 case_id 추출
file_id =  data.frame(unlist(lapply(str_split(file_list,".csv_tag"),function(x){x[1]})))
names(file_id) = 'Case_ID'
file_id$Case_ID = as.character(file_id$Case_ID)

# tiva
file_case_TIVA = inner_join(emr_TIVA,file_id,by='Case_ID') 
nrow(file_case_TIVA)  # 1305

# vitalrecord load(TIVA)
vital_TIVA = data.frame()

system.time(
  
  for (file_id in file_case_TIVA$Case_ID){
    
    df = fread(paste0('seoul_1803_2012/seoul_1803_2012/',file_id,'.csv_tag_filled_del.csv'))
    
    df$Case_ID = file_id
    
    vital_TIVA = bind_rows(vital_TIVA,df) 
    
  })

str(vital_TIVA)
length(unique(vital_TIVA$Case_ID))  # 1305
# saveRDS(vital_TIVA,"/preprocessing/vital_TIVA")

vital_TIVA = readRDS("/preprocessing/vital_TIVA")
names(vital_TIVA)
length(unique(vital_TIVA$Case_ID))  # 1305

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

# saveRDS(emr_TIVA, 'preprocessing/emr_TIVA_0205_op_revise.rds')

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
length(unique(vital_TIVA_total$Case_ID)) # 827  -> 1036

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

file_list_record = list.files("Revise_Record_1803_2004")

# vital record의 case_id 추출
file_id =  unlist(lapply(str_split(file_list_record,".txt"),function(x){x[1]}))
length(file_id)  # 3519

vital_record_case = unique(vital_TIVA_total$Case_ID)[unique(vital_TIVA_total$Case_ID)%in%file_id]
length(vital_record_case)   # 1035 # tiva와 겹치는 마취 기록지 케이스 

TIVA_record = data.frame()

system.time(
  
  for (file_id_record in vital_record_case){
    
    df = read.csv(paste0('Revise_Record_1803_2004/',file_id_record,'.txt.csv'))[,c("arecord_id", "group", "item", "value1", "date1")]
    
    df$Case_ID = file_id_record
    
    TIVA_record = rbind(TIVA_record,df) 
    
  })

length(unique(TIVA_record$Case_ID))  # 1035 확인 
TIVA_record = data.table(TIVA_record)

item_bis = TIVA_record[item=="BIS"]
length(unique(item_bis$Case_ID))  # 993 건 


item_TV = TIVA_record[item=="TV"]
length(unique(item_TV$Case_ID))  # 1011 건

item_RR = TIVA_record[item=="RR"]
length(unique(item_RR$Case_ID)) # 1011건


# 공통되는 케이스 기준으로 코드 돌림. 
common_case = unique(item_RR$Case_ID)[unique(item_RR$Case_ID)%in%unique(item_bis$Case_ID)]
length(common_case)  # 970건 겹침 


#############################################################################
# 시간 차이가 1시간 초과인 경우 마취기록지 시간으로 동기화
#############################################################################


bis_start_time_diff = c()
tv_start_time_diff = c()
rr_start_time_diff = c()

# 끝나는 시간 확인하지 않아도됨.(시작시점만 맞추기 때문에)
bis_end_time_diff = c()
tv_end_time_diff = c()
rr_end_time_diff = c()

for(case in common_case){
  print(case)
  data = data.table(vital_TIVA_total)[Case_ID == case,]
  bis_data = item_bis[Case_ID==case,]
  tv_data = item_TV[Case_ID==case,]
  rr_data = item_RR[Case_ID==case,]
  
  bis_start_time_diff = c(bis_start_time_diff, difftime(data$Time[1], bis_data$date1[1],units = "mins"))
  tv_start_time_diff = c(tv_start_time_diff, difftime(data$Time[1], tv_data$date1[1],units = "mins"))
  rr_start_time_diff = c(rr_start_time_diff, difftime(data$Time[1], rr_data$date1[1],units = "mins"))
  
  bis_end_time_diff = c(bis_end_time_diff, difftime(data$Time[nrow(data)], bis_data$date1[nrow(bis_data)],units = "mins"))
  tv_end_time_diff = c(tv_end_time_diff, difftime(data$Time[nrow(data)], tv_data$date1[nrow(tv_data)],units = "mins"))
  rr_end_time_diff = c(rr_end_time_diff, difftime(data$Time[nrow(data)], rr_data$date1[nrow(rr_data)],units = "mins"))
  
}

time_data = data.frame(cbind(bis_start_time_diff, tv_start_time_diff, rr_start_time_diff, bis_end_time_diff, tv_end_time_diff, rr_end_time_diff,common_case))

# 한 시간 초과 차이나는 경우 시간 맞춤 
time_over_60 = time_data[abs(bis_start_time_diff)>60|abs(rr_start_time_diff)>60|abs(tv_start_time_diff)>60,]   
time_over_60$common_case

head(vital_TIVA_total$Time)
tail(vital_TIVA_total$Time)

time_over_60$common_case

vital_TIVA_total_not_revise = subset(vital_TIVA_total, !Case_ID%in%time_over_60$common_case) 
nrow(vital_TIVA_total_not_revise) #  2236613
common_revise_time = data.frame()

for(case in time_over_60$common_case){
  
  df = subset(vital_TIVA_total,Case_ID == case)
  op_time = difftime(df$Time[nrow(df)],df$Time[1],units='secs')
  df_bis = item_bis[Case_ID == case,]
  
  end_time = as.POSIXct(df_bis$date1[1])+op_time
  
  # 3초 간격으로 되어있다고 하지만 중간중간 끊긴 경우가 있음. 
  # vital 데이터 행개수 기준으로 개수 맞춤. 
  df$Time = seq(as.POSIXct(df_bis$date1[1]), end_time, by = 3)[1:nrow(df)]
  common_revise_time = rbind(common_revise_time, df)
}

nrow(common_revise_time) # 34937

vital_TIVA_total_time_revise = rbind(vital_TIVA_total_not_revise, common_revise_time)

# 마취기록지에서 대체 
total_BIS = data.table()
total_TV = data.table()
total_RR = data.table()

count = 1

Sys.time()

for(case in common_case){
  
  print(count)
  
  df = data.table(vital_TIVA_total_time_revise)[Case_ID==case,]
  
  df_item_bis = item_bis[Case_ID == case,]
  df_item_TV = item_TV[Case_ID == case,]
  df_item_RR = item_RR[Case_ID == case,]
  
  df$RR = NA
  
  df$RR[1] = df_item_RR$value1[1]
  
  for (i in 1:nrow(df_item_RR)){
    
    for(j in 2:nrow(df)){
      
      if(df_item_RR[i,date1]<=df[j,Time]){
        
        df$RR[j] = df_item_RR$value1[i]
        
      }
      
      if(is.na(df$RR[j])==TRUE){
        df$RR[j] = df$RR[j-1]
      }
    }
    
  }
  
  total_RR = rbind(total_RR, df)
  
  if(sum(is.na(df$BIS.BIS))==nrow(df)){
    
    df$BIS.BIS[1] = df_item_bis$value1[1]
    
    for (i in 1:nrow(df_item_bis)){
      
      for(j in 2:nrow(df)){
        
        if(df_item_bis[i,date1]<=df[j,Time]){
          
          df$BIS.BIS[j] = df_item_bis$value1[i]
          
        }
        if(is.na(df$BIS.BIS[j])==TRUE){
          df$BIS.BIS[j] = df$BIS.BIS[j-1]
        }
      }
    }
    
    total_BIS = rbind(total_BIS, df)
    
  }
  
  else if (sum(is.na(df$BIS.BIS))!=nrow(df)) {
    total_BIS = rbind(total_BIS, df)
  }  
  
  if(sum(is.na(df$TV))==nrow(df)){
    
    df$TV[1] = df_item_TV$value1[1]
    
    for (i in 1:nrow(df_item_TV)){
      
      for(j in 2:nrow(df)){
        
        if(df_item_TV[i,date1]<=df[j,Time]){
          
          df$TV[j] = df_item_TV$value1[i]
        }
        
        if(is.na(df$TV[j])==TRUE){
          df$TV[j] = df$TV[j-1]
        }
      }
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
length(unique(total$Case_ID)) # 970
colSums(is.na(total))

# saveRDS(total,"preprocessing/total_na_preprocessing_tiva_0223.rds")
# save.image("record_tiva.RData")
load("record_tiva.RData")

total = readRDS("preprocessing/total_na_preprocessing_tiva_0223.rds")
colSums(is.na(total))
length(unique(total$Case_ID))

# tiva 이상치 변수 제외 
tiva_outlier = read.csv("tiva_이상치.csv")
nrow(tiva_outlier)
length(unique(tiva_outlier$Case_ID))  # 31

total_out = total[total$Case_ID%in%tiva_outlier$Case_ID,]
total_right = total[!total$Case_ID%in%tiva_outlier$Case_ID,]
length(unique(total_out$Case_ID)) # 14
length(unique(total_right$Case_ID)) # 956

# 마취기록지에서 대체 
total_BIS = data.table()
total_TV = data.table()
total_RR = data.table()

count = 1

Sys.time()

for(case in unique(total_out$Case_ID)){
  
  print(count)
  
  df = data.table(vital_TIVA_total_time_revise)[Case_ID==case,]
  
  df_item_bis = item_bis[Case_ID == case,]
  df_item_TV = item_TV[Case_ID == case,]
  df_item_RR = item_RR[Case_ID == case,]
  
  df$RR = NA
  
  df$RR[1] = df_item_RR$value1[1]
  
  for (i in 1:nrow(df_item_RR)){
    
    for(j in 2:nrow(df)){
      
      if(df_item_RR[i,date1]<=df[j,Time]){
        
        df$RR[j] = df_item_RR$value1[i]
        
      }
      
      if(is.na(df$RR[j])==TRUE){
        df$RR[j] = df$RR[j-1]
      }
    }
    
  }
  
  total_RR = rbind(total_RR, df)
  
  if(sum(is.na(df$BIS.BIS))==nrow(df)){
    
    df$BIS.BIS[1] = df_item_bis$value1[1]
    
    for (i in 1:nrow(df_item_bis)){
      
      for(j in 2:nrow(df)){
        
        if(df_item_bis[i,date1]<=df[j,Time]){
          
          df$BIS.BIS[j] = df_item_bis$value1[i]
          
        }
        if(is.na(df$BIS.BIS[j])==TRUE){
          df$BIS.BIS[j] = df$BIS.BIS[j-1]
        }
      }
    }
    
    total_BIS = rbind(total_BIS, df)
    
  }
  
  else if (sum(is.na(df$BIS.BIS))!=nrow(df)) {
    total_BIS = rbind(total_BIS, df)
  }  
  
  if(sum(is.na(df$TV))==nrow(df)){
    
    df$TV[1] = df_item_TV$value1[1]
    
    for (i in 1:nrow(df_item_TV)){
      
      for(j in 2:nrow(df)){
        
        if(df_item_TV[i,date1]<=df[j,Time]){
          
          df$TV[j] = df_item_TV$value1[i]
        }
        
        if(is.na(df$TV[j])==TRUE){
          df$TV[j] = df$TV[j-1]
        }
      }
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

total_out2 = cbind(total_BIS[,c("Case_ID","Time","BIS.BIS")],total_TV[,"TV"], total_RR[,"RR"])
length(unique(total_out2$Case_ID)) # 14
colSums(is.na(total_out2))

total = rbind(total_right, total_out2)
nrow(total) # 2122467
length(unique(total$Case_ID)) # 14

# NA인 케이스 수 
# tv_na_case = c()
# rr_na_case = c()
# 
# for (case in unique(total$Case_ID)){
#   if(is.na(total[Case_ID==case,TV][1])==TRUE){
#     tv_na_case = c(tv_na_case,case)
#   }
#   if(is.na(total[Case_ID==case,RR][1])==TRUE){
#     rr_na_case = c(rr_na_case,case)
#   }
# }


length(unique(vital_TIVA_total_time_revise$Case_ID)) # 1036

# 마취기록지의 TV, BIS.BIS, RR이 있는 경우의 vr data 만 추출 
vital_record_data = subset(vital_TIVA_total_time_revise,vital_TIVA_total_time_revise$Case_ID%in%unique(total$Case_ID))
vital_record_data = data.table(vital_record_data)
length(unique(vital_record_data$Case_ID)) # 956


head(vital_record_data$Case_ID)
tail(vital_record_data$Case_ID)

head(total$Case_ID)
tail(total$Case_ID) # case 순서 다름 


vital_record_data = data.table(subset(vital_TIVA_total,vital_TIVA_total$Case_ID%in%unique(total$Case_ID)))
vital_record_data

length(unique(vital_record_data$Case_ID))

# 값 대체 
end_item_vital = data.frame()

for(case in unique(vital_record_data$Case_ID)){
  
  df = vital_record_data[Case_ID==case,]
  item_df = total[Case_ID==case,]
  
  df$BIS.BIS = item_df$BIS.BIS
  df$TV = item_df$TV
  df$RR = item_df$RR
  
  end_item_vital = rbind(end_item_vital, df)
}

end_item_vital
colSums(is.na(end_item_vital))

###############################################################################
# 처음부터 끝까지 결측치인 변수 0으로 대체
###############################################################################
# 파일별 처음부터 끝까지 결측치 - 확인 함수 적용(데이터프레임 반환)
NA_ENDTOEND_record = file_ENDTOEND_NA(end_item_vital)
apply(NA_ENDTOEND_record,2,a_100)

length(unique(NA_ENDTOEND_record$Case_ID))

TIVA_total= data.frame()
col_list = names(NA_ENDTOEND_record)

count=1

end_item_vital = data.frame(end_item_vital)

for(case in unique(NA_ENDTOEND_record$Case_ID)){
  
  print(count)
  
  df = subset(NA_ENDTOEND_record,Case_ID==case)
  
  df_col_name = names(df[apply(df,2, function(x) x==100)])
  
  real_df = subset(end_item_vital, Case_ID == case)
  
  real_df[df_col_name] = 0
  
  TIVA_total = rbind(TIVA_total, real_df)
  
  count = count+1
} 

colSums(is.na(TIVA_total))

head(TIVA_total)
length(unique(TIVA_total$Case_ID))  
nrow(TIVA_total)

colSums(is.na(TIVA_total))

total_ENDTOEND_record = file_ENDTOEND_NA(TIVA_total)
apply(total_ENDTOEND_record,2,a_100)

HR_NA = subset(TIVA_total, is.na(TIVA_total$HR))

library(zoo)
HR_df = data.frame()

# HR의 앞뒤 결측치 처리 
for (id in unique(HR_NA$Case_ID)){
  df = subset(TIVA_total, TIVA_total$Case_ID == id)
  df2 = df[which(!is.na(df$HR))[1]:which(!is.na(df$HR))[length(which(!is.na(df$HR)))],]
  
  df2$HR =na.approx(df2$HR)
  
  HR_df = rbind(HR_df, df2) 
}

SBP_NA = subset(TIVA_total, is.na(TIVA_total$NIBP_SBP))
unique(length(SBP_NA$Case_ID))

SBP_df = data.frame()

# SBP의 앞뒤 결측치 처리 
for (id in unique(SBP_NA$Case_ID)){
  df = subset(TIVA_total, TIVA_total$Case_ID == id)
  df2 = df[which(!is.na(df$NIBP_SBP))[1]:which(!is.na(df$NIBP_SBP))[length(which(!is.na(df$NIBP_SBP)))],]
  
  df2$NIBP_SBP = na.approx(df2$NIBP_SBP)
  
  SBP_df = rbind(SBP_df, df2) 
}

summary(HR_df$HR)
summary(SBP_df$NIBP_SBP)

find_na(HR_df, rate = TRUE)
find_na(SBP_df, rate = TRUE)

TIVA_total2 = rbind(TIVA_total[!TIVA_total$Case_ID%in%unique(HR_NA$Case_ID),],HR_df)
TIVA_total2 = rbind(TIVA_total2[!TIVA_total2$Case_ID%in%unique(SBP_NA$Case_ID),],SBP_df)

length(unique(TIVA_total2$Case_ID))
find_na(TIVA_total2,rate = TRUE)

nrow(TIVA_total2)
nrow(TIVA_total)

TIVA_total2$BIS.BIS = as.numeric(TIVA_total2$BIS.BIS)
TIVA_total2$TV = as.numeric(TIVA_total2$TV)
TIVA_total2$RR = as.numeric(TIVA_total2$RR)
summary(TIVA_total2)

saveRDS(TIVA_total2,'preprocessing\\vital_TIVA_NA_END_0225.rds')

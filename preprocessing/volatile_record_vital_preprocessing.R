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

setwd("E:/")
emr_volatile = readRDS('preprocessing/emr_volatile_0205.rds')
emr_volatile = data.table(emr_volatile)[BUN.1<500]

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
nrow(file_case_volatile)  # 582

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
length(unique(vital_volatile$Case_ID)) # 546 -> 589 -> 582
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


# BIS.BIS 
total_copy = data.table(vital_volatile_total)

bis_na_case = c()

for(case in unique(total_copy$Case_ID)){
  if(sum(is.na(total_copy[Case_ID==case,BIS.BIS]))/nrow(total_copy[Case_ID==case,])==1){
    bis_na_case = c(bis_na_case, case)
  }
}

length(bis_na_case)  # 147

file_list_record = list.files("Revise_Record_1803_2004")

# vital record의 case_id 추출
file_id =  unlist(lapply(str_split(file_list_record,".txt"),function(x){x[1]}))
length(file_id)  # 3519
vital_record_case = unique(vital_volatile_total$Case_ID)[unique(vital_volatile_total$Case_ID)%in%file_id]
length(vital_record_case)  

table(bis_na_case%in%file_id)  # 1건 빼고 모두 있음. 

volatile_record = data.frame()

system.time(
  
  for (file_id_record in vital_record_case){
    
    df = fread(paste0('Revise_Record_1803_2004/',file_id_record,'.txt.csv'))[,c("arecord_id", "group", "item", "value1", "date1")]
    
    df$Case_ID = file_id_record
    
    volatile_record = bind_rows(volatile_record,df) 
    
  })

length(unique(volatile_record$Case_ID))  # 251

volatile_record = data.table(volatile_record)
item_bis = volatile_record[item=="BIS"]
length(unique(item_bis$Case_ID))  # 209건   # RR과 37건이 안겹침 
item_PPV = volatile_record[item=="PPV"]

bis_che = c()
for(case in unique(item_bis$Case_ID)){
  if(nrow(item_bis[Case_ID==case,])==1){
    bis_che = c(bis_che, case)
  }
}

item_TV = volatile_record[item=="TV"]
length(unique(item_TV$Case_ID))  # 236 건

TV_che = c()
for(case in unique(item_TV$Case_ID)){
  if(nrow(item_TV[Case_ID==case,])==1){
    TV_che = c(TV_che, case)
  }
}

item_RR = volatile_record[item=="RR"]
length(unique(item_RR$Case_ID)) # 236건


RR_che = c()
for(case in unique(item_RR$Case_ID)){
  if(nrow(item_RR[Case_ID==case,])==1){
    RR_che = c(RR_che, case)
  }
}

# BIS 기준으로 코드 돌림. 
common_case = unique(item_RR$Case_ID)[unique(item_RR$Case_ID)%in%unique(item_bis$Case_ID)]
length(common_case)  # 199건 

head(vital_volatile_total$Time)
tail(vital_volatile_total$Time)

# 공통되는 케이스 기준으로 코드 돌림. 
common_case = unique(item_RR$Case_ID)[unique(item_RR$Case_ID)%in%unique(item_bis$Case_ID)]
length(common_case)  # 970건 겹침 


#############################################################################
# 시간 차이가 1시간 초과인 경우 마취기록지 시간으로 동기화
#############################################################################

bis_start_time_diff = c()
tv_start_time_diff = c()
rr_start_time_diff = c()

# END TIME 은 필요없지만 확인 
bis_end_time_diff = c()
tv_end_time_diff = c()
rr_end_time_diff = c()


for(case in common_case){
  print(case)
  data = data.table(vital_volatile_total)[Case_ID == case,]
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

time_over_60 = time_data[abs(bis_start_time_diff)>60|abs(rr_start_time_diff)>60|abs(tv_start_time_diff)>60,]   

# 시작시점을 사용할 것이기 때문에 불필요해진 코드
# time_over_60$common_case
# 
# CC = time_over_60$common_case[time_over_60$common_case%in%bis_che]  
# subset(time_over_60, common_case%in%CC)
# 
# CC = time_over_60$common_case[time_over_60$common_case%in%TV_che]  
# subset(time_over_60, common_case%in%CC)
# 
# CC = time_over_60$common_case[time_over_60$common_case%in%RR_che]
# subset(time_over_60, common_case%in%CC)

# BIS.BIS & RR 시작시간이 다르지만 시작시점 시각은 무시하고 사용 

vital_volatile_total_not_revise = subset(vital_volatile_total, !Case_ID%in%time_over_60$common_case) 
nrow(vital_volatile_total_not_revise) # 595800
common_revise_time = data.frame()

for(case in time_over_60$common_case){
  
  df = subset(vital_volatile_total,Case_ID == case)
  op_time = difftime(df$Time[nrow(df)],df$Time[1],units='secs')
  df_bis = item_bis[Case_ID == case,]
  
  end_time = as.POSIXct(df_bis$date1[1])+op_time
  
  # 3초 간격으로 되어있다고 하지만 중간중간 끊긴 경우가 있음. 
  df$Time = seq(as.POSIXct(df_bis$date1[1]), end_time, by = 3)[1:nrow(df)]
  common_revise_time = rbind(common_revise_time, df)
}

nrow(common_revise_time) # 11374

vital_volatile_time_revise = rbind(vital_volatile_total_not_revise, common_revise_time)

#############################################################################
# BIS, TV가 NA 인 경우 마취기록지에서 값 대체 
# RR 데이터 가져오기 
#############################################################################
total_RR = data.frame()
total_TV = data.frame()
total_BIS = data.frame()

count = 1

Sys.time()  # [1] "2021-02-23 17:26:34 KST"

for(case in commom_case){
  
  print(count)
  
  df = data.table(vital_volatile_time_revise)[Case_ID==case,c("Case_ID","Time","BIS.BIS","TV")]
  
  df_item_bis = item_bis[Case_ID == case,]
  df_item_TV = item_TV[Case_ID == case,]
  df_item_RR = item_RR[Case_ID == case,]
  
  df$RR = NA  # RR 은 마취기록지에서 모든 값을 가져옴 
  
  df$RR[1] = df_item_RR$value1[1]  # 마취기록지와 vital 시간이 안맞아서 대체안되는 경우를 막기위해(차이가 1시간 이내인 경우)
  
  for (i in 1:nrow(df_item_RR)){
    
    for(j in 2:nrow(df)){
      
      if(df_item_RR[i,date1]<=df[j,Time]){
        
        df$RR[j] = df_item_RR$value1[i]
        
      }
    
    if(is.na(df$RR[j])==TRUE){  # NA 인 경우 전값으로 대체 
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
    

      
    # if(is.na(df$BIS.BIS[2])==TRUE){
    #   idx_na_end = which(is.na(df$BIS.BIS))
    #   df[idx_na_end,]$BIS.BIS = df[1,BIS.BIS]
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


Sys.time()  # # 2시간 20분 소요    

nrow(total_BIS) 
nrow(total_TV)
nrow(total_RR)

# BIS 시간 기준으로 합침 
total = cbind(total_BIS[,c("Case_ID","Time","BIS.BIS")],total_TV[,"TV"], total_RR[,"RR"])
length(unique(total$Case_ID))  # 199개
colSums(is.na(total))

# saveRDS(total,"total_vital_na_preprocessing.rds")
total = readRDS("total_vital_na_preprocessing.rds")
colSums(is.na(total))
save.image("record_volatile_vital.RData")

length(unique(vital_volatile_time_revise$Case_ID)) # 252

# 마취기록지의 TV, BIS.BIS, RR이 있는 경우의 vr data 만 추출 
vital_record_data = subset(vital_volatile_time_revise,vital_volatile_time_revise$Case_ID%in%unique(total$Case_ID))
vital_record_data = data.table(vital_record_data)
length(unique(vital_record_data$Case_ID)) # 199


head(vital_record_data$Case_ID)
tail(vital_record_data$Case_ID)

head(total$Case_ID)
tail(total$Case_ID) # case 순서 다름 

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

volatile_total= data.frame()
col_list = names(NA_ENDTOEND_record)

count=1

all_100 = NA_ENDTOEND_record[which(apply(NA_ENDTOEND_record[,c(2:length(NA_ENDTOEND_record)-2)],1,sum)!=0),"Case_ID"]
end_item_vital = data.frame(end_item_vital)

for(case in unique(NA_ENDTOEND_record$Case_ID)){
  
  print(count)
  
  df = subset(NA_ENDTOEND_record,Case_ID==case)
  
  df_col_name = names(df[apply(df,2, function(x) x==100)])
  
  df[df_col_name] = 0
  
  volatile_total = rbind(volatile_total, df)
  
  count = count+1
} 

colSums(is.na(volatile_total))

head(volatile_total)
length(unique(volatile_total$Case_ID))  

colSums(is.na(volatile_total))

total_ENDTOEND_record = file_ENDTOEND_NA(volatile_total)
apply(total_ENDTOEND_record,2,a_100)

saveRDS(volatile_total,'preprocessing/Total_vital_record_NACOL_0223.rds')



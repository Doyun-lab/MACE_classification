library(data.table)
library(stringr)
library(stringi)
library(dplyr)

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

str(vital_volatile)

vital_volatile = readRDS("preprocessing\\vital_volatile.rds")
length(unique(vital_volatile$Case_ID)) # 546 -> 589 -> 583
names(vital_volatile)

except_cols = c("AMB_PRES","AGENT_NAME","MV","MAWP","PEEP_TOTAL","EVENT","Hypotension","Hypertension","BIS/SEF","BIS/SQI","BIS/EMG", "BIS/BIS", 
                "BIS/TOTPOW","REMIFENTANIL_RATE","REMIFENTANIL_VOL","RR_TOTAL", "TV", "PIP")
vital_volatile_ext = vital_volatile[,!names(vital_volatile)%in%except_cols]

# optime 확인 -> 0 인 경우 없음. 
emr_volatile_data = emr_volatile%>%inner_join(vital_volatile_ext,"Case_ID")
table(emr_volatile_data$OP.Time)  


# AGENT 관련 변수 결측치 처리 
# volatile preprocessing
vital_volatile_ext = as.data.table(vital_volatile_ext)
names(vital_volatile_ext)

# AGENT에 기록이 안된 경우 삭제 
a = as.data.frame(vital_volatile_ext)%>%select(Case_ID,AGENT_ET, AGENT_FI, AGENT_MAC)%>%group_by(Case_ID)%>%summarise_all(funs(sum))

a[is.na(a)] = 0
a$check = apply(a[,c(2:4)],1,sum)

case_except = a[a$check==0,]$Case_ID
length(case_except)   # 110개 -> 330개 

vital_volatile_ext_total = vital_volatile_ext[!vital_volatile_ext$Case_ID%in%case_except,]
length(unique(vital_volatile_ext_total$Case_ID)) # 436개 -> 253개

# 필요한 변수 추출 (프로포폴 변수 제외)
propofol_name = names(vital_volatile_ext_total)[str_detect(names(vital_volatile_ext_total), "PROPOFOL")]
vital_volatile_ext_total = data.frame(vital_volatile_ext_total)[,!names(vital_volatile_ext_total)%in%propofol_name]
names(vital_volatile_ext_total)

AGENT_RE_col = names(vital_volatile_ext_total)[str_detect(names(vital_volatile_ext_total),paste0(c("AGENT","REMIFENTANIL"),collapse = "|"))]
vital_volatile_ext_total = data.frame(vital_volatile_ext_total)
vital_volatile_ext_total[,AGENT_RE_col][is.na(vital_volatile_ext_total[,AGENT_RE_col])] = 0 
length(unique(vital_volatile_ext_total$Case_ID)) # 255

names(vital_volatile_ext_total)
find_na(vital_volatile_ext_total,rate=TRUE)

# HR, SBP 이상치 
summary(vital_volatile_ext_total$HR)
summary(vital_volatile_ext_total$NIBP_SBP)

vital_volatile_ext_total$HR = ifelse(vital_volatile_ext_total$HR>200|vital_volatile_ext_total$HR<20,NA,vital_volatile_ext_total$HR)
vital_volatile_ext_total$NIBP_SBP = ifelse(vital_volatile_ext_total$NIBP_SBP>300|vital_volatile_ext_total$NIBP_SBP<20,NA,vital_volatile_ext_total$NIBP_SBP)

summary(vital_volatile_ext_total$HR)
summary(vital_volatile_ext_total$NIBP_SBP)

unique(vital_volatile_ext_total$Case_ID)
length(unique(vital_volatile_ext_total$Case_ID))
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

#파일별 결측치 확인 
vital_volatile_NA = data.frame()

for (id in unique(vital_volatile_ext_total$Case_ID)){
  df = subset(vital_volatile_ext_total, vital_volatile_ext_total$Case_ID==id)
  
  df_na = cbind(data.frame(matrix(find_na(df,rate=TRUE),nrow = 1)))
  names(df_na) = names(df)
  df_na$Case_ID = id
  
  vital_volatile_NA = rbind(vital_volatile_NA,df_na) 
}

# 파일별로 전체 결측치가 100% 인 것들의 수 파악 
a_100 = function(col){
  a=0
  for (i in col){
    if (i == 100)
      a = a + 1
  }
  return(a)
}

round(apply(vital_volatile_NA,2,a_100)/nrow(vital_volatile_NA),2)

find_na(vital_volatile_ext_total,rate = TRUE)

ext_cols = c("CO2", "NMT_TOF_CNT", "COMPLIANCE")
vital_volatile_ext_total = vital_volatile_ext_total[,!names(vital_volatile_ext_total)%in%ext_cols]

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# case 제거하기 
vital_volatile_NA2 = data.frame()
 
for (id in unique(vital_volatile_ext_total$Case_ID)){
  df = subset(vital_volatile_ext_total, vital_volatile_ext_total$Case_ID==id)
  
  df_na = cbind(data.frame(matrix(find_na(df,rate=TRUE),nrow = 1)))
  names(df_na) = names(df)
  df_na$Case_ID = id
  
  vital_volatile_NA2 = rbind(vital_volatile_NA2,df_na) 
}
 
apply(vital_volatile_NA2,2,a_100)
except_100_col = names(vital_volatile_NA2)[apply(vital_volatile_NA2,2,a_100)!=0] 

except_case_id_total = c()

for (col_ in except_100_col){
  except_case_id = vital_volatile_NA2$Case_ID[vital_volatile_NA2[col_]==100]
  except_case_id_total = c(except_case_id_total, except_case_id)
}

except_case_id_total = unique(except_case_id_total)  # 228
length(except_case_id_total) # 252

vital_volatile_ext_total = vital_volatile_ext_total[!vital_volatile_ext_total$Case_ID%in%except_case_id_total,]

length(unique(vital_volatile_ext_total$Case_ID)) # 134

find_na(vital_volatile_ext_total,rate = TRUE)  # HR에 결측치 존재 

HR_NA = subset(vital_volatile_ext_total, is.na(vital_volatile_ext_total$HR))

HR_df = data.frame()

# HR의 앞뒤 결측치 처리 
for (id in unique(HR_NA$Case_ID)){
  df = subset(vital_volatile_ext_total, vital_volatile_ext_total$Case_ID == id)
  df2 = df[which(!is.na(df$HR))[1]:which(!is.na(df$HR))[length(which(!is.na(df$HR)))],]
  
  df2$HR =na.approx(df2$HR)
  
  HR_df = rbind(HR_df, df2) 
}

summary(HR_df$HR)

find_na(HR_df, rate = TRUE)

vital_volatile_ext_total2 = rbind(vital_volatile_ext_total[!vital_volatile_ext_total$Case_ID%in%unique(HR_NA$Case_ID),],HR_df)
length(unique(vital_volatile_ext_total2$Case_ID)) # 134
find_na(vital_volatile_ext_total2,rate = TRUE)

saveRDS(vital_volatile_ext_total2,'preprocessing/vital_volatile_NA_end_ext.rds')


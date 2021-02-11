library(data.table)
library(stringr)
library(stringi)
library(dplyr)

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
vital_TIVA_NA = data.frame()

vital_TIVA = readRDS("/preprocessing/vital_TIVA")
names(vital_TIVA)
length(unique(vital_TIVA$Case_ID))  # 1313

# 필요한 변수만 추출 
names(vital_TIVA)
except_cols = c("AMB_PRES","AGENT_NAME","AGENT_MAC","AGENT_FI","MV","MAWP","PEEP_TOTAL","EVENT","Hypotension","Hypertension",
                "BIS/SEF","BIS/SQI","BIS/EMG","BIS/TOTPOW", "BIS/BIS", "PROPOFOL_RATE","PROPOFOL_VOL","REMIFENTANIL_RATE","REMIFENTANIL_VOL", "RR_TOTAL", "TV", "PIP")

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

#파일별 컬럼별 결측치 확인 
vital_TIVA_NA = data.frame()

for (id in unique(vital_TIVA_total$Case_ID)){
  df = subset(vital_TIVA_total, vital_TIVA_total$Case_ID==id)
  df_na = cbind(data.frame(matrix(find_na(df,rate=TRUE),nrow = 1)))
  names(df_na) = names(df)
  df_na$Case_ID = id
  
  vital_TIVA_NA = rbind(vital_TIVA_NA,df_na) 
}

# write.csv(vital_TIVA_NA,"vital_TIVA_NA.csv")
summary(vital_TIVA_NA)

# 파일별로 전체 결측치가 100% 인 것들의 수 파악 
a_100 = function(col){
  a=0
  for (i in col){
    if (i == 100)
      a = a + 1
  }
  return(a)
}

apply(vital_TIVA_NA,2,a_100)
round(apply(vital_TIVA_NA,2,a_100)/nrow(vital_TIVA_NA),2)


# 파일별 해당 컬럼이 모두 결측치인 case 제외 
except_100_col = names(vital_TIVA_NA)[apply(vital_TIVA_NA,2,a_100)!=0] 

except_case_id_total = c()

for (col_ in except_100_col){
  except_case_id = vital_TIVA_NA$Case_ID[vital_TIVA_NA[col_]==100]
  except_case_id_total = c(except_case_id_total, except_case_id)
}

except_case_id_total = unique(except_case_id_total) # 514 -> 682건
length(except_case_id_total)

vital_TIVA_total = vital_TIVA_total[!vital_TIVA_total$Case_ID%in%except_case_id_total,]
length(unique(vital_TIVA_total$Case_ID)) # 313  -> 427 -> 363

find_na(vital_TIVA_total,rate = TRUE)
unique(vital_TIVA_total$Case_ID)

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

saveRDS(vital_TIVA_total2,'preprocessing\\vital_TIVA_NA_end_ext.rds')

vital_TIVA_total = readRDS('preprocessing\\vital_TIVA_NA_END.rds')
find_na(vital_TIVA_total, rate = T)


setwd("E:/")
emr_volatile = readRDS('preprocessing/emr_volatile.rds')
names(emr_volatile)[1] = "Case_ID"

# volatile
file_case_volatile = inner_join(emr_volatile,file_id,by='Case_ID')  
nrow(file_case_volatile) # 5022

# vitalrecord load(TIVA)
vital_TIVA = data.frame()

vital_volatile = data.frame()

system.time(
  
  for (file_id in file_case_volatile$Case_ID){
    
    df = fread(paste0('seoulVR/',file_id,'.csv_tag_filled_del.csv'))
    
    df$Case_ID = file_id
    
    vital_volatile = bind_rows(vital_volatile,df) 
    
  })

str(vital_volatile)

saveRDS(vital_volatile,'preprocessing\\vital_volatile.rds')
vital_volatile = readRDS('preprocessing\\vital_volatile.rds')

# volatile preprocessing
vital_volatile = as.data.table(vital_volatile)
names(vital_volatile)

# AGENT 관련 변수 결측치 처리 
vital_volatile$AGENT_ET[is.na(vital_volatile$AGENT_ET)] = 0
agent_max = vital_volatile[,max(AGENT_ET),by='Case_ID']   # data.table에서만 가능 
agent_max

agent_ID = agent_max[agent_max$V1<1,Case_ID]         # max값이 1미만인 경우 추출 

vital_volatile2 = vital_volatile[Case_ID%in%agent_ID,] 

vital_volatile2$AGENT_ET = 0 

vital_volatile_total = rbind(vital_volatile[!Case_ID%in%agent_ID,],vital_volatile2)      

names(vital_volatile_total)

# 필요한 변수 추출  
vo_col = names(vital_volatile)[str_detect(names(vital_volatile), paste(c('REMIFENTANIL','AGENT','Time'), collapse = "|"))]

vital_volatile = as.data.frame(vital_volatile)

colSums(is.na(vital_volatile))

vital_volatile_total = vital_volatile[,names(vital_volatile)%in%vo_col]

vital_volatile_total[is.na(vital_volatile_total)] = 0

names(vital_volatile_total)

colSums(is.na(vital_volatile_total))

# RATE, VOL 관련 변수 삭제 
vital_volatile_total$REMIFENTANIL_RATE = NULL
vital_volatile_total$REMIFENTANIL_VOL = NULL
vital_volatile_total$AGENT_NAME= NULL

find_na(vital_volatile_total,rate=TRUE)

vital_volatile_total$Case_ID = vital_volatile$Case_ID 
saveRDS(vital_volatile_total,'preprocessing/vital_volatile_NA_END.rds')

vital_volatile_total = readRDS('preprocessing/vital_volatile_NA_END.rds')

names(vital_volatile_total)

# AGENT에 기록이 안된 경우 삭제 
a = as.data.frame(vital_volatile_total)%>%select(Case_ID,AGENT_ET, AGENT_FI, AGENT_MAC)%>%group_by(Case_ID)%>%summarise_all(funs(sum))

a$check = apply(a[,c(2:4)],1,sum)

case_except = a[a$check==0,]$Case_ID
length(case_except)   # 110   # 모든 case: 1007

vital_volatile_total = vital_volatile_total[!vital_volatile_total$Case_ID%in%case_except,]

saveRDS(vital_volatile_total,'preprocessing\\vital_volatile_NA_END.rds')

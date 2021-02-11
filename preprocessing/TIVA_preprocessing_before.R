setwd('E:\\')

emr_TIVA = readRDS('preprocessing/emr_TIVA.rds')
names(emr_TIVA)[1] = 'Case_ID'

# vital data load
file_list = list.files("seoulVR")

# vital record의 case_id 추출
file_id =  data.frame(unlist(lapply(str_split(file_list,".csv_tag"),function(x){x[1]})))
names(file_id) = 'Case_ID'
file_id$Case_ID = as.character(file_id$Case_ID)

# tiva
file_case_TIVA = inner_join(emr_TIVA,file_id,by='Case_ID') 
nrow(file_case_TIVA)  # 1031

# vitalrecord load(TIVA)
vital_TIVA = data.frame()

system.time(
  
  for (file_id in file_case_TIVA$Case_ID){
    
    df = fread(paste0('seoulVR/',file_id,'.csv_tag_filled_del.csv'))
    
    df$Case_ID = file_id
    
    vital_TIVA = bind_rows(vital_TIVA,df) 
    
  })

str(vital_TIVA)
length(unique(vital_TIVA$Case_ID))

# tiva preprocessing
vital_TIVA = as.data.table(vital_TIVA)
names(vital_TIVA)

# TIVA: AGENT 관련 결측치는 모두 0으로 처리
# AGENT_ET변수는 max 값이 1미만인 경우 모두0 으로 처리 
vital_TIVA$AGENT_ET[is.na(vital_TIVA$AGENT_ET)] = 0
agent_max = vital_TIVA[,max(AGENT_ET),by='Case_ID']   # data.table에서만 가능 

agent_ID = agent_max[agent_max$V1<1,Case_ID]         # max값이 1미만인 경우 추출 

vital_TIVA2 = vital_TIVA[Case_ID%in%agent_ID,] 
vital_TIVA2$AGENT_ET = 0                      # 0 대입

vital_TIVA_total = rbind(vital_TIVA[!Case_ID%in%agent_ID,],vital_TIVA2)      

names(vital_TIVA_total)
table(vital_TIVA_total[vital_TIVA_total$AGENT_ET!=0]$Case_ID, useNA = 'ifany')

# 프로포폴과 REMIFENTANIL, AGENT_ET 변수만 사용
extract_col = names(vital_TIVA_total)[str_detect(names(vital_TIVA_total), paste(c('PROPOFOL','REMIFENTANIL','AGENT'), collapse = "|"))]
extract_col = extract_col[!str_detect(extract_col, paste(c('RATE','VOL','NAME'), collapse = "|"))]
extract_col = c(extract_col,'Case_ID','Time')

vital_TIVA_total2 = select(vital_TIVA_total,extract_col)
names(vital_TIVA_total2)

vital_TIVA_total2[is.na(vital_TIVA_total2)] = 0

colSums(is.na(vital_TIVA_total2))

# TIVA는 레미펜타닐, 프로포폴에 값이 둘다 기록이 안된 경우 해당 케이스 자체를 제거
names(vital_TIVA_total2)

a = as.data.frame(vital_TIVA_total2)%>%select(Case_ID,REMIFENTANIL_CP,REMIFENTANIL_CE,REMIFENTANIL_CT,PROPOFOL_CP, PROPOFOL_CE, PROPOFOL_CT)%>%group_by(Case_ID)%>%summarise_all(funs(sum))

a$check = apply(a[,c(2:6)],1,sum)

case_except = a[a$check==0,]$Case_ID
length(case_except)   # 205

vital_TIVA_total2 = vital_TIVA_total2[!vital_TIVA_total2$Case_ID%in%case_except,]
find_na(vital_TIVA_total2, rate = TRUE)

saveRDS(vital_TIVA_total2,'preprocessing\\vital_TIVA_NA_END.rds')


# # TIVA는 레미펜타닐, 프로포폴에 값이 둘다 기록이 안된 경우 해당 케이스 자체를 제거
# result = c() 
# 
# for (i in 1:length(unique(vital_TIVA_total2$Case_ID))){
#   
#   
#   par(mfrow=c(2,1))
#   
#   
#   
#   data = subset(vital_TIVA_total2, vital_TIVA_total2$Case_ID == unique(vital_TIVA_total2$Case_ID)[i])
#   
#   
#   
#   if (length(names(colSums(is.na(data))[colSums(is.na(data)) == nrow(data)]))>0){
#     
#     
#     
#     print(colSums(is.na(data)))
#     
#     
#     
#     print(unique(data$Case_ID))
#     
#     
#     
#     result = c(result, unique(data$Case_ID))
#     
#     
#     
#   }
#   
# }
# 
# 
# length(result)
# 
# 
# vital_TIVA_total3 = vital_TIVA_total2[!vital_TIVA_total2$Case_ID%in%result,] # 230 제거
# 
# length(unique(vital_TIVA_total3$Case_ID)) # 801
# 
# 
# names(vital_TIVA_total3) 
# 
# colSums(is.na(vital_TIVA_total3))


saveRDS(vital_TIVA_total,'preprocessing\\vital_TIVA_NA_END.rds')


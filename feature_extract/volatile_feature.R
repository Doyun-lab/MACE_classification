library(signal)
library(fBasics)
library(seewave)
library(pracma)

library(dplyr)

setwd("E:\\")


emr_volatile = readRDS('preprocessing/emr_volatile.rds')
names(emr_volatile)[1] = "Case_ID"
table(emr_volatile$OP.Time, useNA = "ifany") 
  
vital_volatile_total = readRDS('preprocessing\\vital_volatile_NA_END.rds')
find_na(vital_volatile_total,rate=TRUE)

emr_vital_volatile = vital_volatile_total%>%left_join(emr_volatile,by='Case_ID')
nrow(emr_vital_volatile)
head(emr_vital_volatile)
str(emr_vital_volatile)

names(emr_vital_volatile)
table(emr_vital_volatile[emr_vital_volatile$OP.Time==0,]$Case_ID)  # VR과 겹치는 case = 0
vital_volatile_total = readRDS('preprocessing/Total_vital_record_NACOL_0121.rds')

##################################################
#---------모든 레코드에 동일한 값 처리------------
##################################################
numeric_name = names(unlist(lapply(emr_volatile, class)[lapply(emr_volatile, class)%in%c("integer","numeric")]))
numeric_name = numeric_name[-1]

colSums(is.na(emr_volatile))

same_name = c()
# 모든레코드에 동일한 값 확인 
for(name in numeric_name){
  if(min(emr_volatile[name])==max(emr_volatile[name])){
    same_name = c(same_name,name)  
  }
}

same_name # 없음 

col_except = c("Time","Case_ID","Hypertension","Hypotension")
col_ = names(vital_volatile_total)[!names(vital_volatile_total)%in%col_except]
table(vital_volatile_total$RR)
vital_volatile_total$BIS.BIS = as.numeric(vital_volatile_total$BIS.BIS)
vital_volatile_total$TV = as.numeric(vital_volatile_total$TV)
vital_volatile_total$RR = as.numeric(vital_volatile_total$RR)
colSums(is.na(vital_volatile_total))

##################################################
#------------------특질추출-----------------------
##################################################
rss = function(x) rms(x)*sqrt(length(x))

summary(vital_volatile_total)

volatile_summary = data.frame()

for(case_id in unique(vital_volatile_total$Case_ID)){
  
  data = data.frame(subset(vital_volatile_total, vital_volatile_total$Case_ID==case_id))
  
  data_sum = c()
  
  for(col_name in col_){
    
    if(length(data[,col_name][which(data[,col_name]==0)]) == nrow(data)){
      data_summmary = data.frame(mean = 0, sd = 0, rms = 0, rss = 0, IQR = 0)
    }
    
    else{
      
      data_summary = data.frame(value = data[,col_name][which(data[,col_name]!=0)[1]:which(data[,col_name]!=0)[length(which(data[,col_name]!=0))]]) %>% summarise_all(funs(mean, sd, rms, rss, IQR))
      
    }
    
    data_sum = c(data_sum, data_summary) 
    
  }
  
  volatile_summary = rbind(volatile_summary,data.frame(matrix(unlist(data_sum),nrow = 1),Case_ID = case_id))
  
}

str(volatile_summary)
nrow(volatile_summary)
names(volatile_summary)[1:length(volatile_summary)-1] = paste0("st_",names(volatile_summary)[1:length(volatile_summary)-1])
names(volatile_summary)

saveRDS(volatile_summary,"volatile_summary_0122.rds")

#######################################################
#----------------변화하는 구간 추출--------------------
#######################################################
library(changepoint)

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

for(case_id in unique(vital_volatile_total$Case_ID)){
  
  data = data.frame(subset(vital_volatile_total, vital_volatile_total$Case_ID==case_id))
  
  data_cpt = c()
  
  for(col_name in col_){
    
    if(length(data[,col_name][which(data[,col_name]==0)]) == nrow(data)){
      data_cpt_sum = data.frame(cpt_var = 0, cpt_mean = 0, cpt_meanvar = 0)
    }
    
    else if(length(unique(round(data[,col_name][which(data[,col_name]!=0)[1]:which(data[,col_name]!=0)[length(which(data[,col_name]!=0))]],2)))==1){
      
      data_cpt_sum = data.frame(cpt_var = 0, cpt_mean = 0, cpt_meanvar = 0)
      
    }
    else{
      
      data_cpt_sum = data[,col_name][which(data[,col_name]!=0)[1]:which(data[,col_name]!=0)[length(which(data[,col_name]!=0))]] %>% cpt_f()
      
    }
    
    data_cpt = c(data_cpt, data_cpt_sum) 
  }
  
  volatile_cpt = rbind(volatile_cpt,data.frame(matrix(unlist(data_cpt),nrow = 1),Case_ID = case_id))
  
}

names(volatile_cpt)[1:length(volatile_cpt)-1] = paste0("cpt",names(volatile_cpt)[1:length(volatile_cpt)-1])
names(volatile_cpt)
saveRDS(volatile_cpt, "volatile_cpt_default_0122.rds")

#######################################################
#----------------PEAK 특징 추출-----------------------
#######################################################

peak_f = function(data){
  
  p = ifelse(!is.null(findpeaks(data,threshold = 0.5,minpeakdistance = 10)),dim(findpeaks(data,threshold = 0.5,minpeakdistance = 10))[1],0)
  
  p_interval_mean = ifelse(!is.null(findpeaks(data,threshold = 0.5,minpeakdistance = 10)),ifelse(dim(findpeaks(data,threshold = 0.5,minpeakdistance = 10))[1]>2,mean(diff(findpeaks(data,threshold = 0.5,minpeakdistance = 10)[,2])),0),0)
  
  p_interval_std = ifelse(!is.null(findpeaks(data,threshold = 0.5,minpeakdistance = 10)),ifelse(dim(findpeaks(data,threshold = 0.5,minpeakdistance = 10))[1]>2,std(diff(findpeaks(data,threshold = 0.5,minpeakdistance = 10)[,2])),0),0)
  
  p_mean = ifelse(!is.null(findpeaks(data,threshold = 0.5,minpeakdistance = 10)),mean(findpeaks(data,threshold = 0.5,minpeakdistance = 10)[,1]),0)
  
  p_max = ifelse(!is.null(findpeaks(data,threshold = 0.5,minpeakdistance = 10)),max(findpeaks(data,threshold = 0.5,minpeakdistance = 10)[,1]),0)
  
  p_min = ifelse(!is.null(findpeaks(data,threshold = 0.5,minpeakdistance = 10)),min(findpeaks(data,threshold = 0.5,minpeakdistance = 10)[,1]),0)
  
  p_std = ifelse(!is.null(findpeaks(data,threshold = 0.5,minpeakdistance = 10)),std(findpeaks(data,threshold = 0.5,minpeakdistance = 10)[,1]),0)
  
  peak_data = c(p = p, p_interval_mean = p_interval_mean, p_interval_std = p_interval_std, p_mean = p_mean, p_max = p_max, p_min = p_min, p_std = p_std)
  
}


volatile_peak = data.frame()

for(case_id in unique(vital_volatile_total$Case_ID)){
  
  data = data.frame(subset(vital_volatile_total, vital_volatile_total$Case_ID==case_id))
  
  data_peak = c()
  
  for(col_name in col_){
    
    if(length(data[,col_name][which(data[,col_name]==0)]) == nrow(data)){
      data_peak_sum = data.frame(p = 0, p_interval_mean = 0, p_interval_std = 0, p_mean = 0, p_max = 0, p_min = 0, p_std = 0)
    }
    
    else{
      
      data_peak_sum = data[,col_name][which(data[,col_name]!=0)[1]:which(data[,col_name]!=0)[length(which(data[,col_name]!=0))]] %>% peak_f
      
    }
    
    data_peak = c(data_peak, data_peak_sum) 
  }
  
  volatile_peak = rbind(volatile_peak,data.frame(matrix(unlist(data_peak),nrow = 1),Case_ID = case_id))
  
}

nrow(volatile_peak)
names(volatile_peak)[1:length(volatile_peak)-1] = paste0("peak_",names(volatile_peak)[1:length(volatile_peak)-1])
names(volatile_peak)

saveRDS(volatile_peak, "volatile_peak_mpd10_0121.rds")

#######################################################
#----------------파고율 특징 추출-----------------------
#######################################################
volatile_cre = data.frame()

for(case_id in unique(vital_volatile_total$Case_ID)){
  
  data = data.frame(subset(vital_volatile_total, vital_volatile_total$Case_ID==case_id))
  
  data_cre = c()
  
  for(col_name in col_){
    
    if(length(data[,col_name][which(data[,col_name]==0)]) == nrow(data)){
      data_cre_sum = 0
    }
    
    else{
      
      data_cre_sum = crest(data[,col_name][which(data[,col_name]!=0)[1]:which(data[,col_name]!=0)[length(which(data[,col_name]!=0))]],1)$C
      
    }
    
    data_cre = c(data_cre, data_cre_sum) 
    
  }
  
  volatile_cre = rbind(volatile_cre,data.frame(matrix(unlist(data_cre),nrow = 1),Case_ID = case_id))
  
}

nrow(volatile_cre)
names(volatile_cre)[1:length(volatile_cre)-1] = paste0("cre_",names(volatile_cre)[1:length(volatile_cre)-1])
names(volatile_cre)
saveRDS(volatile_cre, "volatile_cre_0121.rds")


#######################################################
#----------------주파수 특징 추출-----------------------
#######################################################

# 상위 파워를 가진 frequency를 3개 적용 
volatile_freq = data.frame()

for(case_id in unique(vital_volatile_total$Case_ID)){
  
  data = data.frame(subset(vital_volatile_total, vital_volatile_total$Case_ID==case_id))
  
  data_freq = c()
  
  for(col_name in col_){
    
    if(length(data[,col_name][which(data[,col_name]==0)]) == nrow(data)){
      data_freq_sum = data.frame(V1 = 0, V2 = 0, V3 = 0, V4 = 0, V5 = 0, V6 = 0)
    }
    
    else if(min(data[,col_name][which(data[,col_name]!=0)[1]:which(data[,col_name]!=0)[length(which(data[,col_name]!=0))]])== max(data[,col_name][which(data[,col_name]!=0)[1]:which(data[,col_name]!=0)[length(which(data[,col_name]!=0))]])){
      data_freq_sum = data.frame(V1 = 0, V2 = 0, V3 = 0, V4 = 0, V5 = 0, V6 = 0)
      
    }
    
    else{
      
      volatile_spec = spectrum(data[,col_name][which(data[,col_name]!=0)[1]:which(data[,col_name]!=0)[length(which(data[,col_name]!=0))]], span = 5)
      
      rslt = data.frame(r.freq = volatile_spec$freq, r.spec = volatile_spec$spec)
      rslt = arrange(rslt, desc(r.spec))
      
      data_freq_sum = as.data.frame(t(c(rslt[1:3,1],rslt[1:3,2])),strinAsFactors=FALSE)   # 상위 frequency 3개 사용 
      
    }
    
    data_freq = c(data_freq, data_freq_sum) 
    
  }
  
  volatile_freq = rbind(volatile_freq,data.frame(matrix(unlist(data_freq),nrow = 1), Case_id = case_id))
  
}

# 변수 총 7개 * 6 = 42 + 1 -> 43개의 변수 
str(volatile_freq)

# 문자열 변수 숫자로 변환 
conv = function(x){
  as.numeric(as.character(x))  
}


volatile_freq = cbind(volatile_freq%>%select(Case_id),as.data.frame(sapply(volatile_freq%>%select(-Case_id),conv)))

# 이름 변경 
names(volatile_freq)[2:length(volatile_freq)] = paste0("freq_",names(volatile_freq)[2:length(volatile_freq)])
names(volatile_freq)

saveRDS(volatile_freq, "volatile_freq_0121.rds")


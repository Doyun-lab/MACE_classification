setwd("E:/")

library(signal)
library(fBasics)
library(seewave)
library(pracma)

library(dplyr)

emr_TIVA = readRDS('preprocessing/emr_TIVA.rds')
names(emr_TIVA)[1] = "Case_ID"
table(emr_TIVA$OP.Time, useNA = "ifany")  # 수술시간이 잘못된 것: 3개. 

vital_TIVA_total = readRDS('preprocessing\\vital_TIVA_NA_END.rds')
names(vital_TIVA_total)

emr_vital_tiva = vital_TIVA_total%>%left_join(emr_TIVA,by='Case_ID')

length(unique(emr_vital_tiva$Case_ID))

unique(emr_vital_tiva[emr_vital_tiva$OP.Time ==0,]$Case_ID)  # VR과 겹치는 case = 0

# 수술시간이 0인 부분 vr의 time변수를 통해 전처리 
# emr_TIVA[emr_TIVA$OP.Time==0,]$OP.Time = round(difftime(emr_vital_tiva[emr_vital_tiva$OP.Time==0,]$Time[nrow(emr_vital_tiva[emr_vital_tiva$OP.Time==0,])],emr_vital_tiva[emr_vital_tiva$OP.Time==0,]$Time[1],units="mins"))

# table(emr_TIVA$OP.Time)

nrow(emr_vital_tiva)
head(emr_vital_tiva)
str(emr_vital_tiva)

names(emr_vital_tiva)

summary(emr_TIVA)

##################################################
#---------모든 레코드에 동일한 값 처리------------
##################################################
numeric_name = names(unlist(lapply(emr_TIVA, class)[lapply(emr_TIVA, class)%in%c("integer","numeric")]))
numeric_name = numeric_name[-1]

colSums(is.na(emr_TIVA))

same_name = c()

# 모든레코드에 동일한 값 확인 
for(name in numeric_name){
  if(min(emr_TIVA[name])==max(emr_TIVA[name])){
    same_name = c(same_name,name)  
  }
}

emr_TIVA = emr_TIVA[,!names(emr_TIVA)%in%same_name]


##################################################
#-----------------통계 특징 추출-----------------
##################################################

rss = function(x) rms(x)*sqrt(length(x))
col_ = names(vital_TIVA_total)[c(1,4:9)]  # Time, Case_id를 제외하고 특징을 추출할 변수들 

data = vital_TIVA_total[vital_TIVA_total$Case_ID == unique(vital_TIVA_total$Case_ID)[2],]
summary(data)

TIVA_summary = data.frame()

for(case_id in unique(vital_TIVA_total$Case_ID)){
  
  data = data.frame(subset(vital_TIVA_total, vital_TIVA_total$Case_ID==case_id))
  
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
  
  TIVA_summary = rbind(TIVA_summary,data.frame(matrix(unlist(data_sum),nrow = 1),Case_ID = case_id))
  
}

str(TIVA_summary)
nrow(TIVA_summary)
names(TIVA_summary)[1:length(TIVA_summary)-1] = paste0("st_",names(TIVA_summary)[1:length(TIVA_summary)-1])
names(TIVA_summary)

saveRDS(TIVA_summary,"preprocessing/TIVA_summary.rds")

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
  
  cpts_data = c(cpt_var = cpt_var, cpt_mean =cpt_mean, cpt_meanvar = cpt_meanvar)
  
  return(cpts_data)
  
}

TIVA_cpt = data.frame()

for(case_id in unique(vital_TIVA_total$Case_ID)){
  
  data = data.frame(subset(vital_TIVA_total, vital_TIVA_total$Case_ID==case_id))
  
  data_cpt = c()
  
  for(col_name in col_){
    
    if(length(data[,col_name][which(data[,col_name]==0)]) == nrow(data)){
      data_cpt_sum = data.frame(cpt_var = 0, cpt_mean = 0, cpt_meanvar = 0)
    }
    
    else if(length(unique(data[,col_name][which(data[,col_name]!=0)[1]:which(data[,col_name]!=0)[length(which(data[,col_name]!=0))]]))==1){
      
      data_cpt_sum = data.frame(cpt_var = 0, cpt_mean = 0, cpt_meanvar = 0)
      
    }
    else{
      
      data_cpt_sum = data[,col_name][which(data[,col_name]!=0)[1]:which(data[,col_name]!=0)[length(which(data[,col_name]!=0))]] %>% cpt_f()
      
    }
    
    data_cpt = c(data_cpt, data_cpt_sum) 
  }
  
  TIVA_cpt = rbind(TIVA_cpt,data.frame(matrix(unlist(data_cpt),nrow = 1),Case_ID = case_id))
  
}

names(TIVA_cpt)[1:length(TIVA_cpt)-1] = paste0("cpt_",names(TIVA_cpt)[1:length(TIVA_cpt)-1])
names(TIVA_cpt)
saveRDS(TIVA_cpt, "preprocessing/TIVA_cpt.rds")

#######################################################
#----------------PEAK 특징 추출-----------------------
#######################################################

peak_f = function(data){
  
  p = ifelse(!is.null(findpeaks(data,threshold = 0.5)),dim(findpeaks(data,threshold = 0.5))[1],0)
  
  p_interval = ifelse(!is.null(findpeaks(data,threshold = 0.5)),ifelse(dim(findpeaks(data,threshold = 0.5))[1]>2,mean(diff(findpeaks(data,threshold = 0.5)[,2])),0),0)
  
  p_interval_std = ifelse(!is.null(findpeaks(data,threshold = 0.5)),ifelse(dim(findpeaks(data,threshold = 0.5))[1]>2,std(diff(findpeaks(data,threshold = 0.5)[,2])),0),0)
  
  p_mean = ifelse(!is.null(findpeaks(data,threshold = 0.5)),mean(findpeaks(data,threshold = 0.5)[,1]),0)
  
  p_max = ifelse(!is.null(findpeaks(data,threshold = 0.5)),max(findpeaks(data,threshold = 0.5)[,1]),0)
  
  p_min = ifelse(!is.null(findpeaks(data,threshold = 0.5)),min(findpeaks(data,threshold = 0.5)[,1]),0)
  
  p_std = ifelse(!is.null(findpeaks(data,threshold = 0.5)),std(findpeaks(data,threshold = 0.5)[,1]),0)
  
  peak_data = c(p = p, p_interval = p_interval, p_interval_std = p_interval_std, p_mean = p_mean, p_max = p_max, p_min = p_min, p_std = p_std)
  
}


TIVA_peak = data.frame()

for(case_id in unique(vital_TIVA_total$Case_ID)){
  
  data = data.frame(subset(vital_TIVA_total, vital_TIVA_total$Case_ID==case_id))
  
  data_peak = c()
  
  for(col_name in col_){
    
    if(length(data[,col_name][which(data[,col_name]==0)]) == nrow(data)){
      data_peak_sum = data.frame(p = 0, p_interval = 0, p_interval_std = 0, p_mean = 0, p_max = 0, p_min = 0, p_std = 0)
    }
    
    else{
      
      data_peak_sum = data[,col_name][which(data[,col_name]!=0)[1]:which(data[,col_name]!=0)[length(which(data[,col_name]!=0))]] %>% peak_f
      
    }
    
    data_peak = c(data_peak, data_peak_sum) 
  }
  
  TIVA_peak = rbind(TIVA_peak,data.frame(matrix(unlist(data_peak),nrow = 1),Case_ID = case_id))
  
}

nrow(TIVA_peak)
names(TIVA_peak)[1:length(TIVA_peak)-1] = paste0("peak_",names(TIVA_peak)[1:length(TIVA_peak)-1])
names(TIVA_peak)

saveRDS(TIVA_peak, "preprocessing/TIVA_peak.rds")

#######################################################
#----------------파고율 특징 추출-----------------------
#######################################################
TIVA_cre = data.frame()

for(case_id in unique(vital_TIVA_total$Case_ID)){
  
  data = data.frame(subset(vital_TIVA_total, vital_TIVA_total$Case_ID==case_id))
  
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
  
  TIVA_cre = rbind(TIVA_cre,data.frame(matrix(unlist(data_cre),nrow = 1),Case_ID = case_id))
  
}

names(TIVA_cre) = c("cre1","cre2","cre3","cre4","cre5","cre6","cre7",'Case_ID')
saveRDS(TIVA_cre, "preprocessing/TIVA_cre.rds")


#######################################################
#----------------주파수 특징 추출-----------------------
#######################################################

# 상위 파워를 가진 frequency를 3개 적용 
TIVA_freq = data.frame()

for(case_id in unique(vital_TIVA_total$Case_ID)){
  
  data = data.frame(subset(vital_TIVA_total, vital_TIVA_total$Case_ID==case_id))
  
  data_freq = c()
  
  for(col_name in col_){
    
    if(length(data[,col_name][which(data[,col_name]==0)]) == nrow(data)){
      data_freq_sum = data.frame(V1 = 0, V2 = 0, V3 = 0, V4 = 0, V5 = 0, V6 = 0)
    }
    
    else if(min(data[,col_name][which(data[,col_name]!=0)[1]:which(data[,col_name]!=0)[length(which(data[,col_name]!=0))]])== max(data[,col_name][which(data[,col_name]!=0)[1]:which(data[,col_name]!=0)[length(which(data[,col_name]!=0))]])){
      data_freq_sum = data.frame(V1 = 0, V2 = 0, V3 = 0, V4 = 0, V5 = 0, V6 = 0)
      
    }
    
    else{
      
      tiva_spec = spectrum(data[,col_name][which(data[,col_name]!=0)[1]:which(data[,col_name]!=0)[length(which(data[,col_name]!=0))]], span = 5)
      
      rslt = data.frame(r.freq = tiva_spec$freq, r.spec = tiva_spec$spec)
      rslt = arrange(rslt, desc(r.spec))
      
      data_freq_sum = as.data.frame(t(c(rslt[1:3,1],rslt[1:3,2])),strinAsFactors=FALSE)   # 상위 frequency 3개 사용 
      
    }
    
    data_freq = c(data_freq, data_freq_sum) 
    
  }
  
  TIVA_freq = rbind(TIVA_freq,data.frame(matrix(unlist(data_freq),nrow = 1), Case_id = case_id))
  
}

# 변수 총 7개 * 6 = 42 + 1 -> 43개의 변수 
str(TIVA_freq)

# 문자열 변수 숫자로 변환 
conv = function(x){
  as.numeric(as.character(x))  
}


TIVA_freq = cbind(TIVA_freq%>%select(Case_id),as.data.frame(sapply(TIVA_freq%>%select(-Case_id),conv)))

# 이름 변경 
names(TIVA_freq)[2:length(TIVA_freq)] = paste0("freq_",names(TIVA_freq)[2:length(TIVA_freq)])
names(TIVA_freq)

saveRDS(TIVA_freq, "preprocessing/TIVA_freq.rds")

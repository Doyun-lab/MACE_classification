setwd("E:/")

library(signal)
library(fBasics)
library(seewave)
library(pracma)
library(dplyr)
library(changepoint)

TIVA_ans_bp = readRDS('preprocessing\\TIVA_ans_bp')
names(TIVA_ans_bp)
length(unique(TIVA_ans_bp))
summary(TIVA_ans_bp)

TIVA_ans_bp = TIVA_ans_bp[,c(1:5,11)]
head(TIVA_ans_bp)

TIVA_ans_bp$item = sapply(str_split(TIVA_ans_bp$item," "), function(x){x[1]})
TIVA_ans_bp$value1 = as.numeric(TIVA_ans_bp$value1)

TIVA_ans_bp = as.data.frame(TIVA_ans_bp)
table(is.na(TIVA_ans_bp))

col_ = c("SBP","DBP","MBP")

case_check = c()

for (case_id in unique(TIVA_ans_bp$Case_ID)){
  
  data = as.data.table(TIVA_ans_bp)[Case_ID == case_id]
  
  if (length(unique(data$item))!=3){
    
    case_check = c(case_check,case_id)
  }
  
}

case_check

TIVA_ans_bp = TIVA_ans_bp[!case_id%in%case_check,]


##################################################
#-----------------통계 특징 추출-----------------
##################################################

rss = function(x) rms(x)*sqrt(length(x))

TIVA_summary = data.frame()

count = 1

for(case_id in unique(TIVA_ans_bp$Case_ID)){
  print(case_id)
  print(count)
  count = count + 1
  data = data.table(subset(TIVA_ans_bp, TIVA_ans_bp$Case_ID==case_id))
  
  data_sum = c()
  
  for(item_name in col_){
    
    if(length(data[item == item_name,value1])==0){
      
      data_summary = data.frame(mean = 0, sd = 0, rms = 0, rss = 0, IQR = 0)
      names(data_summary) = paste0(item_name,"_",names(data_summary))
    }
    
    else{
      
      data_summary = cbind(data.frame(value = data[item == item_name,value1]) %>% summarise_each(funs(mean, sd, rms, rss, IQR)))
      names(data_summary) = paste0(item_name,"_",names(data_summary))
      
    }
    
    data_sum = data.frame(c(data_sum, data_summary))
    
  }
  
  TIVA_summary = rbind(TIVA_summary,c(data_sum, Case_ID = case_id))
  
}

str(TIVA_summary)
nrow(TIVA_summary)
head(TIVA_summary)

length(unique(TIVA_summary$Case_ID))
length(unique(TIVA_ans_bp$Case_ID))


saveRDS(TIVA_summary,"TIVA_summary_bp.rds")

#######################################################
#----------------변화하는 구간 추출--------------------
#######################################################

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

TIVA_cpt = data.frame()

for(case_id in unique(TIVA_ans_bp$Case_ID)){
  
  print(case_id)
  data = data.table(subset(TIVA_ans_bp, TIVA_ans_bp$Case_ID==case_id))
  
  data_cpt = c()
  
  for(item_name in col_){
    print(item_name)
    if(length(data[item == item_name,value1])<4){  # 개수가 4보다 작으면 셀 수 없음.
      
      check = rbind(check,c(case_id, item_name))
      
      data_cpt_sum = data.frame(cpt_var = 0, cpt_mean = 0, cpt_meanvar = 0)

    }
    
    # else if(length(unique(round(data[,col_name][which(data[item == item_name,value1]!=0)[1]:which(data[,col_name]!=0)[length(which(data[,col_name]!=0))]],2)))==1){
    #   
    #   data_cpt_sum = data.frame(cpt_var = 0, cpt_mean = 0, cpt_meanvar = 0)
    #   
    # }
    
    else{
      
      data_cpt_sum = data[item == item_name,value1] %>% cpt_f()
      print(data_cpt_sum)
      names(data_cpt_sum) = paste0(item_name,"_",names(data_cpt_sum))
      names(data_cpt_sum)
      
    }
    
    data_cpt = c(data_cpt, data.frame(matrix(c(unlist(data_cpt_sum)),nrow = 1)))
    

  }
  
  names(data_cpt) = c("SBP_cpt_var","SBP_cpt_mean","SBP_cpt_meanvar","DBP_cpt_var","DBP_cpt_mean","DBP_cpt_meanvar","MBP_cpt_var","MBP_cpt_mean","MBP_cpt_meanvar")
  
  TIVA_cpt = rbind(TIVA_cpt,data.frame(data_cpt,Case_ID = case_id))
}


head(TIVA_cpt)
length(unique(TIVA_cpt$Case_ID))
check  # 급격하게 변하는 시점 추출할 수 없는 경우 

saveRDS(TIVA_cpt, "TIVA_cpt_bp.rds")

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


TIVA_peak = data.frame()

for(case_id in unique(vital_TIVA_total$Case_ID)){
  
  data = data.frame(subset(vital_TIVA_total, vital_TIVA_total$Case_ID==case_id))
  
  data_peak = c()
  
  for(item_name in col_){
    
    if(length(data[,item_name][which(data[,item_name]==0)]) == nrow(data)){
      data_peak_sum = data.frame(p = 0, p_interval_mean = 0, p_interval_std = 0, p_mean = 0, p_max = 0, p_min = 0, p_std = 0)
    }
    
    else{
      
      data_peak_sum = data[,item_name][which(data[,item_name]!=0)[1]:which(data[,item_name]!=0)[length(which(data[,item_name]!=0))]] %>% peak_f
      
    }
    
    data_peak = c(data_peak, data_peak_sum) 
  }
  
  TIVA_peak = rbind(TIVA_peak,data.frame(matrix(unlist(data_peak),nrow = 1),Case_ID = case_id))
  
}

nrow(TIVA_peak)
names(TIVA_peak)[1:length(TIVA_peak)-1] = paste0("peak_",names(TIVA_peak)[1:length(TIVA_peak)-1])
names(TIVA_peak)

saveRDS(TIVA_peak, "preprocessing/TIVA_peak_mpd10_0122.rds")

#######################################################
#----------------파고율 특징 추출-----------------------
#######################################################
TIVA_cre = data.frame()

for(case_id in unique(TIVA_ans_bp$Case_ID)){
  
  data = data.table(subset(TIVA_ans_bp, TIVA_ans_bp$Case_ID==case_id))
  
  data_cre = c()
  
  for(item_name in col_){
    
    if(length(data[item == item_name,value1])==0){
      
      data_cre_sum = 0
    }
    
    else{
      
      data_cre_sum = crest(data[item == item_name, value1],1)$C
      
    }
    
    data_cre = c(data_cre, data_cre_sum) 
    
  }
  
  TIVA_cre = rbind(TIVA_cre,data.frame(matrix(unlist(data_cre),nrow=1),Case_ID = case_id))
  
}

names(TIVA_cre) = c("SBP_cre","DBP_cre","MBP_cre","Case_ID")

nrow(TIVA_cre)
head(TIVA_cre)

saveRDS(TIVA_cre, "preprcessing/TIVA_cre_bp.rds")


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
      
      tiva_spec = spectrum(data[,col_name][which(data[,col_name]!=0)[1]:which(data[,col_name]!=0)[length(which(data[,col_name]!=0))]])
      
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

saveRDS(TIVA_freq, "TIVA_freq_0122.rds")


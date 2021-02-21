setwd("E:/")

library(signal)
library(fBasics)
library(seewave)
library(pracma)
library(dplyr)
library(changepoint)

volatile_ans_bp = readRDS('preprocessing\\volatile_ans_bp')
names(volatile_ans_bp)
length(unique(volatile_ans_bp))
summary(volatile_ans_bp)

volatile_ans_bp = volatile_ans_bp[,c(1:5,11)]
head(volatile_ans_bp)

volatile_ans_bp$item = sapply(str_split(volatile_ans_bp$item," "), function(x){x[1]})
volatile_ans_bp$value1 = as.numeric(volatile_ans_bp$value1)

volatile_ans_bp = as.data.frame(volatile_ans_bp)
table(is.na(volatile_ans_bp))

col_ = c("SBP","DBP","MBP")

case_check = c()

for (case_id in unique(volatile_ans_bp$Case_ID)){
  
  data = as.data.table(volatile_ans_bp)[Case_ID == case_id]
  
  if (length(unique(data$item))!=3){
    
    case_check = c(case_check,case_id)
  }
  
}

case_check

volatile_ans_bp = volatile_ans_bp[!case_id%in%case_check,]


##################################################
#-----------------통계 특징 추출-----------------
##################################################

rss = function(x) rms(x)*sqrt(length(x))

volatile_summary = data.frame()

count = 1

for(case_id in unique(volatile_ans_bp$Case_ID)){
  print(case_id)
  print(count)
  count = count + 1
  data = data.table(subset(volatile_ans_bp, volatile_ans_bp$Case_ID==case_id))
  
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
  
  volatile_summary = rbind(volatile_summary,c(data_sum, Case_ID = case_id))
  
}

str(volatile_summary)
nrow(volatile_summary)
head(volatile_summary)

length(unique(volatile_summary$Case_ID))
length(unique(volatile_ans_bp$Case_ID))


saveRDS(volatile_summary,"preprocessing/volatile_summary_bp.rds")

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

volatile_cpt = data.frame()

for(case_id in unique(volatile_ans_bp$Case_ID)){
  
  print(case_id)
  data = data.table(subset(volatile_ans_bp, volatile_ans_bp$Case_ID==case_id))
  
  data_cpt = c()
  
  for(item_name in col_){
    print(item_name)
    if(length(data[item == item_name,value1])<4){  # 개수가 4보다 작으면 셀 수 없음.
      
      check = rbind(check,c(case_id, item_name))
      
      data_cpt_sum = data.frame(cpt_var = 0, cpt_mean = 0, cpt_meanvar = 0)
      
    }
    
    
    else{
      
      data_cpt_sum = data[item == item_name,value1] %>% cpt_f()
      print(data_cpt_sum)
      names(data_cpt_sum) = paste0(item_name,"_",names(data_cpt_sum))
      names(data_cpt_sum)
      
    }
    
    data_cpt = c(data_cpt, data.frame(matrix(c(unlist(data_cpt_sum)),nrow = 1)))
    
    
  }
  
  names(data_cpt) = c("SBP_cpt_var","SBP_cpt_mean","SBP_cpt_meanvar","DBP_cpt_var","DBP_cpt_mean","DBP_cpt_meanvar","MBP_cpt_var","MBP_cpt_mean","MBP_cpt_meanvar")
  
  volatile_cpt = rbind(volatile_cpt,data.frame(data_cpt,Case_ID = case_id))
}


head(volatile_cpt)
length(unique(volatile_cpt$Case_ID))
check  # 급격하게 변하는 시점 추출할 수 없는 경우  # 19개  

saveRDS(volatile_cpt, "volatile_cpt_bp.rds")


#######################################################
#----------------파고율 특징 추출-----------------------
#######################################################
volatile_cre = data.frame()

for(case_id in unique(volatile_ans_bp$Case_ID)){
  
  data = data.table(subset(volatile_ans_bp, volatile_ans_bp$Case_ID==case_id))
  
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
  
  volatile_cre = rbind(volatile_cre,data.frame(matrix(unlist(data_cre),nrow=1),Case_ID = case_id))
  
}

names(volatile_cre) = c("SBP_cre","DBP_cre","MBP_cre","Case_ID")

nrow(volatile_cre)
head(volatile_cre)

saveRDS(volatile_cre, "preprocessing/volatile_cre_bp.rds")

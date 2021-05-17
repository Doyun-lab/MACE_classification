library(e1071)
library(caret)
library(NLP)
library(tm)
library(dplyr)
library(dummies)

setwd("E://preprocessing")

set.seed(20171490)

# tiva
emr_TIVA = readRDS("emr_TIVA.rds")
nrow(emr_TIVA)

head(emr_TIVA)
names(emr_TIVA)
names(emr_TIVA)[1] = "Case_ID"

summary(emr_TIVA)

TIVA_summary = readRDS("TIVA_summary.rds")
TIVA_cpt = readRDS("TIVA_cpt.rds")
TIVA_peak = readRDS("TIVA_peak_mpd10.rds")
TIVA_cre = readRDS("TIVA_cre.rds")
TIVA_freq = readRDS("TIVA_freq.rds")

names(TIVA_cre)
names(TIVA_cpt)
names(TIVA_peak)
names(TIVA_freq)[1] = "Case_ID"

# volatile
emr_volatile = readRDS("emr_volatile.rds")

head(emr_volatile)
names(emr_volatile)
names(emr_volatile)[1] = "Case_ID"

volatile_summary = readRDS("volatile_summary.rds")
volatile_cpt = readRDS("volatile_cpt.rds")   # pelt 사용 시 성능 저하 
volatile_peak = readRDS("volatile_peak_mpd10.rds")
volatile_cre = readRDS("volatile_cre.rds")
volatile_freq = readRDS("volatile_freq.rds")

names(volatile_cre)
names(volatile_cpt)
names(volatile_peak)
names(volatile_freq)[1] = "Case_ID"

# 모델링 코드 

model = function(total_data){
  
  set.seed(12546)
  
  total_data$class = as.character(total_data$class)
  
  total_data$ANE.Time = NULL
  total_data$OP.Time = NULL
  total_data$ASA = as.numeric(total_data$ASA)
  
  total_data$class[total_data$class=="NORMAL"] = "TRUE"
  total_data$class[total_data$class!="TRUE"] = "FALSE"
  
  char_name = names(unlist(lapply(total_data, class)[lapply(total_data, class)%in%c("factor","character")]))
  
  total_data[is.na(total_data)] = 0
  
  # OP_Score로 인해 OP_Severity 삭제해도됨. 
  except_name = c("Case_ID","OP.Name","OP.Name_re","EKG","OP_surgery_type","OP_Severity")
  total_data2 = total_data[,!names(total_data)%in%except_name]
  
  total_data2 = total_data2 %>% mutate_if(is.character, as.factor)
  
  ##################################################
  #---------모든 레코드에 동일한 값 처리------------
  ##################################################
  same_name = c()
  
  # 모든레코드에 동일한 값 확인 
  for(name in names(total_data2)){
    if(length(unique(total_data2[,name]))==1){
      same_name = c(same_name,name)  
    }
  }
  
  
  total_data2 = total_data2[,!names(total_data2)%in%same_name]
  
  # facotr형으로 변경해야되는 변수들 확인(기저질환 변수)
  name_ = c()
  col_factor_feature = data.frame()
  
  for(col_ in names(total_data2)){
    df = data.frame(col_, count = length(unique(data.frame(total_data2)[,col_])))
    
    col_factor_feature = rbind(col_factor_feature,df)
  }
  
  factor_col = as.character(subset(col_factor_feature, col_factor_feature$count == 2)$col_)
  
  factor_col2 = factor_col[!str_detect(factor_col,paste0(c("class","ASA","peak_","freq_","cpt_","cre_"), collapse = "|"))]
  
  total_data2[,factor_col2] = lapply(total_data2[,factor_col2], as.factor)
  
  # factor, 문자열 변수 따로 추출 
  char_name_new = names(unlist(lapply(total_data2, class)[lapply(total_data2, class)%in%c("factor","character")]))
  char_name_new = char_name_new[-grep("class",char_name_new)] # class 위치 제외 
  
  
  # 더미변수 생성
  total_data3 = total_data2[,!names(total_data2)%in%char_name_new]
  
  
  df = c()
  
  for (col_name in char_name_new){
    
    df = cbind(df, dummy(total_data2[,col_name],sep='_'))
    
  }
  
  total_data3 = cbind(total_data3, df)
  
  total_data3$class = as.character(total_data3$class)
  
  
  data_TRUE=subset(total_data3,class=="TRUE")
  data_FALSE=subset(total_data3,class=="FALSE")
  
  # 데이터셋 분리 
  inTrain=createDataPartition(1:nrow(data_TRUE),p=0.8,list=FALSE)
  
  
  # 특징만 추출해서 하려면 labeling으로 특징이 있는 열만 입력해야함. 
  train_x = data_TRUE[inTrain,-grep("class",names(data_TRUE))]
  
  # 모든 변수에 값은 값이 들어가면 scale할 수가 없어서 에러가 발생하기 때문에 해당 변수 삭제 
  same_name_train = c()
  
  for(name in names(train_x)){
    if(min(train_x[name])==max(train_x[name])){
      same_name_train = c(same_name_train,name)  
    }
  }
  
  train_x = train_x[,!names(train_x)%in%same_name_train]
  
  train_y = data_TRUE[inTrain,"class"]
  
  test_TRUE = data_TRUE[-inTrain,]
  test=rbind(test_TRUE,data_FALSE)
  test_x = test[,-grep("class",names(test))]
  test_x = test_x[,!names(test_x)%in%same_name_train]
  test_y = test[,"class"]
  
  svm.model=svm(train_x,y=NULL,
                type='one-classification',
                nu=0.10,
                scale = TRUE,
                kernel = "radial")
  
  
  svm.predtrain=predict(svm.model,train_x)
  svm.predtest=predict(svm.model,test_x)
  
  confTrain=table(Predicted=svm.predtrain,Reference=train_y)
  confTest=table(Predicted=svm.predtest,Reference=test_y)
  
  accuracy = mean(svm.predtest==test_y)
  
  sensi = sensitivity(as.factor(svm.predtest), as.factor(test_y),positive = levels(as.factor(test_y))[2])
  
  specif = specificity(as.factor(svm.predtest), as.factor(test_y), negative = levels(as.factor(test_y))[1])
  
  total_result = c(accuracy, sensi, specif)
  
  return(total_result)
  
  
}


# 특징 조합에 따른 성능 확인 (volatile)
total_data_list = c("volatile_summary", "volatile_cpt", "volatile_cre", "volatile_peak", "volatile_freq")

total_result = c()
row_name = c()

for(i in 1:5){
  comb = combn(total_data_list,i)
  print(i)
  
  if(i==1){
    for (z in 1:(length(comb)/i)){
      row_name = c(row_name,comb[,z])
      
      total_data = emr_volatile%>%inner_join(get(comb[,z]), by = "Case_ID")
      total_result = rbind(total_result, model(total_data))
    }
    
  }
  
  if(i==2){
    for (z in 1:(length(comb)/i)){
      row_name = c(row_name,paste0(comb[1,z]," + ",comb[2,z]))
      total_data = emr_volatile%>%inner_join(get(comb[1,z]), by = "Case_ID")%>%inner_join(get(comb[2,z]), by = "Case_ID")
      total_result = rbind(total_result, model(total_data))
    }
  }
  
  if(i==3){
    for (z in 1:(length(comb)/i)){
      row_name = c(row_name,paste0(comb[1,z]," + ",comb[2,z]," + ",comb[3,z]))
      total_data = emr_volatile%>%inner_join(get(comb[1,z]), by = "Case_ID")%>%inner_join(get(comb[2,z]), by = "Case_ID")%>%inner_join(get(comb[3,z]), by = "Case_ID")
      total_result = rbind(total_result, model(total_data))
    }
  }
  
  if(i==4){
    for (z in 1:(length(comb)/i)){
      row_name = c(row_name,paste0(comb[1,z]," + ",comb[2,z]," + ",comb[3,z],"+",comb[4,z]))
      total_data = emr_volatile%>%inner_join(get(comb[1,z]), by = "Case_ID")%>%inner_join(get(comb[2,z]), by = "Case_ID")%>%inner_join(get(comb[3,z]), by = "Case_ID")%>%inner_join(get(comb[4,z]), by = "Case_ID")
      total_result = rbind(total_result, model(total_data))
    }
  }
  
  if(i==5){
    for (z in 1:(length(comb)/i)){
      row_name = c(row_name,paste0(comb[1,z]," + ",comb[2,z]," + ",comb[3,z],"+",comb[4,z]," + ",comb[5,z]))
      total_data = emr_volatile%>%inner_join(get(comb[1,z]), by = "Case_ID")%>%inner_join(get(comb[2,z]), by = "Case_ID")%>%inner_join(get(comb[3,z]), by = "Case_ID")%>%inner_join(get(comb[4,z]), by = "Case_ID")%>%inner_join(get(comb[5,z]), by = "Case_ID")
      
      total_result = rbind(total_result, model(total_data))
    }
  }
}

row.names(total_result) = row_name
colnames(total_result) = c("Accuracy","Sensitivity","Specificity")

nrow(total_result)
length(unique(row.names(total_result)))

# total_result의 행이름을 열로 생성 
use_data = row.names(total_result)
row.names(total_result) = NULL

# numeric으로 변환 후 수치를 비교하기 위해 데이터 프레임으로 변경 
result = cbind(use_data, total_result)
head(data.frame(result))

result= data.frame(result)
str(result)
result[,c(2:4)] = apply(result[,c(2:4)], 2, as.numeric)

volatile_result_freq_revise = result %>% arrange(desc(Specificity),desc(Sensitivity))

write.csv(volatile_result_freq_revise[volatile_result_freq_revise$Specificity>0.5,],"volatile_result_default.csv")



# 특징 조합에 따른 성능 확인 (volatile)
total_data_list = c("TIVA_summary", "TIVA_cpt", "TIVA_cre", "TIVA_peak", "TIVA_freq")

total_result_tiva = c()
row_name = c()

for(i in 1:5){
  comb = combn(total_data_list,i)
  print(i)
  
  if(i==1){
    for (z in 1:(length(comb)/i)){
      row_name = c(row_name,comb[,z])
      
      total_data = emr_TIVA%>%inner_join(get(comb[,z]), by = "Case_ID")
      total_result_tiva = rbind(total_result_tiva, model(total_data))
    }
    
  }
  
  if(i==2){
    for (z in 1:(length(comb)/i)){
      row_name = c(row_name,paste0(comb[1,z]," + ",comb[2,z]))
      total_data = emr_TIVA%>%inner_join(get(comb[1,z]), by = "Case_ID")%>%inner_join(get(comb[2,z]), by = "Case_ID")
      total_result_tiva = rbind(total_result_tiva, model(total_data))
    }
  }
  
  if(i==3){
    for (z in 1:(length(comb)/i)){
      row_name = c(row_name,paste0(comb[1,z]," + ",comb[2,z]," + ",comb[3,z]))
      total_data = emr_TIVA%>%inner_join(get(comb[1,z]), by = "Case_ID")%>%inner_join(get(comb[2,z]), by = "Case_ID")%>%inner_join(get(comb[3,z]), by = "Case_ID")
      total_result_tiva = rbind(total_result_tiva, model(total_data))
    }
  }
  
  if(i==4){
    for (z in 1:(length(comb)/i)){
      row_name = c(row_name,paste0(comb[1,z]," + ",comb[2,z]," + ",comb[3,z],"+",comb[4,z]))
      total_data = emr_TIVA%>%inner_join(get(comb[1,z]), by = "Case_ID")%>%inner_join(get(comb[2,z]), by = "Case_ID")%>%inner_join(get(comb[3,z]), by = "Case_ID")%>%inner_join(get(comb[4,z]), by = "Case_ID")
      total_result_tiva = rbind(total_result_tiva, model(total_data))
    }
  }
  
  if(i==5){
    for (z in 1:(length(comb)/i)){
      row_name = c(row_name,paste0(comb[1,z]," + ",comb[2,z]," + ",comb[3,z],"+",comb[4,z]," + ",comb[5,z]))
      total_data = emr_TIVA%>%inner_join(get(comb[1,z]), by = "Case_ID")%>%inner_join(get(comb[2,z]), by = "Case_ID")%>%inner_join(get(comb[3,z]), by = "Case_ID")%>%inner_join(get(comb[4,z]), by = "Case_ID")%>%inner_join(get(comb[5,z]), by = "Case_ID")
      
      total_result_tiva = rbind(total_result_tiva, model(total_data))
    }
  }
}

row.names(total_result_tiva) = row_name
colnames(total_result_tiva) = c("Accuracy","Sensitivity","Specificity")

nrow(total_result_tiva)
length(unique(row.names(total_result_tiva)))

# total_result의 행이름을 열로 생성 
use_data = row.names(total_result_tiva)
row.names(total_result_tiva) = NULL

# numeric으로 변환 후 수치를 비교하기 위해 데이터 프레임으로 변경 
result = cbind(use_data, total_result_tiva)
head(data.frame(result))

result= data.frame(result)
str(result)
result[,c(2:4)] = apply(result[,c(2:4)], 2, as.numeric)

tiva_result_freq_revise = result %>% arrange(desc(Specificity),desc(Sensitivity))

write.csv(tiva_result_freq_revise[tiva_result_freq_revise$Specificity>0.7,],"tiva_result_default.csv")

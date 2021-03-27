library(e1071)
library(caret)
library(NLP)
library(tm)
library(dplyr)
library(dummies)

# Data load
emr_vol = readRDS('preprocessing/emr_volatile_0205.rds')
names(emr_vol)[1] = 'Case_ID'

vol_record_cre = readRDS('preprocessing/volatile_record_cre.rds')
vol_record_cpt = readRDS('preprocessing/volatile_record_cpt.rds')
vol_record_summary = readRDS('preprocessing/volatile_record_summary.rds')

vol_medi = readRDS('preprocessing/volatile_record_medi.rds')

# ----------------------------------------------------------------------------------------------------------------------------------------------
# EMR + 마취기록지 혈압 + 마취기록지 약품
vol_record <- vol_record_cpt %>%
  inner_join(vol_record_cre, by = 'Case_ID') %>%
  inner_join(vol_record_summary, by = 'Case_ID')

bp_cols <- c(grep('SBP', names(vol_record)), grep('MBP', names(vol_record)), grep('DBP', names(vol_record)))
vol_record_bp <- vol_record[, bp_cols]
vol_record_bp$Case_ID <- vol_record$Case_ID

total_data <- emr_vol %>%
  inner_join(vol_record, by = 'Case_ID') %>%
  inner_join(vol_medi, by = 'Case_ID')

# ----------------------------------------------------------------------------------------------------------------------------------------------
# 모델링 전 데이터 전처리
set.seed(12546)

total_data$Na.1 <- ifelse(total_data$Na.1 < 135 | total_data$Na.1 > 148, "1", "0")
total_data$K.1 <- ifelse(total_data$K.1 < 3.5 | total_data$K.1 > 5.1, "1", "0")
total_data$BUN.1 <- ifelse(total_data$BUN.1 < 9 | total_data$BUN.1 > 29, "1","0")
total_data$Cr.1 <- ifelse(total_data$Cr.1 < 0.4 | total_data$Cr.1 > 1.5, "1", "0")

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
  
  df = cbind(df, dummy(total_data2[,col_name],sep=col_name))
  
}

total_data3 = cbind(total_data3, df)

total_data3$class = as.character(total_data3$class)

nrow(total_data3)
table(total_data3$class)
# ----------------------------------------------------------------------------------------------------------------------------------------------

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
              nu=0.01,
              gamma = 0.01,
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
total_result

# ----------------------------------------------------------------------------------------------------------------------------------------------
nu.w <- c(0.0001, 0.001, 0.01, 0.1)
gamma.w <- c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 0.25, 0.5, 1)

result <- data.frame()
for(i in nu.w){
  for(j in gamma.w){
    
    svm.model=svm(train_x, y = NULL,
                  type = 'one-classification',
                  nu = i,
                  gamma = j,
                  scale = TRUE,
                  kernel = "radial")
    
    
    svm.predtrain=predict(svm.model,train_x)
    svm.predtest=predict(svm.model,test_x)
    
    confTrain=table(Predicted=svm.predtrain,Reference=train_y)
    confTest=table(Predicted=svm.predtest,Reference=test_y)
    
    accuracy = mean(svm.predtest==test_y)
    
    sensi = sensitivity(as.factor(svm.predtest), as.factor(test_y),positive = levels(as.factor(test_y))[2])
    
    specif = specificity(as.factor(svm.predtest), as.factor(test_y), negative = levels(as.factor(test_y))[1])
    
    total_result = data.frame(accuracy, sensi, specif, i, j)
    
    result = rbind(result, total_result)
  }
}

result %>%
  arrange(desc(result$specif))

# ----------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------------------------
# EMR + 마취기록지 혈압 + 마취기록지 약품 + 마취기록지 Vital
total_data <- emr_vol %>%
  inner_join(vol_record, by = 'Case_ID') %>%
  inner_join(vol_medi, by = 'Case_ID')

# ----------------------------------------------------------------------------------------------------------------------------------------------
# 모델링 전 데이터 전처리
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

nrow(total_data3)
table(total_data3$class)
# ----------------------------------------------------------------------------------------------------------------------------------------------

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
              gamma = 0.000001,
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
total_result

# ----------------------------------------------------------------------------------------------------------------------------------------------
nu.w <- c(0.0001, 0.001, 0.01, 0.1)
gamma.w <- c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 0.25, 0.5, 1)

result <- data.frame()
for(i in nu.w){
  for(j in gamma.w){
    
    svm.model=svm(train_x, y = NULL,
                  type = 'one-classification',
                  nu = i,
                  gamma = j,
                  scale = TRUE,
                  kernel = "radial")
    
    
    svm.predtrain=predict(svm.model,train_x)
    svm.predtest=predict(svm.model,test_x)
    
    confTrain=table(Predicted=svm.predtrain,Reference=train_y)
    confTest=table(Predicted=svm.predtest,Reference=test_y)
    
    accuracy = mean(svm.predtest==test_y)
    
    sensi = sensitivity(as.factor(svm.predtest), as.factor(test_y),positive = levels(as.factor(test_y))[2])
    
    specif = specificity(as.factor(svm.predtest), as.factor(test_y), negative = levels(as.factor(test_y))[1])
    
    total_result = data.frame(accuracy, sensi, specif, i, j)
    
    result = rbind(result, total_result)
  }
}

result %>%
  arrange(desc(result$specif))




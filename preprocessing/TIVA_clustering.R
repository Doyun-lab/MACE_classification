library(stringr)
library(dplyr)
library(signal)
library(fBasics)
library(seewave)
library(pracma)
library(dplyr)
library(changepoint)
library(caret)
library(e1071)
library(caret)
library(NLP)
library(tm)
library(dplyr)
library(dummies)

setwd('E:\\')

emr_TIVA = readRDS('preprocessing/emr_TIVA_0205_op_revise.rds')
names(emr_TIVA)[1] = 'Case_ID'

emr_TIVA$class <- ifelse(emr_TIVA$class == "NORMAL", "TRUE", "FALSE")

emr_TIVA <- as.data.frame(emr_TIVA)

# upsampling
table(emr_TIVA$class) # 8 : 92

emr_TIVA_abn <- subset(emr_TIVA, emr_TIVA$class == "FALSE")

emr_TIVA <- rbind(emr_TIVA, emr_TIVA_abn, emr_TIVA_abn, emr_TIVA_abn, emr_TIVA_abn, emr_TIVA_abn, emr_TIVA_abn, emr_TIVA_abn)
table(emr_TIVA$class) # 36 : 64

# clustering
set.seed(12546)

inTrain <- createDataPartition(y = emr_TIVA$class, p = 0.7, list = F)
train_x <- emr_TIVA[inTrain,]
test <- emr_TIVA[-inTrain,]

cluster_col <- c('Age', 'Wt', 'Ht', 'BMI', 'Na.1', 'K.1', 'Cl.1', 'BUN.1', 'Cr.1', 'OP.Time', 'Crystal', 'Colloid', 'PFT_3', 'PFT_2', 'PFT_1', 'OP_score')
cluster_train <- scale(train_x[,names(train_x)%in%cluster_col])

TIVA.kmeans <- kmeans(cluster_train, centers = 3, iter.max = 10000)
TIVA.kmeans$centers

train_x$cluster <- as.factor(TIVA.kmeans$cluster)
qplot(Crystal, Colloid, colour = cluster, data = train_x)

# 중심 개수 설정
library(NbClust)

nc <- NbClust(cluster_train, min.nc = 2, max.nc = 10, method = "kmeans")

barplot(table(nc$Best.nc[1,])) # 3개가 좋음

# ------------------------------------------------------------------------------------------------------------------------------------------------
# 모델링
# cluster 1
data_c1 <- subset(train_x, train_x$cluster == 1)

TIVA_record_cre = readRDS('preprocessing/tiva_record_cre.rds')
TIVA_record_cpt = readRDS('preprocessing/tiva_record_cpt.rds')
TIVA_record_summary = readRDS('preprocessing/tiva_record_summary.rds')

TIVA_medi = readRDS('preprocessing/TIVA_record_medi.rds')

# EMR + 마취기록지 혈압 + 마취기록지 약품
TIVA_record <- TIVA_record_cpt %>%
  inner_join(TIVA_record_cre, by = 'Case_ID') %>%
  inner_join(TIVA_record_summary, by = 'Case_ID')

bp_cols <- c(grep('SBP', names(TIVA_record)), grep('MBP', names(TIVA_record)), grep('DBP', names(TIVA_record)))
TIVA_record_bp <- TIVA_record[, bp_cols]
TIVA_record_bp$Case_ID <- TIVA_record$Case_ID

total_data <- data_c1 %>%
  inner_join(TIVA_record_bp, by = 'Case_ID') %>%
  inner_join(TIVA_medi, by = 'Case_ID')

# 모델링 전 데이터 전처리
set.seed(12546)

total_data$class = as.character(total_data$class)

total_data$ANE.Time = NULL
total_data$OP.Time = NULL
total_data$ASA = as.numeric(total_data$ASA)

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

# 특징만 추출해서 하려면 labeling으로 특징이 있는 열만 입력해야함. 
train_x = data_TRUE[,-grep("class",names(data_TRUE))]
train_y = data_TRUE[,"class"]

test_x = test[,-grep("class",names(test))]
test_x = test_x[,!names(test_x)%in%same_name_train]
test_y = test[,"class"]

# 모든 변수에 값은 값이 들어가면 scale할 수가 없어서 에러가 발생하기 때문에 해당 변수 삭제 
same_name_train = c()

for(name in names(train_x)){
  if(min(train_x[name])==max(train_x[name])){
    same_name_train = c(same_name_train,name)  
  }
}

train_x = train_x[,!names(train_x)%in%same_name_train]

train_y = data_TRUE[,"class"]

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
TIVA_record <- TIVA_record_cpt %>%
  inner_join(TIVA_record_cre, by = 'Case_ID') %>%
  inner_join(TIVA_record_summary, by = 'Case_ID')

bp_cols <- c(grep('SBP', names(TIVA_record)), grep('MBP', names(TIVA_record)), grep('DBP', names(TIVA_record)))
TIVA_record_bp <- TIVA_record[, bp_cols]
TIVA_record_bp$Case_ID <- TIVA_record$Case_ID

total_data <- emr_TIVA %>%
  inner_join(TIVA_record, by = 'Case_ID') %>%
  inner_join(TIVA_medi, by = 'Case_ID')

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




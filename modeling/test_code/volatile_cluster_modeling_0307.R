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

vol_freq = readRDS('preprocessing/volatile_freq_0225.rds')
vol_summary = readRDS('preprocessing/volatile_summary_0225.rds')
names(vol_freq)[1] = 'Case_ID'
# ----------------------------------------------------------------------------------------------------------------------------------------------
# EMR + 마취기록지 혈압 + 마취기록지 약품
volatile_record <- vol_record_cpt %>%
  inner_join(vol_record_cre, by = 'Case_ID') %>%
  inner_join(vol_record_summary, by = 'Case_ID')

bp_cols <- c(grep('SBP', names(volatile_record)), grep('MBP', names(volatile_record)), grep('DBP', names(volatile_record)))
vol_record_bp <- volatile_record[, bp_cols]
vol_record_bp$Case_ID <- volatile_record$Case_ID

total_data <- emr_vol %>%
  inner_join(volatile_record, by = 'Case_ID') %>%
  inner_join(vol_medi, by = 'Case_ID') %>%
  inner_join(vol_freq, by = 'Case_ID') %>%
  inner_join(vol_summary, by = 'Case_ID')

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
  
  df = cbind(df, dummy(total_data2[,col_name],sep=col_name))
  
}

total_data3 = cbind(total_data3, df)

total_data3$class = as.character(total_data3$class)

nrow(total_data3)
table(total_data3$class)

# up-sampling
total_data3_abn <- subset(total_data3, total_data3$class == "FALSE")

total_data3 <- rbind(total_data3, total_data3_abn, total_data3_abn, total_data3_abn)
table(total_data3$class) # 36 : 64
# ----------------------------------------------------------------------------------------------------------------------------------------------
# clustering
cluster_col <- c('Age', 'Wt', 'Ht', 'BMI', 'Na.1', 'K.1', 'Cl.1', 'BUN.1', 'Cr.1', 'OP.Time', 'Crystal', 'Colloid', 'PFT_3', 'PFT_2', 'PFT_1', 'OP_score')
cluster_train <- scale(total_data3[,names(total_data3)%in%cluster_col])

set.seed(2234)
vol.kmeans <- kmeans(cluster_train, centers = 3, iter.max = 1000)
vol.kmeans$centers

total_data3$cluster <- as.factor(vol.kmeans$cluster)
qplot(Crystal, Colloid, colour = cluster, data = total_data3)

total_data3$cluster <- as.numeric(total_data3$cluster)

saveRDS(total_data3, "preprocessing/volatile_record_VR_M.rds")
# ----------------------------------------------------------------------------------------------------------------------------------------------

set.seed(12546)
data_TRUE=subset(total_data3,class=="TRUE")
data_FALSE=subset(total_data3,class=="FALSE")

# 데이터셋 분리 
inTrain=createDataPartition(1:nrow(data_TRUE),p=0.7,list=FALSE)

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

# ----------------------------------------------------------------------------------------------------------------------------------------------

train_x_cl1 <- subset(train_x, train_x$cluster == 1)
train_y_cl1 <- subset(train_y, train_x$cluster == 1)

same_name_train = c()

for(name in names(train_x_cl1)){
  if(min(train_x_cl1[name])==max(train_x_cl1[name])){
    same_name_train = c(same_name_train,name)  
  }
}

train_x_cl1 = train_x_cl1[,!names(train_x_cl1)%in%same_name_train]
test_x_cl1 = test_x[,!names(test_x)%in%same_name_train]
svm.model_cl1=svm(train_x_cl1,y=NULL,
                  type='one-classification',
                  nu=0.01,
                  gamma = 0.01,
                  scale = TRUE,
                  kernel = "radial")


svm.predtrain_cl1=predict(svm.model_cl1,train_x_cl1)
svm.predtest_cl1=predict(svm.model_cl1,test_x_cl1)

confTrain_cl1=table(Predicted=svm.predtrain_cl1,Reference=train_y_cl1)
confTest_cl1=table(Predicted=svm.predtest_cl1,Reference=test_y)

accuracy_cl1 = mean(svm.predtest_cl1==test_y)

sensi_cl1 = sensitivity(as.factor(svm.predtest_cl1), as.factor(test_y),positive = levels(as.factor(test_y))[2])

specif_cl1 = specificity(as.factor(svm.predtest_cl1), as.factor(test_y), negative = levels(as.factor(test_y))[1])

total_result1 = c(accuracy_cl1, sensi_cl1, specif_cl1)
total_result1

# ----------------------------------------------------------------------------------------------------------------------------------------------

train_x_cl2 <- subset(train_x, train_x$cluster == 2)
train_y_cl2 <- subset(train_y, train_x$cluster == 2)

same_name_train2 = c()

for(name in names(train_x_cl2)){
  if(min(train_x_cl2[name])==max(train_x_cl2[name])){
    same_name_train2 = c(same_name_train2,name)  
  }
}

train_x_cl2 = train_x_cl2[,!names(train_x_cl2)%in%same_name_train2]
test_x_cl2 = test_x[,!names(test_x)%in%same_name_train2]
svm.model_cl2=svm(train_x_cl2,y=NULL,
                  type='one-classification',
                  nu=0.01,
                  gamma = 0.01,
                  scale = TRUE,
                  kernel = "radial")


svm.predtrain_cl2=predict(svm.model_cl2,train_x_cl2)
svm.predtest_cl2=predict(svm.model_cl2,test_x_cl2)

confTrain_cl2=table(Predicted=svm.predtrain_cl2,Reference=train_y_cl2)
confTest_cl2=table(Predicted=svm.predtest_cl2,Reference=test_y)

accuracy_cl2 = mean(svm.predtest_cl2==test_y)

sensi_cl2 = sensitivity(as.factor(svm.predtest_cl2), as.factor(test_y),positive = levels(as.factor(test_y))[2])

specif_cl2 = specificity(as.factor(svm.predtest_cl2), as.factor(test_y), negative = levels(as.factor(test_y))[1])

total_result2 = c(accuracy_cl2, sensi_cl2, specif_cl2)
total_result2


# ----------------------------------------------------------------------------------------------------------------------------------------------

train_x_cl3 <- subset(train_x, train_x$cluster == 3)
train_y_cl3 <- subset(train_y, train_x$cluster == 3)

same_name_train3 = c()

for(name in names(train_x_cl3)){
  if(min(train_x_cl3[name])==max(train_x_cl3[name])){
    same_name_train3 = c(same_name_train3,name)  
  }
}

train_x_cl3 = train_x_cl3[,!names(train_x_cl3)%in%same_name_train3]
test_x_cl3 = test_x[,!names(test_x)%in%same_name_train3]
svm.model_cl3=svm(train_x_cl3,y=NULL,
                  type='one-classification',
                  nu=0.01,
                  gamma = 0.01,
                  scale = TRUE,
                  kernel = "radial")


svm.predtrain_cl3=predict(svm.model_cl3,train_x_cl3)
svm.predtest_cl3=predict(svm.model_cl3,test_x_cl3)

confTrain_cl3=table(Predicted=svm.predtrain_cl3,Reference=train_y_cl3)
confTest_cl3=table(Predicted=svm.predtest_cl3,Reference=test_y)

accuracy_cl3 = mean(svm.predtest_cl3==test_y)

sensi_cl3 = sensitivity(as.factor(svm.predtest_cl3), as.factor(test_y),positive = levels(as.factor(test_y))[2])

specif_cl3 = specificity(as.factor(svm.predtest_cl3), as.factor(test_y), negative = levels(as.factor(test_y))[1])

total_result3 = c(accuracy_cl3, sensi_cl3, specif_cl3)
total_result3

# ----------------------------------------------------------------------------------------------------------------------------------------------

svm.predtest_cl1[3]
svm.predtest_cl2[3]
svm.predtest_cl3[3]

table(svm.predtest_cl1 == svm.predtest_cl2 & svm.predtest_cl1 == svm.predtest_cl3)

answer <- c()
for(x in 1:length(svm.predtest_cl1)){
  
  if(svm.predtest_cl1[x] == FALSE){
    x.t <- 1
  } else {
    x.t <- 0
  }
  
  if(svm.predtest_cl2[x] == FALSE){
    y.t <- 1
  } else {
    y.t <- 0
  }
  
  if(svm.predtest_cl3[x] == FALSE){
    z.t <- 1
  } else {
    z.t <- 0
  }
  
  xyz.t <- ifelse(x.t + y.t + z.t == 3, FALSE, TRUE)
  
  answer <- c(answer, xyz.t)
  
}


confTest_cl4=table(Predicted=answer,Reference=test_y)

accuracy_cl4 = mean(answer==test_y)

sensi_cl4 = sensitivity(as.factor(answer), as.factor(test_y),positive = levels(as.factor(test_y))[2])

specif_cl4 = specificity(as.factor(answer), as.factor(test_y), negative = levels(as.factor(test_y))[1])

total_result4 = c(accuracy_cl4, sensi_cl4, specif_cl4)
total_result4

total_result1
total_result2
total_result3
total_result4


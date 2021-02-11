# 환경 설정
setwd('E:\\')

# 라이브러리 불러오기 
library(stringr)
library(readxl)
library(dplyr)
library(data.table)
library(dlookr)
library(stringi)

#################################################################################################################
# 1. emr data preprocessing
# * 검사 결과에 대한 결측치는 검사를 하지 않아 기록이 되지 않은 것.(pre-op evaluation)
# * PFT(폐기능 검사) 3개의 검사 수치를 파생변수로 생성 - 결측치는 0으로 대체 
# * PFT, EF 검사 유무에 대한 파생 변수 생성 
# * Method에 따라 잘못 입력된 case id 수정 
#################################################################################################################

#데이터 불러오기
emr = read.csv("data/seoul_EMR_180301_200430_revise.csv")
table(emr_copy2$OP.Time)  ####**** 0인 것에 대해 VR 데이터 존재하는 지 확인 후 수정해야함!!!!!!!

str(emr)
names(emr)
summary(emr$EF)
summary(emr$P)

emr$P = as.numeric(emr$P)
emr$Ca = as.numeric(emr$Ca)

names(emr)
# 삭제할 변수 
# 변수 카테고리 파일 확인 결과 RPR(매독검사)는 분석제외라고 적혀있음. 
# OP NAME 은 수술명에 따른 중증도 score에 사용 
# Inotropics: 승압제 투여여부는 수술 후 변수 
# POD_1: Death 이벤트에 관한
# PreopDx: 수술 전 진단명 
except_name = c('X','X.1','Cx','CPR','POD','POD_1', 'Inotropics','Cause','RPR','PreopDx','받기','기본','마취','X.2','End.stage.renal.disease.ESRD.','Chronic.kidney.injury','Acute.kidney.injury','POD.1','POD.2','Patient.ID','HBsAg')
emr2 = emr[,!names(emr)%in%except_name]
str(emr2)

table(emr2$MACE)  # 321
table(emr2$Death) # 126

##################################################
#-----------------결측치 처리-----------------
##################################################

## 결측치 확인 
# OP Time: 0, 결측치인 것들은 VR 데이터로 부터 시간 계산하여 대체 
col_na = names(colSums(is.na(emr2))[colSums(is.na(emr2))>0&colSums(is.na(emr2))<5])
colSums(is.na(emr[col_na]))

# 2110307: Wt, Ht, BMI -> 삭제 
# Sex에도 빈값으로 되어있음. 
emr2 = emr2[!is.na(emr2$Wt),] 

dim(emr2)
dim(emr2[,!names(emr2)%in%names(emr2)[find_na(emr2,rate=TRUE)==100]])   # 모든 관측치에 결측치 존재하는 열 확인  -> 없음

# EF 결측치 처리 
for(i in 1:nrow(emr2)){
  if(is.na(emr2$EF[i])){
    emr2$EF[i] = as.numeric(str_sub(emr2$Echo[i],4,5))
  }
}

# 나머지 결측치는 0으로 대체
emr2$EF[is.na(emr2$EF)] = 0
emr2$Echo = NULL

summary(emr2)

# PFT변수 전처리
head(emr2$PFT,15)

PFT_split = str_split(emr2$PFT,'-')

emr2$PFT_3 = sapply(PFT_split,function(x){x[3]})

emr2$PFT_2 = unlist(lapply(sapply(PFT_split,function(x){x[2]}),function(x){str_sub(x,str_locate(x,'\\(')[1]+1,str_locate(x,'\\)')[1]-1)}))

emr2$PFT_1 = unlist(lapply(sapply(PFT_split,function(x){x[1]}),function(x){str_sub(x,str_locate(x,'\\(')[1]+1,str_locate(x,'\\)')[1]-1)}))

emr2$PFT_3 = as.numeric(emr2$PFT_3)           # 자료형변환 
emr2$PFT_2 = as.numeric(emr2$PFT_2)
emr2$PFT_1 = as.numeric(emr2$PFT_1)

table(is.na(emr2$PFT_3))
table(is.na(emr2$PFT_2))
table(is.na(emr2$PFT_1))

emr2$PFT_3[is.na(emr2$PFT_3)] = 0                 # 결측치 0으로 대체
emr2$PFT_2[is.na(emr2$PFT_2)] = 0
emr2$PFT_1[is.na(emr2$PFT_1)] = 0

emr2$EF_inspect = ifelse(emr2$EF==0,0,1)            # 검사 유무 속성 생성 

emr2$PFT1_inspect = ifelse(emr2$PFT_1==0,0,1)
emr2$PFT2_inspect = ifelse(emr2$PFT_2==0,0,1)
emr2$PFT3_inspect = ifelse(emr2$PFT_3==0,0,1)

emr2$EF_inspect = as.factor(emr2$EF_inspect)         # factor형으로 변환 

emr2$PFT1_inspect = as.factor(emr2$PFT1_inspect)
emr2$PFT2_inspect = as.factor(emr2$PFT2_inspect)
emr2$PFT3_inspect = as.factor(emr2$PFT3_inspect)

emr2 = emr2[,!names(emr2)%in%c('EF','PFT')]   # 불필요해진 변수 삭제 

# ASA 변수 업데이트 
table(emr2$ASA)
emr2$ASA = ifelse(emr2$ASA==1|emr2$ASA==2|emr2$ASA==3,3,4) 
table(emr2$ASA)

names(emr2)
str(emr2)

# 문자열 변수 확인 
char_name = names(unlist(lapply(emr2, class)[lapply(emr2, class)%in%c("factor","character")]))
char_name = char_name[-1]
str(emr2[,names(emr2)%in%char_name])

# 값이 존재하지 않으면 검사를 하지 않은 것. 
emr2$AntiHBs = ifelse(emr2$AntiHBs=="","0",emr2$AntiHBs)
emr2$AntiHIV = ifelse(emr2$AntiHIV=="","0",emr2$AntiHIV)

nrow(emr2)
colSums(is.na(emr2)) 

emr_copy = emr2

table(is.na(emr_copy$Na.1))
table(is.na(emr_copy$Cl.1))
table(is.na(emr_copy$K.1))

# postHD 표준을로 사용 
# postHD에 값이 없는 경우만 preHD나 pre-op로 대체 
for (i in 1:nrow(emr_copy)){
  
  if(is.na(emr_copy$Na.1[i])==TRUE){
    if(is.na(emr_copy$Na.2[i])==FALSE){
      emr_copy$Na.1[i] = emr_copy$Na.2[i]
    }
    else if(is.na(emr_copy$Na.2[i])==TRUE){
      emr_copy$Na.1[i] = emr_copy$Na[i]
    }
  }
  
  if(is.na(emr_copy$Cl.1[i])==TRUE){
    if(is.na(emr_copy$Cl.2[i])==FALSE){
      emr_copy$Cl.1[i] = emr_copy$Cl.2[i]
    }
    else if(is.na(emr_copy$Cl.2[i])==TRUE){
      emr_copy$Cl.1[i] = emr_copy$Cl[i]
    }  
  }
  
  if(is.na(emr_copy$K.1[i])==TRUE){
    if(is.na(emr_copy$K.2[i])==FALSE){
      emr_copy$K.1[i] = emr_copy$K.2[i]
    }
    else if(is.na(emr_copy$K.2[i])==TRUE){
      emr_copy$K.1[i] = emr_copy$K[i]
    }  
  }
}

# 결측치 수 확인 
table(is.na(emr_copy$Na.1)) 
table(is.na(emr_copy$Cl.1))   
table(is.na(emr_copy$K.1))  

emr_copy2 = emr_copy[!is.na(emr_copy$Na.1),]  # NA,k,cl에 결측치 제거 
nrow(emr_copy)
nrow(emr_copy2)  # 14건 삭제됨. 

except_preHD_OP = c("Na","Na.2","Cl","Cl.2","K","K.2","BUN","BUN.2","Cr","Cr.2")
emr_copy2 = emr_copy2[,!names(emr_copy2)%in%except_preHD_OP]

na_col = names(emr_copy2)[colSums(is.na(emr_copy2))!=0]
summary(emr_copy2[na_col])
length(na_col)

par(mfrow=c(3,4))

for (col in na_col[c(3:13,15)]){
  boxplot(emr_copy2[col],main = paste0(col," | na_rate: ",round(sum(is.na(emr_copy2[col]))/nrow(emr_copy2),2)," (",sum(is.na(emr_copy2[col])),"건)"))
  
}

nrow(emr2)  #3248건


# 더미변수 생성 
data = read_excel('data/변수 카테고리.xlsx', skip=2)[1,]
na_col_REAL = na_col[c(3:5,8:13)]
na_col_REAL

data = data[,na_col_REAL]

normal_list = sapply(data,function(x)str_split(x,c("-|~")))

normal_list[col_name][[1]][1]
normal_list[col_name][1]


for(col_name in na_col_REAL){
  
  normal_name = paste0(col_name,"_normal")
  print(normal_name)
  emr_copy2[normal_name] = ifelse(emr_copy2[,col_name]>=as.numeric(normal_list[col_name][[1]][1])&emr_copy2[,col_name]<=as.numeric(normal_list[col_name][[1]][2])&!is.na(emr_copy2[,col_name]),1,0)
  
  out_name = paste0(col_name,"_out")
  print(normal_name)
  emr_copy2[out_name] = ifelse(emr_copy2[,col_name]<as.numeric(normal_list[col_name][[1]][1])&!is.na(emr_copy2[,col_name])|emr_copy2[,col_name]>as.numeric(normal_list[col_name][[1]][2])&!is.na(emr_copy2[,col_name]),1,0)
  
  NA_name = paste0(col_name,"_na")
  print(normal_name)
  emr_copy2[NA_name] = ifelse(is.na(emr_copy2[,col_name]),1,0)
}

# P: 2.5~4.5
# Ca: 8.2 - 10.2
# HbA1c = 4.0~6.0
col_name = "P"
emr_copy2["P_normal"] = ifelse(emr_copy2[,"P"]>=2.5&emr_copy2[,col_name]<=4.5&!is.na(emr_copy2[,col_name]),1,0)
emr_copy2["P_out"] = ifelse(emr_copy2[,col_name]<2.5&!is.na(emr_copy2[,col_name])|emr_copy2[,col_name]>4.5&!is.na(emr_copy2[,col_name]),1,0)
emr_copy2["P_na"] = ifelse(is.na(emr_copy2[,col_name]),1,0)

col_name = "Ca"
emr_copy2["Ca_normal"] = ifelse(emr_copy2[,col_name]>=8.2&emr_copy2[,col_name]<=10.2&!is.na(emr_copy2[,col_name]),1,0)
emr_copy2["Ca_out"] = ifelse(emr_copy2[,col_name]<8.2&!is.na(emr_copy2[,col_name])|emr_copy2[,col_name]>10.2&!is.na(emr_copy2[,col_name]),1,0)
emr_copy2["Ca_na"] = ifelse(is.na(emr_copy2[,col_name]),1,0)

col_name = "HbA1c"
emr_copy2["HbA1c_normal"] = ifelse(emr_copy2[,col_name]>=4.0&emr_copy2[,col_name]<=6.0&!is.na(emr_copy2[,col_name]),1,0)
emr_copy2["HbA1c_out"] = ifelse(emr_copy2[,col_name]<4.0&!is.na(emr_copy2[,col_name])|emr_copy2[,col_name]>6.0&!is.na(emr_copy2[,col_name]),1,0)
emr_copy2["HbA1c_na"] = ifelse(is.na(emr_copy2[,col_name]),1,0)

names(emr_copy2)
colSums(is.na(emr_copy2))

emr_copy2 = emr_copy2[,!names(emr_copy2)%in%na_col[c(3:13,15)]]
names(emr_copy2)

# 박스플롯 범위의 이상치 확인 
na_1_value = c()
value_total = c()

for(col in na_col_REAL){
  
  print(col)
  
  na_index = which(na.omit(emr_copy2[,col])>summary(emr_copy2[,col])[5] + 1.5*IQR(na.omit(emr_copy2[,col]))) # 464부터 이상치라고 함 (박스플롯상)
  na_1_value = c(na_1_value,na.omit(emr_copy2[,col])[na_index[1]])

}

value_total = cbind(na_col_REAL,na_1_value)


##################################################
#-----------------이상치 처리-----------------
##################################################
# numeric_name = names(unlist(lapply(emr2, class)[lapply(emr2, class)%in%c("integer","numeric")]))
# numeric_name = numeric_name[-1]
# 
# colSums(is.na(emr2))
# 
# 
# # 이상치 확인
# dir.create("plot_new")
# 
# for(name in numeric_name){
#   if(length(table(data.frame(emr2)[name]))!=2){
#     png(filename=paste0('plot_new/',name,".png"))
#     boxplot(data.frame(emr2)[name],main = name)
# 
#     dev.off()
#   }
# 
# }

# 이상치는 직접 엑셀 파일에서 수정했음. 

emr2 = emr_copy2
emr2[is.na(emr2)] = 0 # 일단 0(아마 수정될 듯 )
##################################################
#---------잘못 입력된 값 수정------------
##################################################
# voaltile에 잘못들어있는 케이스들 TIVA로 분류
Tiva_case_id = c('03_190201_1225','03_181002_1340','03_191210_1430','06_191205_0741','08_190208_0905','06_191112_0740','11_190517_0758')

emr_method_revise_tiva = subset(emr2, emr2$Case.ID%in%Tiva_case_id)
emr_method_revise_tiva$Method = 'TIVA'

emr_method_revise = subset(emr2, !emr2$Case.ID%in%Tiva_case_id)
emr_method_total = rbind(emr_method_revise, emr_method_revise_tiva)

table(emr_method_total$Method)

# class 생성
# MACE 가 DEATH보다 먼저 
emr_method_total$class = ifelse(emr_method_total$MACE==0&emr_method_total$Death==0,'NORMAL',ifelse(emr_method_total$MACE==1,'MACE','DEATH'))
table(emr_method_total$class) # DEATH: 43, MACE: 321, NORMAL: 2884
nrow(emr_method_total)

names(emr_method_total)

# 전처리 후 불필요해진 필드 삭제 
except_name = c("Death","MACE")
emr_method_total = emr_method_total[,!names(emr_method_total)%in%except_name]
summary(emr_method_total)

# 수술명 전처리 
OP_score = read_excel('data/수술명 리스트_수술명 별 중증도 점수화.xlsx', sheet=2)
OP_score = OP_score[,c(1:4)]
str(OP_score)

unique(OP_score)

OP_score = OP_score[!duplicated(OP_score),] # 중복 행 제거 

emr_method_total$OP.Name_re = sapply(strsplit(emr_method_total$OP.Name, "\\[.+?"), function(a) a[1])%>% stri_trim()

OP_score$`OP name` = OP_score$`OP name` %>% stri_trim()

for (i in 1:nrow(emr_method_total)){
  for(x in 1:nrow(OP_score)){
    if(emr_method_total$OP.Name_re[i]==OP_score$`OP name`[x]){
      
      emr_method_total$OP_surgery_type[i] = OP_score$`surgery type`[x]
      emr_method_total$OP_Severity[i] = OP_score$Severity[x]
      emr_method_total$OP_score[i] = OP_score$Score[x]
      }
  }
}

head(emr_method_total)

str(emr_method_total)

# 사용하지 않을 변수 삭제 
emr_method_total$OP_Severity = NULL
emr_method_total$OP_surgery_type = NULL
emr_method_total$OP.Name = NULL
emr_method_total$OP.Name_re = NULL
emr_method_total$ANE.Time = NULL
emr_method_total$EKG = NULL

emr_TIVA = emr_method_total[emr_method_total$Method=='TIVA',]
emr_volatile = emr_method_total[emr_method_total$Method!='TIVA',]

table(emr_TIVA$class)
table(emr_volatile$class)

table(emr_TIVA$Method)
table(emr_TIVA$OP.Time)

table(emr_volatile$Method)
table(emr_volatile$OP.Time)

emr_volatile$Method = NULL
emr_TIVA$Method = NULL

summary(emr_TIVA)
names(emr_TIVA)
names(emr_volatile)

saveRDS(emr_TIVA,'preprocessing/emr_TIVA_0124.rds')
saveRDS(emr_volatile,'preprocessing/emr_volatile_0124.rds')

nrow(emr_TIVA)  
nrow(emr_volatile) 

emr_TIVA

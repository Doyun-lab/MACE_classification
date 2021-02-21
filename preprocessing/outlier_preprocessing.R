data = emr_copy2
data_na = ifelse(data$Hb < boxplot(data$Hb)$stats[1,] | data$Hb > boxplot(data$Hb)$stats[5,], NA, data$Hb)

data$Hb < boxplot(data$Hb)$stats[1,]

mean(data$Hb,na.rm=TRUE)+
  
sum(data$Hb,na.rm = TRUE)

data$IBP1 <- na_interpolation(data_na, option = "stine")

data = read_excel('data/변수 카테고리.xlsx', skip=2)[1,]
na_col_REAL

data = data[,na_col_REAL]

normal_list = sapply(data,function(x)str_split(x,c("-|~")))

emr_co = emr_copy2

na_col_REAL = na_col[c(3:5,8:13)]
na_col_REAL

normal_list[col_name][[1]][1]
normal_list[col_name][1]


for(col_name in na_col_REAL){
  
  normal_name = paste0(col_name,"_normal")
  print(normal_name)
  emr_co[normal_name] = ifelse(emr_co[,col_name]>=as.numeric(normal_list[col_name][[1]][1])&emr_co[,col_name]<=as.numeric(normal_list[col_name][[1]][2])&!is.na(emr_co[,col_name]),1,0)
  
  out_name = paste0(col_name,"_out")
  print(normal_name)
  emr_co[out_name] = ifelse(emr_co[,col_name]<as.numeric(normal_list[col_name][[1]][1])&!is.na(emr_co[,col_name])|emr_co[,col_name]>as.numeric(normal_list[col_name][[1]][2])&!is.na(emr_co[,col_name]),1,0)
  
  NA_name = paste0(col_name,"_na")
  print(normal_name)
  emr_co[NA_name] = ifelse(is.na(emr_co[,col_name]),1,0)
}

# P: 2.5~4.5
# Ca: 8.2 - 10.2
# HbA1c = 4.0~6.0
col_name = "P"
emr_co["P_normal"] = ifelse(emr_co[,"P"]>=2.5&emr_co[,col_name]<=4.5&!is.na(emr_co[,col_name]),1,0)
emr_co["P_out"] = ifelse(emr_co[,col_name]<2.5&!is.na(emr_co[,col_name])|emr_co[,col_name]>4.5&!is.na(emr_co[,col_name]),1,0)
emr_co["P_na"] = ifelse(is.na(emr_co[,col_name]),1,0)

col_name = "Ca"
emr_co["Ca_normal"] = ifelse(emr_co[,col_name]>=8.2&emr_co[,col_name]<=10.2&!is.na(emr_co[,col_name]),1,0)
emr_co["Ca_out"] = ifelse(emr_co[,col_name]<8.2&!is.na(emr_co[,col_name])|emr_co[,col_name]>10.2&!is.na(emr_co[,col_name]),1,0)
emr_co["Ca_na"] = ifelse(is.na(emr_co[,col_name]),1,0)

col_name = "HbA1c"
emr_co["HbA1c_normal"] = ifelse(emr_co[,col_name]>=4.0&emr_co[,col_name]<=6.0&!is.na(emr_co[,col_name]),1,0)
emr_co["HbA1c_out"] = ifelse(emr_co[,col_name]<4.0&!is.na(emr_co[,col_name])|emr_co[,col_name]>6.0&!is.na(emr_co[,col_name]),1,0)
emr_co["HbA1c_na"] = ifelse(is.na(emr_co[,col_name]),1,0)

colSums(is.na(emr_co))

table(emr_co$HbA1c_normal)
table(emr_co$HbA1c_out)
table(emr_co$HbA1c_na)

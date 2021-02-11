library(data.table)
library(stringr)
library(stringi)
library(dplyr)

setwd('E:\\')

total_data = readRDS('preprocessing/total_data.rds')

# vital data load
file_list = list.files("seoul_ECG_pleth")
tail(file_list,20)
length(file_list)

file_0size = c("03_190228_1525.csv","06_190328_0830.csv","09_180716_125648.csv","10_180910_073244.csv","11_180907_141628.csv","11_181018_100502.csv","13_190507_0735.csv","13_191204_0943.csv")

file_list = file_list[!file_list%in%file_0size]
length(file_list)

# wave의 case_id
file_id =  unlist(lapply(str_split(file_list,".csv"),function(x){x[1]}))
file_id = data.frame(file_id[file_id%in%file_id[!str_detect(file_id, "[a-z]")]])

names(file_id) = 'Case_ID'
tail(file_id,8)

file_id$Case_ID = as.character(file_id$Case_ID)

# tiva
file_case_TIVA = inner_join(total_data,file_id,by='Case_ID') 
length(unique(file_case_TIVA$Case_ID))  # 822

# wave data load(TIVA)
wave_TIVA = data.table()
count = 0

system.time(
  
  for (file_name in file_case_TIVA$Case_ID){
    
    df = fread(paste0('seoul_ECG_pleth/',file_name,'.csv'))
      
    df$Case_ID = file_name
    
    wave_TIVA = bind_rows(wave_TIVA,df) 
    
    count = count+1
    
    if(count%%10==0){
      print(count)}

  })

str(wave_TIVA)

file_case_TIVA$Case_ID[300]
wave_TIVA1 = wave_TIVA[!wave_TIVA$Case_ID%in%c("06_190425_1103","06_190504_0830","06_190504_1017"),]

length(unique(wave_TIVA1$Case_ID))

wave_TIVA = wave_TIVA[,c(1,3,5,10)]
colSums(is.na(wave_TIVA))

length(unique(wave_TIVA$Case_ID))

saveRDS(wave_TIVA, "wave_TIVA.rds")

result = c()

for (i in 1:length(unique(wave_TIVA$Case_ID))){
  
  par(mfrow=c(2,1))
  
  data = subset(wave_TIVA, wave_TIVA$Case_ID == unique(wave_TIVA$Case_ID)[i])
  
  if (length(names(colSums(is.na(data))[colSums(is.na(data)) == nrow(data)]))>0){
  
    print(colSums(is.na(data)))
    
    print(unique(data$Case_ID))
    
    result = c(result, unique(data$Case_ID))
    
  }
}

length(result)

wave_TIVA2 = wave_TIVA[!wave_TIVA$Case_ID %in% result,]
length(unique(wave_TIVA2$Case_ID))




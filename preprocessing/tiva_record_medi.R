
###
library(tidyverse)
library(data.table)

setwd("/Users/kwondoyun/Downloads/")

emr_tiva = readRDS('emr_TIVA_0629.rds')

file_list_record = list.files("seoul_Record_210913")

# 06_200313_1133
file_list_record = file_list_record[!file_list_record=='06_200313_1133.txt.csv']
file_list_record

# vital record의 case_id 추출
file_id =  unlist(lapply(str_split(file_list_record,".txt"),function(x){x[1]}))
length(file_id)  # 3519

# tiva 겹치는 마취 기록지 케이스 
tiva_record_case = emr_tiva$Case_ID[emr_tiva$Case_ID%in%file_id]
length(tiva_record_case)   # 1510 

# tiva
file_case_tiva = file_id[file_id%in%emr_tiva$Case_ID]
length(file_case_tiva)  # 1507

# wave data load (tiva)
record_tiva = data.table()
count = 0
system.time(
  
  for (file_name in file_case_tiva){
    
    df = data.table(read.csv(paste0('seoul_Record_210913/',file_name,'.txt.csv'), fileEncoding = 'cp949')[,c('group','item','value1')])
    
    names(df)
    
    df$Case_ID = file_name
    
    record_tiva = rbind(record_tiva,df) 
    
    count = count+1
    
    if(count%%10==0){
      print(count)}
    
  })

str(record_tiva)
head(record_tiva)
length(unique(record_tiva$Case_ID))

summary(record_tiva)

unique(record_tiva$value1)
table(record_tiva$group)

medi_tiva <- subset(record_tiva, record_tiva$group =="Drug" | record_tiva$group =="Fluid" | record_tiva$group =="Inhalational Agent" | record_tiva$group =="Intravenous Agent" | record_tiva$group =="Local Anesthetic")
medi_tiva <- medi_tiva %>% select(item, value1, Case_ID)
medi_tiva$item <- gsub('\n','',medi_tiva$item)
medi_tiva$item <- gsub('1종','종',medi_tiva$item)

medi_tiva$value1 <- gsub('-','',medi_tiva$value1)
medi_tiva$value1 <- gsub('G','',medi_tiva$value1)
medi_tiva$value1 <- as.numeric(medi_tiva$value1)
medi_tiva[is.na(medi_tiva)] <- 0

medisum_tiva <- medi_tiva %>%
  group_by(item, Case_ID) %>%
  summarise(sum_value = sum(value1)) %>%
  arrange(Case_ID)

medi_name <- unique(medisum_tiva$item)
medi_name <- sort(medi_name)
medi_case <- unique(medisum_tiva$Case_ID)

medi_df <- data.frame(matrix(nrow = length(medi_case), ncol = length(medi_name)))
rownames(medi_df) <- medi_case
colnames(medi_df) <- medi_name

medi_idx <- data.frame(medi_name, c(1:length(medi_name)))
colnames(medi_idx) <- c("item", "idx")

medisum_tiva2 <- inner_join(medisum_tiva, medi_idx, by = "item")

system.time(
  for (x in unique(medisum_tiva2$Case_ID)){
    for (y in rownames(medi_df)){
      if (x == y){
        match_case <- subset(medisum_tiva2, medisum_tiva2$Case_ID == x)
        
        for (i in match_case$item){
          item <- subset(match_case, match_case$item == i)
          medi_df[x, item$idx] <- item$sum_value
        }
      }
    }
    print(x)
  }
)

medi_df[is.na(medi_df)] <- 0
saveRDS(medi_df, "tiva_medi.rds")



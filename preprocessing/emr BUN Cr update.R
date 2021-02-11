setwd("E:\\")

emr_TIVA = readRDS('preprocessing/emr_TIVA_0124.rds')
emr_volatile = readRDS('preprocessing/emr_volatile_0124.rds')
names(emr_volatile)[1] = "Case.ID"

emr_bc <- read.csv('emr_update_bun_and_cr.csv')
emr_bc <- emr_bc %>%
  select(Case.ID, BUN, Cr)

nrow(emr_TIVA) # 1720
nrow(emr_volatile) # 1514

emr_TIVA_new <- inner_join(emr_TIVA, emr_bc, by = "Case.ID")
emr_TIVA_new$BUN.1 <- emr_TIVA_new$BUN
emr_TIVA_new$Cr.1 <- emr_TIVA_new$Cr
emr_TIVA_new <- emr_TIVA_new[,1:92]

emr_volatile_new <- inner_join(emr_volatile, emr_bc, by = "Case.ID")
emr_volatile_new$BUN.1 <- emr_volatile_new$BUN
emr_volatile_new$Cr.1 <- emr_volatile_new$Cr
emr_volatile_new <- emr_volatile_new[,1:92]

nrow(emr_TIVA_new) # 1720 - 6
nrow(emr_volatile_new) # 1514 -2

saveRDS(emr_TIVA_new, 'preprocessing/emr_TIVA_0205.rds')
saveRDS(emr_volatile_new, 'preprocessing/emr_volatile_0205.rds')

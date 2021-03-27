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
  
  # OP_Score??? ?????? OP_Severity ???????????????. 
  except_name = c("Case_ID","OP.Name","OP.Name_re","EKG","OP_surgery_type","OP_Severity")
  total_data2 = total_data[,!names(total_data)%in%except_name]
  
  total_data2 = total_data2 %>% mutate_if(is.character, as.factor)
  
  ##################################################
  #---------모든 ???코드??? ????????? ??? 처리------------
  ##################################################
  same_name = c()
  
  # 모든???코드??? ????????? ??? ?????? 
  for(name in names(total_data2)){
    if(length(unique(total_data2[,name]))==1){
      same_name = c(same_name,name)  
    }
  }
  
  
  total_data2 = total_data2[,!names(total_data2)%in%same_name]
  
  # facotr????????? 변경해????????? 변?????? ??????(기???질환 변???)
  name_ = c()
  col_factor_feature = data.frame()
  
  for(col_ in names(total_data2)){
    df = data.frame(col_, count = length(unique(data.frame(total_data2)[,col_])))
    
    col_factor_feature = rbind(col_factor_feature,df)
  }
  
  factor_col = as.character(subset(col_factor_feature, col_factor_feature$count == 2)$col_)
  
  factor_col2 = factor_col[!str_detect(factor_col,paste0(c("class","ASA","peak_","freq_","cpt_","cre_"), collapse = "|"))]
  
  total_data2[,factor_col2] = lapply(total_data2[,factor_col2], as.factor)

  # factor_col = factor_col[-grep("class",factor_col)]  # peak ???징??? ?????? 
  # factor_col = factor_col[c(1:3,24:29)]
  # total_data2[,factor_col] = lapply(total_data2[,factor_col], as.factor)
  
  # factor, 문자??? 변??? ?????? 추출 
  char_name_new = names(unlist(lapply(total_data2, class)[lapply(total_data2, class)%in%c("factor","character")]))
  char_name_new = char_name_new[-grep("class",char_name_new)] # class ?????? ?????? 
  
  
  # ???미?????? ??????
  total_data3 = total_data2[,!names(total_data2)%in%char_name_new]
  
  
  df = c()
  
  for (col_name in char_name_new){
    
    df = cbind(df, dummy(total_data2[,col_name],sep='_'))
    
  }
  
  total_data3 = cbind(total_data3, df)
  
  total_data3$class = as.character(total_data3$class)
  
  
  data_TRUE=subset(total_data3,class=="TRUE")
  data_FALSE=subset(total_data3,class=="FALSE")
  
  # ???????????? 분리 
  inTrain=createDataPartition(1:nrow(data_TRUE),p=0.8,list=FALSE)
  
  
  # ???징만 추출?????? ????????? labeling?????? ???징이 ?????? ?????? ???????????????. 
  train_x = data_TRUE[inTrain,-grep("class",names(data_TRUE))]
  
  # 모든 변?????? 값??? 값이 ??????가??? scale??? ???가 ????????? ??????가 발생?????? ???문에 ?????? 변??? ?????? 
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
  # test_x = test_x[,-control_index]
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
  # return(confusionMatrix(confTest,positive="TRUE"))
  
  
}

model_mace = function(total_data){
  
  set.seed(12546)
  
  total_data$class = as.character(total_data$class)
  
  total_data$ANE.Time = NULL
  total_data$OP.Time = NULL
  total_data$ASA = as.numeric(total_data$ASA)
  
  total_data$class[total_data$class=="NORMAL"] = "TRUE"
  total_data$class[total_data$class!="TRUE"] = "FALSE"
  
  char_name = names(unlist(lapply(total_data, class)[lapply(total_data, class)%in%c("factor","character")]))
  
  total_data[is.na(total_data)] = 0
  
  # OP_Score??? ?????? OP_Severity ???????????????. 
  except_name = c("Case_ID","OP.Name","OP.Name_re","EKG","OP_surgery_type","OP_Severity")
  total_data2 = total_data[,!names(total_data)%in%except_name]
  
  total_data2 = total_data2 %>% mutate_if(is.character, as.factor)
  
  ##################################################
  #---------모든 ???코드??? ????????? ??? 처리------------
  ##################################################
  same_name = c()
  
  # 모든???코드??? ????????? ??? ?????? 
  for(name in names(total_data2)){
    if(length(unique(total_data2[,name]))==1){
      same_name = c(same_name,name)  
    }
  }
  
  
  total_data2 = total_data2[,!names(total_data2)%in%same_name]
  
  # facotr????????? 변경해????????? 변?????? ??????(기???질환 변???)
  name_ = c()
  col_factor_feature = data.frame()
  
  for(col_ in names(total_data2)){
    df = data.frame(col_, count = length(unique(data.frame(total_data2)[,col_])))
    
    col_factor_feature = rbind(col_factor_feature,df)
  }
  
  factor_col = as.character(subset(col_factor_feature, col_factor_feature$count == 2)$col_)
  
  factor_col2 = factor_col[!str_detect(factor_col,paste0(c("class","ASA","peak_","freq_","cpt_","cre_"), collapse = "|"))]
  
  total_data2[,factor_col2] = lapply(total_data2[,factor_col2], as.factor)
  
  # factor_col = factor_col[-grep("class",factor_col)]  # peak ???징??? ?????? 
  # factor_col = factor_col[c(1:3,24:29)]
  # total_data2[,factor_col] = lapply(total_data2[,factor_col], as.factor)
  
  # factor, 문자??? 변??? ?????? 추출 
  char_name_new = names(unlist(lapply(total_data2, class)[lapply(total_data2, class)%in%c("factor","character")]))
  char_name_new = char_name_new[-grep("class",char_name_new)] # class ?????? ?????? 
  
  
  # ???미?????? ??????
  total_data3 = total_data2[,!names(total_data2)%in%char_name_new]
  
  
  df = c()
  
  for (col_name in char_name_new){
    
    df = cbind(df, dummy(total_data2[,col_name],sep='_'))
    
  }
  
  total_data3 = cbind(total_data3, df)
  
  total_data3$class = as.character(total_data3$class)
  
  
  data_TRUE=subset(total_data3,class=="FALSE")
  data_FALSE=subset(total_data3,class=="TRUE")
  
  # ???????????? 분리 
  inTrain=createDataPartition(1:nrow(data_TRUE),p=0.8,list=FALSE)
  
  
  # ???징만 추출?????? ????????? labeling?????? ???징이 ?????? ?????? ???????????????. 
  train_x = data_TRUE[inTrain,-grep("class",names(data_TRUE))]
  
  # 모든 변?????? 값??? 값이 ??????가??? scale??? ???가 ????????? ??????가 발생?????? ???문에 ?????? 변??? ?????? 
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
  # test_x = test_x[,-control_index]
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
  # return(confusionMatrix(confTest,positive="TRUE"))
  
  
}

model_kfold = function(total_data){
  
  set.seed(12546)
  
  total_data$class = as.character(total_data$class)
  
  total_data$ANE.Time = NULL
  total_data$OP.Time = NULL
  total_data$ASA = as.numeric(total_data$ASA)
  
  total_data$class[total_data$class == "NORMAL"] = "TRUE"
  total_data$class[total_data$class != "TRUE"] = "FALSE"
  
  char_name = names(unlist(lapply(total_data, class)[lapply(total_data, class) %in% c("factor", "character")]))
  
  total_data[is.na(total_data)] = 0
  
  except_name = c("Case_ID", "OP.Name", "OP.Name_re", "EKG", "OP_surgery_type", "OP_Severity")
  total_data2 = total_data[,!names(total_data) %in% except_name]
  
  total_data2 = total_data2 %>% mutate_if(is.character, as.factor)
  
  same_name = c()
  for(name in names(total_data2)){
    if(length(unique(total_data2[,name])) == 1){
      same_name = c(same_name, name)  
    }
  }
  
  total_data2 = total_data2[,!names(total_data2) %in% same_name]
  
  name_ = c()
  col_factor_feature = data.frame()
  
  for(col_ in names(total_data2)){
    df = data.frame(col_, count = length(unique(data.frame(total_data2)[,col_])))
    
    col_factor_feature = rbind(col_factor_feature, df)
  }
  
  factor_col = as.character(subset(col_factor_feature, col_factor_feature$count == 2)$col_)
  
  factor_col2 = factor_col[!str_detect(factor_col, paste0(c("class","ASA","peak_","freq_","cpt_","cre_"), collapse = "|"))]
  
  total_data2[,factor_col2] = lapply(total_data2[,factor_col2], as.factor)
  
  
  char_name_new = names(unlist(lapply(total_data2, class)[lapply(total_data2, class)%in%c("factor", "character")]))
  char_name_new = char_name_new[-grep("class", char_name_new)] 
  
  total_data3 = total_data2[,!names(total_data2) %in% char_name_new]
  
  
  df = c()
  
  for (col_name in char_name_new){
    
    df = cbind(df, dummy(total_data2[,col_name],sep='_'))
    
  }
  
  total_data3 = cbind(total_data3, df)
  
  total_data3$class = as.character(total_data3$class)
  
  
  data_TRUE=subset(total_data3, class == "TRUE")
  data_FALSE=subset(total_data3, class == "FALSE")
  
  set.seed(12546)
  k_fold_result <- createFolds(data_TRUE$class, k = 10, list = TRUE, returnTrain = FALSE)
  
  result <- data.frame(indicators = c("acc", "sens", "spec"))
  for (i in k_fold_result){
    
    train_x = data_TRUE[-i, -grep("class", names(data_TRUE))]
    
    same_name_train = c()
    
    for(name in names(train_x)){
      if(min(train_x[name]) == max(train_x[name])){
        same_name_train = c(same_name_train, name)  
      }
    }
    
    train_x = train_x[,!names(train_x) %in% same_name_train]
    
    train_y = data_TRUE[-i, "class"]
    
    test_TRUE = data_TRUE[i,]
    test=rbind(test_TRUE, data_FALSE)
    test_x = test[,-grep("class", names(test))]
    
    test_x = test_x[,!names(test_x) %in% same_name_train]
    test_y = test[,"class"]
    
    svm.model = svm(train_x, y = NULL,
                    type = 'one-classification',
                    nu = 0.10,
                    scale = TRUE,
                    kernel = "radial")
    
    
    svm.predtrain = predict(svm.model, train_x)
    svm.predtest = predict(svm.model, test_x)
    
    confTrain = table(Predicted = svm.predtrain, Reference = train_y)
    confTest = table(Predicted = svm.predtest, Reference = test_y)
    
    accuracy = mean(svm.predtest == test_y)
    
    sensi = sensitivity(as.factor(svm.predtest), as.factor(test_y), positive = levels(as.factor(test_y))[2])
    
    specif = specificity(as.factor(svm.predtest), as.factor(test_y), negative = levels(as.factor(test_y))[1])
    
    total_result = c(accuracy, sensi, specif)
    
    result <- cbind(result, data.frame(total_result))
    
  }
  
  result <- result[,2:11]
  acc_mean <- mean(as.numeric(result[1,]))
  sens_mean <-  mean(as.numeric(result[2,]))
  spec_mean <-  mean(as.numeric(result[3,]))
  
  result <- cbind(result, c(acc_mean, sens_mean, spec_mean))
  colnames(result) <-c("fold1", "fold2", "fold3", "fold4", "fold5", "fold6", "fold7", "fold8", "fold9", "fold10", "mean")
  rownames(result) <- c("accuracy", "sensitivity", "specificity")
  
  return(result)
}

model_kfold_mace = function(total_data){
  
  set.seed(12546)
  
  total_data$class = as.character(total_data$class)
  
  total_data$ANE.Time = NULL
  total_data$OP.Time = NULL
  total_data$ASA = as.numeric(total_data$ASA)
  
  total_data$class[total_data$class == "NORMAL"] = "TRUE"
  total_data$class[total_data$class != "TRUE"] = "FALSE"
  
  char_name = names(unlist(lapply(total_data, class)[lapply(total_data, class) %in% c("factor", "character")]))
  
  total_data[is.na(total_data)] = 0
  
  except_name = c("Case_ID", "OP.Name", "OP.Name_re", "EKG", "OP_surgery_type", "OP_Severity")
  total_data2 = total_data[,!names(total_data) %in% except_name]
  
  total_data2 = total_data2 %>% mutate_if(is.character, as.factor)
  
  same_name = c()
  for(name in names(total_data2)){
    if(length(unique(total_data2[,name])) == 1){
      same_name = c(same_name, name)  
    }
  }
  
  total_data2 = total_data2[,!names(total_data2) %in% same_name]
  
  name_ = c()
  col_factor_feature = data.frame()
  
  for(col_ in names(total_data2)){
    df = data.frame(col_, count = length(unique(data.frame(total_data2)[,col_])))
    
    col_factor_feature = rbind(col_factor_feature, df)
  }
  
  factor_col = as.character(subset(col_factor_feature, col_factor_feature$count == 2)$col_)
  
  factor_col2 = factor_col[!str_detect(factor_col, paste0(c("class","ASA","peak_","freq_","cpt_","cre_"), collapse = "|"))]
  
  total_data2[,factor_col2] = lapply(total_data2[,factor_col2], as.factor)
  
  
  char_name_new = names(unlist(lapply(total_data2, class)[lapply(total_data2, class)%in%c("factor", "character")]))
  char_name_new = char_name_new[-grep("class", char_name_new)] 
  
  total_data3 = total_data2[,!names(total_data2) %in% char_name_new]
  
  
  df = c()
  
  for (col_name in char_name_new){
    
    df = cbind(df, dummy(total_data2[,col_name],sep='_'))
    
  }
  
  total_data3 = cbind(total_data3, df)
  
  total_data3$class = as.character(total_data3$class)
  
  
  data_TRUE=subset(total_data3, class == "FALSE")
  data_FALSE=subset(total_data3, class == "TRUE")
  
  set.seed(12546)
  k_fold_result <- createFolds(data_TRUE$class, k = 10, list = TRUE, returnTrain = FALSE)
  
  result <- data.frame(indicators = c("acc", "sens", "spec"))
  for (i in k_fold_result){
    
    train_x = data_TRUE[-i, -grep("class", names(data_TRUE))]
    
    same_name_train = c()
    
    for(name in names(train_x)){
      if(min(train_x[name]) == max(train_x[name])){
        same_name_train = c(same_name_train, name)  
      }
    }
    
    train_x = train_x[,!names(train_x) %in% same_name_train]
    
    train_y = data_TRUE[-i, "class"]
    
    test_TRUE = data_TRUE[i,]
    test=rbind(test_TRUE, data_FALSE)
    test_x = test[,-grep("class", names(test))]
    
    test_x = test_x[,!names(test_x) %in% same_name_train]
    test_y = test[,"class"]
    
    svm.model = svm(train_x, y = NULL,
                    type = 'one-classification',
                    nu = 0.10,
                    scale = TRUE,
                    kernel = "radial")
    
    
    svm.predtrain = predict(svm.model, train_x)
    svm.predtest = predict(svm.model, test_x)
    
    confTrain = table(Predicted = svm.predtrain, Reference = train_y)
    confTest = table(Predicted = svm.predtest, Reference = test_y)
    
    accuracy = mean(svm.predtest == test_y)
    
    sensi = sensitivity(as.factor(svm.predtest), as.factor(test_y), positive = levels(as.factor(test_y))[2])
    
    specif = specificity(as.factor(svm.predtest), as.factor(test_y), negative = levels(as.factor(test_y))[1])
    
    total_result = c(accuracy, sensi, specif)
    
    result <- cbind(result, data.frame(total_result))
    
  }
  
  result <- result[,2:11]
  acc_mean <- mean(as.numeric(result[1,]))
  sens_mean <-  mean(as.numeric(result[2,]))
  spec_mean <-  mean(as.numeric(result[3,]))
  
  result <- cbind(result, c(acc_mean, sens_mean, spec_mean))
  colnames(result) <-c("fold1", "fold2", "fold3", "fold4", "fold5", "fold6", "fold7", "fold8", "fold9", "fold10", "mean")
  rownames(result) <- c("accuracy", "sensitivity", "specificity")
  
  return(result)
}

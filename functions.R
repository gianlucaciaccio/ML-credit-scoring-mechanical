#### Function: loopTrain ####
loopTrain <- function(form, data, ...) {
  
  set.seed(111)
  trainGLM <- caret::train(form=form, data=data, ...)
  
  return(trainGLM)
  
}


#### Function: tuningMetrics ####
tuningMetrics <- function(data, lev, model) {
  

  prob <- data$Default
  pred <- data$pred
  actual <- data$obs
  actualN <- ifelse(actual=="Default",1,0)
  
  brier <- mean((prob-actualN)^2)
  logloss <- unname(mnLogLoss(data, lev))
  
  CM <- confusionMatrix(pred,actual)$table
  TP <- as.numeric(CM[1])
  FP <- as.numeric(CM[2])
  FN <- as.numeric(CM[3])
  TN <- as.numeric(CM[4])
  
  Sens <- TP/(TP+FP)
  Spec <- TN/(TN+FN)
  Prec <- TP/(TP+FN)
  Info <- Sens+Spec-1
  
  MCC <- (TP*TN-FP*FN)/
    sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  
  Kappa <- (2*(TP*TN-FP*FN))/
    ((TP+FP)*(FP+TN)+(TP+FN)*(TN+FN))
  
  AUROC <- unname(twoClassSummary(data, lev)[1])
  AUPR <- unname(prSummary(data, lev)[1])
  

  metrics <- c(logloss, brier, MCC, Info, Kappa, 
               AUPR, Prec, Sens, Spec, AUROC)
  
  names(metrics) <- c("LogLoss", "Brier", "MCC", "Informedness", "Kappa",
                      "AUPR", "Precision", "Sens_Recall", "Specificity",
                      "AUROC")

  return(metrics)
  
}



#### Function: predMetrics ####
predMetrics <- function(model, newdata, Ycol, PosClass = "Default") {
  
  require(dplyr)
  require(tibble)
  require(caret)
  require(MLmetrics)
  
  Xtest <- select(newdata, model$finalModel$xNames)
  Ytest <- select(newdata, all_of(Ycol)) %>% pull()
  
  if (!is.factor(Ytest)) {
    stop("The argument 'Ytest' must be a factor")
  }
  
  mdl <- deparse(substitute(model))
  
  kFcv <- model$results %>% 
    pivot_longer(cols = c(LogLoss:AUROC),
                 names_to = "Metrics",values_to = mdl) %>%
    pivot_longer(cols = c(LogLossSD:AUROCSD), 
                 names_to = "metrics2", values_to = "sd") %>%
    mutate(metrics2 = str_replace(metrics2,"SD","")) %>%
    filter(Metrics == metrics2) %>%
    select(-metrics2)
  
  if (class(model)=="rfe") {
    
    kFcv_Best <- kFcv %>%
      filter(Variables == model$optsize)
    
  } else {
    
    if (any(model$bestTune == "none")) {  
      
      kFcv_Best <- kFcv
      
    } else {
      
      kFcv_Best <- model$results %>%
        dplyr::slice(as.numeric(rownames(model$bestTune))) %>% 
        pivot_longer(cols = c(LogLoss:AUROC),
                     names_to = "Metrics",values_to = mdl) %>%
        pivot_longer(cols = c(LogLossSD:AUROCSD), 
                     names_to = "metrics2", values_to = "sd") %>%
        mutate(metrics2 = str_replace(metrics2,"SD","")) %>%
        filter(Metrics == metrics2) %>%
        select(-metrics2)
      
    }
    
  }
  
  if (model$method == "lda") {
    
    df_test <- data.frame(Xtest, Class = Ytest)
    
    probs <- data.frame(obs = df_test$Class,
                        pred = predict(model, newdata = df_test),
                        predict(model, newdata = df_test,type = "prob")) %>%
      mutate(model = mdl)

  } else {
    
    probs <- extractProb(models = list(model), 
                         testX = Xtest,
                         testY = Ytest) %>% 
      mutate(model = mdl) %>% 
      filter(dataType == "Test") %>%
      select(-c(object, dataType))
    
  }
  
  test_prob <- probs %>% select(all_of(PosClass)) %>% pull()
  test_pred <- probs %>% select(pred) %>% pull()
  test_actual <- probs %>% select(obs) %>% pull()
  test_actualN <- ifelse(test_actual==PosClass,1,0)
  
  test_brier <- mean((test_prob-test_actualN)^2)
  test_logloss <- unname(mnLogLoss(data = probs, lev = levels(probs$obs)))
  
  test_CM <- confusionMatrix(test_pred,test_actual)$table
  test_TP <- as.numeric(test_CM[1])
  test_FP <- as.numeric(test_CM[2])
  test_FN <- as.numeric(test_CM[3])
  test_TN <- as.numeric(test_CM[4])
  
  test_Sens <- test_TP/(test_TP+test_FP)
  test_Spec <- test_TN/(test_TN+test_FN)
  test_Prec <- test_TP/(test_TP+test_FN)
  test_Info <- test_Sens+test_Spec-1
  
  test_MCC <- (test_TP*test_TN-test_FP*test_FN)/
    sqrt((test_TP+test_FP)*(test_TP+test_FN)*(test_TN+test_FP)*(test_TN+test_FN))
  
  test_Kappa <- (2*(test_TP*test_TN-test_FP*test_FN))/
    ((test_TP+test_FP)*(test_FP+test_TN)+(test_TP+test_FN)*(test_TN+test_FN))
  
  test_AUROC <- unname(twoClassSummary(data = probs, lev = levels(probs$obs))[1])
  test_AUPR <- unname(prSummary(data = probs, lev = levels(probs$obs))[1])
  
  name_metrics <- c("LogLoss", "Brier", "MCC", "Informedness", "Kappa",
                    "AUPR", "Precision", "Sens_Recall", "Specificity", "AUROC")
  
  test_metrics <- tibble(Metrics = name_metrics,
                         !! mdl := c(test_logloss, test_brier, test_MCC,
                                     test_Info, test_Kappa, test_AUPR, test_Prec,
                                     test_Sens, test_Spec, test_AUROC))
  
  
  # metr_diff = unname(abs(train_metrics[, 2]-test_metrics[, 2]))
  # 
  # diff_metrics <- data.frame(Metrics = name_metrics,
  #                            diff = metr_diff) %>%
  #   tibble(!! mdl := diff) %>%
  #   select(-diff)
  
  
  return(list(kFcv = kFcv, kFcv_Best = kFcv_Best,
              test = test_metrics,
              test_CM = test_CM, probs = probs))
  
}

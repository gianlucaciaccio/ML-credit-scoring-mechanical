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
  J <- Sens+Spec-1
  
  MCC <- (TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  
  Kappa <- (2*(TP*TN-FP*FN))/((TP+FP)*(FP+TN)+(TP+FN)*(TN+FN))
  
  AUROC <- unname(twoClassSummary(data, lev)[1])
  AUPR <- unname(prSummary(data, lev)[1])
  

  metrics <- c(logloss, brier, MCC, J, Kappa, 
               AUPR, Prec, Sens, Spec, AUROC)
  
  names(metrics) <- c("LogLoss", "Brier", "MCC", "J", "Kappa",
                      "AUPR", "Precision", "Sens_Recall", "Specificity",
                      "AUROC")

  return(metrics)
  
}



#### Function: predMetrics ####
predMetrics <- function(model, Xtest, Ytest, PosClass = "Default") {
  
  require(dplyr)
  require(tibble)
  require(caret)
  require(MLmetrics)
  
  #Xtest <- select(newdata, model$finalModel$xNames)
  #Ytest <- select(newdata, all_of(Ycol)) %>% pull()
  
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
  

  probs <- extractProb(models = list(model), 
                       testX = Xtest,
                       testY = Ytest) %>% 
    mutate(model = mdl) %>% 
    filter(dataType == "Test") %>%
    select(-c(object, dataType))

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
  test_J <- test_Sens+test_Spec-1
  
  test_MCC <- (test_TP*test_TN-test_FP*test_FN)/
    sqrt((test_TP+test_FP)*(test_TP+test_FN)*(test_TN+test_FP)*(test_TN+test_FN))
  
  test_Kappa <- (2*(test_TP*test_TN-test_FP*test_FN))/
    ((test_TP+test_FP)*(test_FP+test_TN)+(test_TP+test_FN)*(test_TN+test_FN))
  
  test_AUROC <- unname(twoClassSummary(data = probs, lev = levels(probs$obs))[1])
  test_AUPR <- unname(prSummary(data = probs, lev = levels(probs$obs))[1])
  
  name_metrics <- c("LogLoss", "Brier", "MCC", "J", "Kappa",
                    "AUPR", "Precision", "Sens_Recall", "Specificity", "AUROC")
  
  test_metrics <- tibble(Metrics = name_metrics,
                         !! mdl := c(test_logloss, test_brier, test_MCC,
                                     test_J, test_Kappa, test_AUPR, test_Prec,
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




#### Function: compareMetrics ####
compareMetrics <- function(list, metrics = c("LogLoss", "Brier", "MCC",
                                             "J", "Kappa", "AUPR", "Precision",
                                             "Sens_Recall", "Specificity",
                                             "AUROC"), long = FALSE) {
  
  require(purrr)
  require(dplyr)
  
  if (!inherits(list, "list")) {
    stop("The argument 'list' must be an object of class list")
  }
  
  metrics = match.arg(metrics, several.ok = TRUE)
  

  suppressWarnings(
    
    data_compare <- map(list, ~ .x$test) %>%
                     reduce(., left_join, by = "Metrics") %>%
                     mutate(Winner = ifelse(
                       Metrics %in% c("LogLoss", "Brier"),
                       pmap_chr(across(-Metrics),~ names(c(...)[which.min(c(...))])),
                       pmap_chr(across(-Metrics),~ names(c(...)[which.max(c(...))]))
                       )) %>% filter(Metrics %in% metrics)
    
    )
    

  
  
  suppressWarnings(
    
    data_compareLONG <- data_compare %>%
      select(-Winner) %>%
      pivot_longer(cols = -c(Metrics),
                   names_to = "model") %>%
      pivot_wider(id_cols = "model",
                  names_from = "Metrics",
                  values_from = "value") %>%
      select(model, all_of(metrics))
    
    )
    
  
  Winners <- data_compare %>%
    select(Metrics, Winner)
  
  data_aggregate <- data_compare  %>%
    group_by(Winner) %>%
    summarize(n=n()) %>%
    arrange(-n)
  
  
  if (long == FALSE) {
    
    return(list("Comparison" = data_compare,
                "Counts" = data_aggregate))
    
  } else {
    
    return(list("Comparison" = data_compareLONG,
                "Winners" = Winners,
                "Counts" = data_aggregate))
    
  }
  
}





#### Function: plotTuning ####
plotTuning <- function(perfmod, x_axis, color_by = NULL)  {
  
  modelperf <- gsub(".p", "", x = deparse(substitute(perfmod)))
  
  df <- data.frame(perfmod$kFcv)
  
  if (is.null(x_axis) | any(x_axis %in% colnames(perfmod$kFcv)) == FALSE) {
    
    stop("argument x_axis is not a right hyperparameter to plot")
    
  } 
  
  
  
  if (!is.null(color_by) && any(color_by %in% colnames(perfmod$kFcv)) == FALSE) {
    
    stop("argument color_by is not a right hyperparameter to plot")
    
  } else if (is.null(color_by) && is.numeric(df %>%
                                             select(all_of(x_axis)) %>%
                                             pull())) {
    
    plt <- df %>%
      ggplot(aes(x = get(x_axis), y = get(modelperf))) +
      geom_point(shape = 1, color = "dodgerblue") +
      geom_line(color = "dodgerblue") +
      facet_wrap(~Metrics, scales = "free") +
      labs(x = x_axis, y = modelperf) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(), 
            strip.background = element_rect(fill = "#FDF7E2"))
    
    
  } else if (is.null(color_by) && !is.numeric(df %>%
                                             select(all_of(x_axis)) %>%
                                             pull())) {
    
    plt <- df %>%
      ggplot(aes(x = get(x_axis), y = get(modelperf), group = 1)) +
      geom_point(shape = 1, color = "dodgerblue") +
      geom_line(color = "dodgerblue") +
      facet_wrap(~Metrics, scales = "free") +
      labs(x = x_axis, y = modelperf) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(), 
            strip.background = element_rect(fill = "#FDF7E2"))
    
  } else if (!is.null(color_by) && any(color_by %in% colnames(perfmod$kFcv)) == TRUE) {

    plt <- df %>%
      ggplot(aes(x = get(x_axis), y = get(modelperf),
                 color = factor(get(color_by)))) +
      geom_point(shape = 1) +
      geom_line() +
      facet_wrap(~Metrics, scales = "free") +
      labs(x = x_axis, y = modelperf, color = color_by) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(), 
            strip.background = element_rect(fill = "#FDF7E2"))
    
    
  }
  
  
  return(plt)
  
}



#### Function: plotProbs ####
plotProbs <- function(x) {
  
  require(cowplot)
  
  df <- x$probs
  
  
  plt_hist <- df %>%
    ggplot(aes(x=Default, fill = obs)) +
    geom_histogram(color = "white", bins = 30, alpha=0.5) +  
    scale_fill_manual(values = c("orangered","green")) +
    scale_x_continuous(breaks = seq(0,1,0.25)) +
    facet_wrap(~ fct_rev(obs), scales = "free") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.background = element_rect(fill = "#FDF7E2"),
          legend.position = "none") +
    labs(title = "Distribution of predicted probability of default",
         x = "Probability of default")
  
  
  plt_dens <- df %>%
    ggplot(aes(x=Default, color = obs, fill = obs)) +
    geom_density(alpha=0.2) +  
    scale_color_manual(values = c("orangered","green")) +
    scale_fill_manual(values = c("orangered","green")) +
    scale_x_continuous(breaks = seq(0,1,0.25)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    labs(title = "Predicted class probabilities",
         x = "Probability of default",
         fill = "status", color = "status")
  
  
  plt <- plot_grid(plt_hist, plt_dens, nrow = 2)
  
  return(plt)
}



#### Function: plotCurves ####
plotCurves <- function(x, type = c("roc", "proc", "prg", "cc", "ALL"), ...) {

require(MLeval)


if ("probs" %in% names(x) == TRUE) {
  
  data <- x$probs %>%
    select(Default,InBonis,obs, Group = model) %>%
    mutate(Group = str_to_lower(Group))
  
} else {
  
  data <- x %>%
    map(~ .x$probs %>%
          select(Default,InBonis, obs, Group = model) %>%
          mutate(Group = str_to_lower(Group))) %>%
    bind_rows()
  
}


evalML <- evalm(data, showplots = FALSE, silent = TRUE,
                positive = "Default", rlinethick = 0.6, ...)


plt_roc <- evalML$roc + 
  ggtitle("ROC curve")

plt_proc <- evalML$proc + 
  ggtitle("Precision-Recall curve")

plt_prg <- evalML$prg + 
  ggtitle("Precision-Recall Gain curve")

plt_cc <- evalML$cc + 
  ggtitle("Calibration curve")


type <- match.arg(type)


if (type == "roc") {
  
  return(plt_roc)
  
} else if (type == "proc") {
  
  return(plt_proc)
  
  
} else if (type == "prg") {
  
  return(plt_prg)
  
  
} else if (type == "cc") {
  
  return(plt_cc)
  
  
} else if (type == "ALL") {
  
  plt_all <- plot_grid(plt_roc, plt_proc,
                       plt_prg, plt_cc)
  
  return(plt_all)
  
}


}

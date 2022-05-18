rm(list = ls())
load("split.RData")
source("libraries.R")
source("functions.R")

models <- expand.grid(CostDebt = "cost.debt",
                      Liquidity = c("current.ratio","quick.ratio","wc.ratio"),
                      Leverage = c("debt.equity","findep"),
                      Coverage = c("cash.debt","asset.cov"),
                      Profitability = c("roe", "roce", "roa", "profit.margin"),
                      Efficiency = c("asset.turnover", "DPO", "DSO")) %>%
  mutate(formulas = paste(CostDebt, Liquidity, Leverage, Coverage, 
                          Profitability, Efficiency, sep = "+"),
         formulas = paste("status ~",formulas,sep = " ")) %>%
  select(formulas) %>% pull()

models

#### Set resampling and metrics ####
fitctrl <- trainControl(method = "repeatedcv",
                        number = 10, 
                        repeats = 10,
                        sampling = "smote",
                        summaryFunction = tuningMetrics, 
                        classProbs = TRUE)

#### GLM ####
glm_mods <- lapply(models, function(form) loopTrain(as.formula(form),
                                                    data = data_train,
                                                    method = "glm",
                                                    trControl = fitctrl,
                                                    metric = "Informedness",
                                                    maximize = TRUE))

names(glm_mods) <- paste("GLM", seq(1,144,1),sep = "-")

glm_mods.p <- map(glm_mods, ~ predMetrics(.x, data_test, Ycol = "status"))

glm_mods.test <- map(glm_mods.p, ~ .x$test)

glm_mods.test <- lapply(names(glm_mods.test), 
  function(x) setNames(glm_mods.test[[x]], c(names(glm_mods.test[[x]])[1], x)))

glm_mods.test %>% 
  reduce(., left_join, by = "Metrics") %>%
  pivot_longer(cols = -c(Metrics), names_to = "model") %>%
  pivot_wider(id_cols = "model", names_from = "Metrics",
              values_from = "value") %>%
  mutate(formula = models) %>%
  select(formula, everything()) %>%
  top_n(5,wt = Informedness) %>%
  select(-formula) %>%
  pivot_longer(cols = -model, names_to = "metrics") %>%
  pivot_wider(id_cols = metrics, names_from = model,values_from = value)  %>%
  mutate(Winner = ifelse(metrics %in% c("LogLoss", "Brier"),
                         pmap_chr(across(-metrics),
                                  ~ names(c(...)[which.min(c(...))])),
                         pmap_chr(across(-metrics),
                                  ~ names(c(...)[which.max(c(...))]))))

glm_mods_BEST <- list(model=glm_mods$`GLM-89`,
                      perf=glm_mods.p$`GLM-89`,
                      test=glm_mods.test[[89]])


save(fitctrl, glm_mods, glm_mods.p, file = "GLM-Mods.RData")
save(glm_mods_BEST, file = "GLM-Mods-BEST.RData")

#### LDA ####
lda_mods <- lapply(models, function(form) loopTrain(as.formula(form),
                                                    data = data_train,
                                                    method = "lda",
                                                    trControl = fitctrl,
                                                    metric = "Informedness",
                                                    maximize = TRUE))

names(lda_mods) <- paste("LDA", seq(1,144,1),sep = "-")


lda_mods.p <- map(lda_mods, ~ predMetrics(.x, data_test, Ycol = "status"))

lda_mods.test <- map(lda_mods.p, ~ .x$test)

lda_mods.test <- lapply(names(lda_mods.test), 
                        function(x) setNames(lda_mods.test[[x]], c(names(lda_mods.test[[x]])[1], x)))

lda_mods.test %>% 
  reduce(., left_join, by = "Metrics") %>%
  pivot_longer(cols = -c(Metrics), names_to = "model") %>%
  pivot_wider(id_cols = "model", names_from = "Metrics",
              values_from = "value") %>%
  mutate(formula = models) %>%
  select(formula, everything()) %>%
  top_n(5,wt = Informedness) %>%
  select(-formula) %>%
  pivot_longer(cols = -model, names_to = "metrics") %>%
  pivot_wider(id_cols = metrics, names_from = model,values_from = value)  %>%
  mutate(Winner = ifelse(metrics %in% c("LogLoss", "Brier"),
                         pmap_chr(across(-metrics),
                                  ~ names(c(...)[which.min(c(...))])),
                         pmap_chr(across(-metrics),
                                  ~ names(c(...)[which.max(c(...))]))))


lda_mods_BEST <- list(model=lda_mods$`LDA-89`,
                      perf=lda_mods.p$`LDA-89`,
                      test=lda_mods.test[[89]])

save(fitctrl, lda_mods, lda_mods.p, file = "LDA-Mods.RData")
save(lda_mods_BEST, file = "LDA-Mods-BEST.RData")


#### GLM vs LDA ####
list(glm_mods_BEST$test, lda_mods_BEST$test) %>%
  reduce(., left_join) %>%
  mutate(Winner = ifelse(Metrics %in% c("LogLoss", "Brier"),
                         pmap_chr(across(-Metrics),
                                  ~ names(c(...)[which.min(c(...))])),
                         pmap_chr(across(-Metrics),
                                  ~ names(c(...)[which.max(c(...))]))))


glm_mods.p$`GLM-91`$test_CM
lda_mods.p$`LDA-91`$test_CM

# Winner: glm
# status ~ cost.debt + quick.ratio + findep + cash.debt + profit.margin + DPO












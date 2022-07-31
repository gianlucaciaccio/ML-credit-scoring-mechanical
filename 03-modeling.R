rm(list = ls())
load("split.RData")
source("libraries.R")
source("functions.R")

#### fit without PCA ####
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     sampling = "smote",
                     summaryFunction = tuningMetrics,
                     classProbs = TRUE)


#### fit withouto SMOTE (for class weight) ####
ctrl_w <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 5,
                      summaryFunction = tuningMetrics,
                      classProbs = TRUE)


#### fit with PCA ####
ctrl_pca <- trainControl(method = "repeatedcv",
                         number = 10,
                         repeats = 5,
                         preProcOptions = list(pcaComp = 7),
                         sampling = "smote",
                         summaryFunction = tuningMetrics,
                         classProbs = TRUE)


#### GLM SW ####
set.seed(111)
GLM_SW <- train(Xtrain, Ytrain,
                method = "glmStepAIC", 
                trControl = ctrl, 
                metric = "J")

GLM_SW$finalModel
summary(GLM_SW)

GLM_SW.p <- predMetrics(GLM_SW, Xtest, Ytest)
GLM_SW.p$kFcv_Best
GLM_SW.p$test
GLM_SW.p$test_CM
plotProbs(GLM_SW.p)
plotCurves(GLM_SW.p, type = "ALL")


#### GLM PCA ####
set.seed(111)
GLM_PCA <- train(Xtrain, Ytrain, method = "glm",
                 preProcess = "pca",
                 trControl = ctrl_pca, 
                 metric = "J")

GLM_PCA$finalModel
summary(GLM_PCA)

GLM_PCA.p <- predMetrics(GLM_PCA, Xtest, Ytest)
GLM_PCA.p$test
GLM_PCA.p$test_CM
plotProbs(GLM_PCA.p)
plotCurves(GLM_PCA.p, type = "ALL")


# Save Workspace before R session abort 
save.image("modeling.RData")


#### GLM Lasso ####
GLM_LASS_grid <- expand.grid(alpha = 1,
                             lambda = seq(0.001, 0.1, by = 0.001))

set.seed(111)
GLM_LASS <- train(Xtrain, Ytrain, method = "glmnet",
                  trControl = ctrl,
                  tuneGrid = GLM_LASS_grid, 
                  metric = "J")

GLM_LASS$bestTune
coef(GLM_LASS$finalModel, GLM_LASS$finalModel$lambdaOpt)
# Variables selected: 3
# wc.ratio, findep, cost.debt


GLM_LASS.p <- predMetrics(GLM_LASS, Xtest, Ytest)
plotTuning(GLM_LASS.p, x_axis = "lambda")
GLM_LASS.p$kFcv_Best
GLM_LASS.p$test
GLM_LASS.p$test_CM
plotProbs(GLM_LASS.p)
plotCurves(GLM_LASS.p, type = "ALL")




# Save Workspace before R session abort 
save.image("modeling.RData")



#### GLM Ridge ####
GLM_RIDGE_grid <- expand.grid(alpha = 0,
                              lambda = seq(0.001, 0.1, by = 0.001))

set.seed(111)
GLM_RIDGE <- train(Xtrain, Ytrain, method = "glmnet",
                   trControl = ctrl,
                   tuneGrid = GLM_RIDGE_grid, 
                   metric = "J")

GLM_RIDGE$bestTune
round(coef(GLM_RIDGE$finalModel, GLM_RIDGE$finalModel$lambdaOpt),4)
# Variables selected: 15
# All

GLM_RIDGE.p <- predMetrics(GLM_RIDGE, Xtest, Ytest)
plotTuning(GLM_RIDGE.p, x_axis = "lambda")
GLM_RIDGE.p$kFcv_Best
GLM_RIDGE.p$test
GLM_RIDGE.p$test_CM
plotProbs(GLM_RIDGE.p)
plotCurves(GLM_RIDGE.p, type = "ALL")


compareMetrics(list(GLM_LASS.p$test,GLM_RIDGE.p$test))


# Save Workspace before R session abort 
save.image("modeling.RData")



#### GLM Elastic Net ####
GLM_ELAS_grid <- expand.grid(alpha = seq(0.5,0.95,0.03),
                             lambda = c(1:10*0.001, 2:10*0.01))

set.seed(111)
GLM_ELAS <- train(Xtrain, Ytrain, method = "glmnet",
                  trControl = ctrl,
                  tuneGrid = GLM_ELAS_grid, 
                  metric = "J")

GLM_ELAS$bestTune
coef(GLM_ELAS$finalModel, GLM_ELAS$finalModel$lambdaOpt)
# Variables selected: 4
# wc.ratio, findep, profit.margin, cost.debt

GLM_ELAS.p <- predMetrics(GLM_ELAS, Xtest, Ytest)
plotTuning(GLM_ELAS.p, x_axis = "lambda", color_by = "alpha")
GLM_ELAS.p$kFcv_Best
GLM_ELAS.p$test
GLM_ELAS.p$test_CM
plotProbs(GLM_ELAS.p)
plotCurves(GLM_ELAS.p, type = "ALL")



compareMetrics(list(GLM_LASS.p, GLM_RIDGE.p, GLM_ELAS.p))
plotCurves(list(GLM_LASS.p, GLM_RIDGE.p, GLM_ELAS.p), type = "ALL")
compareMetrics(list(GLM_LASS.p, GLM_ELAS.p))



# Save Workspace before R session abort 
save.image("modeling.RData")



#### GLM Elastic Net 2 ####
GLM_ELAS2_grid <- expand.grid(alpha = seq(0.95,0.995,0.005),
                              lambda = seq(0.1,1,0.05))

set.seed(111)
GLM_ELAS2 <- train(Xtrain, Ytrain, method = "glmnet",
                   trControl = ctrl,
                   tuneGrid = GLM_ELAS2_grid, 
                   metric = "J")

GLM_ELAS2$bestTune
coef(GLM_ELAS2$finalModel, GLM_ELAS2$finalModel$lambdaOpt)
# Variables selected: 3
# wc.ratio, findep, cost.debt

GLM_ELAS2.p <- predMetrics(GLM_ELAS2, Xtest, Ytest)
plotTuning(GLM_ELAS2.p, x_axis = "lambda", color_by = "alpha")
GLM_ELAS2.p$kFcv_Best
GLM_ELAS2.p$test
GLM_ELAS2.p$test_CM
plotProbs(GLM_ELAS2.p)
plotCurves(GLM_ELAS2.p, type = "ALL")



compareMetrics(list(GLM_ELAS.p, GLM_ELAS2.p))
plotCurves(list(GLM_ELAS.p, GLM_ELAS2.p), type = "ALL")



# Save Workspace before R session abort 
save.image("modeling.RData")


#### LDA SW ####
LDA_SW_grid <- expand.grid(direction = c("both", "forward", "backward"),
                           maxvar = Inf)

set.seed(111)
LDA_SW <- train(Xtrain, Ytrain,
                method = "stepLDA", 
                trControl = ctrl,
                tuneGrid = LDA_SW_grid, 
                metric = "J")

LDA_SW$finalModel
LDA_SW$bestTune
# Variables selected: 10
# current.ratio, quick.ratio, wc.ratio, debt.equity, findep, 
# asset.cov, roe, profit.margin, cost.debt, DSO

LDA_SW.p <- predMetrics(LDA_SW, Xtest, Ytest)
plotTuning(LDA_SW.p, x_axis = "direction")
LDA_SW.p$kFcv_Best
LDA_SW.p$test
LDA_SW.p$test_CM
plotProbs(LDA_SW.p)
plotCurves(LDA_SW.p, type = "ALL")


# Save Workspace before R session abort 
save.image("modeling.RData")


#### LDA PCA ####
set.seed(111)
LDA_PCA <- train(Xtrain, Ytrain, method = "lda",
                 preProcess = "pca",
                 trControl = ctrl_pca, 
                 metric = "J")

LDA_PCA$finalModel

LDA_PCA.p <- predMetrics(LDA_PCA, Xtest, Ytest)
LDA_PCA.p$kFcv_Best
LDA_PCA.p$test
LDA_PCA.p$test_CM
plotProbs(LDA_PCA.p)
plotCurves(LDA_PCA.p, type = "ALL")


# Save Workspace before R session abort 
save.image("modeling.RData")


#### LDA with Regularization ####
LDA_REGUL_grid <- expand.grid(diagonal = FALSE,
                              lambda = seq(0,1,0.01))

set.seed(111)
LDA_REGUL <- train(Xtrain, Ytrain,
                   method = "sda", 
                   trControl = ctrl,
                   tuneGrid = LDA_REGUL_grid, 
                   metric = "J")

LDA_REGUL$finalModel$beta

# Variables selected: 15
# ALL 

LDA_REGUL$bestTune

LDA_REGUL.p <- predMetrics(LDA_REGUL, Xtest, Ytest)
plotTuning(LDA_REGUL.p, x_axis = "lambda")
LDA_REGUL.p$kFcv_Best
LDA_REGUL.p$test
LDA_REGUL.p$test_CM
plotProbs(LDA_REGUL.p)
plotCurves(LDA_REGUL.p, type = "ALL")


# Save Workspace before R session abort 
save.image("modeling.RData")


#### Define model weights (they sum to one) ####
model_weights <- ifelse(Ytrain == "Default",
                        (1/table(Ytrain)[1]) * 0.5,
                        (1/table(Ytrain)[2]) * 0.5)


#model_weights2 <- ifelse(Ytrain == "Default",
#                         length(Ytrain)/table(Ytrain)[1],
#                         length(Ytrain)/table(Ytrain)[2])#



#### Decision Tree (CART) with class weights ####
set.seed(111)
CARTw <- train(Xtrain, Ytrain,
              method = "rpart",
              trControl = ctrl_w, 
              tuneLength = 50,
              metric = "J",
              weights = model_weights)

CARTw$bestTune
varImp(CARTw)
plot(varImp(CARTw))

CARTw.p <- predMetrics(CARTw,Xtest,Ytest)
plotTuning(CARTw.p, x_axis = "cp")
CARTw.p$kFcv_Best
CARTw.p$test
CARTw.p$test_CM
plotProbs(CARTw.p)
plotCurves(CARTw.p, type = "ALL")


# Save Workspace before R session abort 
save.image("modeling.RData")



#### Decision Tree (CART) with smote ####
set.seed(111)
CARTs <- train(Xtrain, Ytrain,
               method = "rpart",
               trControl = ctrl, 
               tuneLength = 50,
               metric = "J")

CARTs$bestTune
varImp(CARTs)
plot(varImp(CARTs))

CARTs.p <- predMetrics(CARTs,Xtest,Ytest)
plotTuning(CARTs.p, x_axis = "cp")
CARTs.p$kFcv_Best
CARTs.p$test
CARTs.p$test_CM
plotProbs(CARTs.p)
plotCurves(CARTs.p, type = "ALL")


compareMetrics(list(CARTw.p,CARTs.p))
plotCurves(list(CARTw.p,CARTs.p), type = "ALL")

# Save Workspace before R session abort 
save.image("modeling.RData")




#### Random Forest with class weights ####
RF_grid <- expand.grid(mtry = c(5,7,9,11))

set.seed(111)
RFw <- train(Xtrain, Ytrain,
             method = "rf",
             trControl = ctrl_w,
             tuneGrid = RF_grid,
             metric = "J",
             weights = model_weights)

RFw$bestTune
varImp(RFw)
plot(varImp(RFw))

RFw.p <- predMetrics(RFw,Xtest,Ytest)
plotTuning(RFw.p, x_axis = "mtry")
RFw.p$kFcv_Best
RFw.p$test
RFw.p$test_CM
plotProbs(RFw.p)
plotCurves(RFw.p, type = "ALL")




# Save Workspace before R session abort 
save.image("modeling.RData")



#### Random Forest with smote ####
set.seed(111)
RFs <- train(Xtrain, Ytrain,
             method = "rf",
             trControl = ctrl,
             tuneGrid = RF_grid,
             metric = "J")

RFs$bestTune
varImp(RFs)
plot(varImp(RFs))

RFs.p <- predMetrics(RFs,Xtest,Ytest)
plotTuning(RFs.p, x_axis = "mtry")
RFs.p$kFcv_Best
RFs.p$test
RFs.p$test_CM
plotProbs(RFs.p)
plotCurves(RFs.p, type = "ALL")




# Save Workspace before R session abort 
save.image("modeling.RData")


#### Gradient Boosting Machine with class weights ####
GBMw_grid <- expand.grid(n.trees = 1000,
                        interaction.depth = c(1,3,6),
                        shrinkage = 0.01,
                        n.minobsinnode = c(1,2,3,5))
set.seed(111)
GBMw <- train(Xtrain, Ytrain,
              method = "gbm",
              trControl = ctrl_w,
              tuneGrid = GBMw_grid,
              metric = "J",
              weights = model_weights)

GBMw$bestTune

GBMw.p <- predMetrics(GBMw,Xtest,Ytest)
plotTuning(GBMw.p, x_axis = "n.minobsinnode",
           color_by = "interaction.depth")
GBMw.p$kFcv_Best
GBMw.p$test
GBMw.p$test_CM
plotProbs(GBMw.p)
plotCurves(GBMw.p, type = "ALL")


# Save Workspace before R session abort 
save.image("modeling.RData")




#### Gradient Boosting Machine with smote ####
GBMs_grid <- expand.grid(n.trees = 1000,
                         interaction.depth = 1,
                         shrinkage = 0.01,
                         n.minobsinnode = c(1,5,7))
set.seed(111)
GBMs <- train(Xtrain, Ytrain,
              method = "gbm",
              trControl = ctrl,
              tuneGrid = GBMs_grid,
              metric = "J")

GBMs$bestTune

GBMs.p <- predMetrics(GBMs,Xtest,Ytest)
plotTuning(GBMs.p, x_axis = "n.minobsinnode",
           color_by = "interaction.depth")
GBMs.p$kFcv_Best
GBMs.p$test
GBMs.p$test_CM
plotProbs(GBMs.p)
plotCurves(GBMs.p, type = "ALL")


compareMetrics(list(CARTw.p,CARTs.p,RFs.p,RFw.p,GBMw.p,GBMs.p))
plotCurves(list(CARTw.p,CARTs.p,RFs.p,RFw.p,GBMw.p,GBMs.p), type = "ALL")

# Save Workspace before R session abort 
save.image("modeling.RData")





#### Support Vector Machine - linear kernel ####
SVM_LIN_grid <- expand.grid(C = 2**(-2:5))

set.seed(111)
SVM_LIN <- train(Xtrain, Ytrain,
                 method = "svmLinear", 
                 trControl = ctrl,
                 tuneGrid = SVM_LIN_grid, 
                 preProcess = c("center", "scale"),
                 metric = "J")

SVM_LIN$bestTune

SVM_LIN.p <- predMetrics(SVM_LIN,Xtest,Ytest)
plotTuning(SVM_LIN.p, x_axis = "C")
SVM_LIN.p$kFcv_Best
SVM_LIN.p$test
SVM_LIN.p$test_CM
plotProbs(SVM_LIN.p)
plotCurves(SVM_LIN.p, type = "ALL")



# Save Workspace before R session abort 
save.image("modeling.RData")




#### SVM linear 2 ####
SVM_LIN2_grid <- expand.grid(C = seq(0.50,0.95, 0.075))

set.seed(111)
SVM_LIN2 <- train(Xtrain, Ytrain,
                  method = "svmLinear", 
                  trControl = ctrl,
                  tuneGrid = SVM_LIN2_grid, 
                  preProcess = c("center", "scale"),
                  metric = "J")

SVM_LIN2$bestTune

SVM_LIN2.p <- predMetrics(SVM_LIN2,Xtest,Ytest)
plotTuning(SVM_LIN2.p, x_axis = "C")
SVM_LIN2.p$kFcv_Best
SVM_LIN2.p$test
SVM_LIN2.p$test_CM
plotProbs(SVM_LIN2.p)
plotCurves(SVM_LIN2.p, type = "ALL")


compareMetrics(list(SVM_LIN.p,SVM_LIN2.p))


# Save Workspace before R session abort 
save.image("modeling.RData")



#### SVM linear 3 ####
SVM_LIN3_grid <- expand.grid(C = c(seq(0.48,0.52,0.012),
                                   seq(0.675,0.75, 0.025)))

set.seed(111)
SVM_LIN3 <- train(Xtrain, Ytrain,
                  method = "svmLinear", 
                  trControl = ctrl,
                  tuneGrid = SVM_LIN3_grid, 
                  preProcess = c("center", "scale"),
                  metric = "J")

SVM_LIN3$bestTune

SVM_LIN3.p <- predMetrics(SVM_LIN3,Xtest,Ytest)
plotTuning(SVM_LIN3.p, x_axis = "C")
SVM_LIN3.p$kFcv_Best
SVM_LIN3.p$test
SVM_LIN3.p$test_CM
plotProbs(SVM_LIN3.p)
plotCurves(SVM_LIN3.p, type = "ALL")

compareMetrics(list(SVM_LIN2.p,SVM_LIN3.p))


# Save Workspace before R session abort 
save.image("modeling.RData")


#### Comparison ####
compareMetrics(list(GLM_SW.p, GLM_PCA.p, GLM_LASS.p, GLM_RIDGE.p, GLM_ELAS.p,
                    LDA_SW.p, LDA_PCA.p, LDA_REGUL.p, CARTs.p, RFs.p, GBMs.p,
                    SVM_LIN2.p), long = TRUE)$Comparison %>% arrange(-J)


compareMetrics(list(GLM_SW.p, GLM_PCA.p, GLM_LASS.p, GLM_RIDGE.p, GLM_ELAS.p,
                    LDA_SW.p, LDA_PCA.p, LDA_REGUL.p,
                    SVM_LIN2.p))

compareMetrics(list(GLM_LASS.p, SVM_LIN2.p))

plotCurves(list(GLM_LASS.p, SVM_LIN2.p), type = "proc")
plotCurves(list(GLM_LASS.p, SVM_LIN2.p), type = "cc")


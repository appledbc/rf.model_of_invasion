# Author:
# Date: Tue Jan 10 2023
# Last Modified by:
# Last Modified time: Tue Jan 10 2023
# Email:
# Description: machine learning

# cleaning memory
cat("\014")
rm(list = ls())
gc()

#  loading packages
# You have loaded plyr after dplyr - this is likely to cause problems.
# If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
# library(plyr); library(dplyr)
#
# install.packages(c("patchwork", "skimr"))
library(caret)
library(data.table)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(purrr)
library(readr)
library(readxl)
library(skimr)
library(dplyr)
library(caretEnsemble)
library(doParallel)
library(mgcv)
library(naivebayes)

# set your own working directory
# G:\我的坚果云\2022_06.生物入侵_随机森林\Results20240220
file_path <- "G:/我的坚果云/2022_06.生物入侵_随机森林"

setwd(file_path)
getwd()

# save to .Rdata
# 20240220.X000.raw_invasion.data
load(file = "./Results20240220/20240220.X000.raw_invasion.data.Rdata")
ls()

str(invasion_list04)

# ---------------------------------------------------------------------------- #
#                                Data splitting                                #
# ---------------------------------------------------------------------------- #
# create data partition
# 80% for training; 20% for testing
set.seed(2024)
train.index <- createDataPartition(invasion_list04$invasion.status, p = 0.80, list = F, times = 1)

train.set <- invasion_list04[train.index, ] # i = 1:m

test.set <- invasion_list04[-train.index, ] # i = n-m:n

train.set <- data.frame(train.set)
test.set <- data.frame(test.set)

str(train.index)
str(train.set)
str(test.set)

# explanatory variables
x <- train.set[, colnames(train.set) != "invasion.status"]
head(x)

# invasion status variable
y <- train.set$invasion.status
y

# Compute imbalance ratio
library(imbalance)

imbalance::imbalanceRatio(train.set, classAttr = "invasion.status")
imbalance::imbalanceRatio(test.set, classAttr = "invasion.status")

# ---------------------------------------------------------------------------- #
#                                  Description                                 #
# ---------------------------------------------------------------------------- #
rstatix::get_summary_stats(train.set)

# IT DOES NOT WORK !!!
# # feature plot
# featurePlot(
#       x = train.set[, colnames(train.set) != "invasion.status"],
#       y = train.set$invasion.status,
#       plot = "box",
#       strip = strip.custom(par.strip.text = list(cex = .7)),
#       scales = list(
#             x = list(relation = "free"),
#             y = list(relation = "free")
#       )
# )

# class weights
model_weights <- ifelse(train.set$invasion.status == "no",
      (1 / table(train.set$invasion.status)[1]) * 0.5,
      (1 / table(train.set$invasion.status)[2]) * 0.5
)

model_weights

# save to .Rdata
save(invasion_list03,
     invasion_list04,
     train.index,
     train.set,
     test.set,
     model_weights,
     file = "./Results20240220/20240220.X001.ml_invasion.data.Rdata"
)

ls()

# ---------------------------------------------------------------------------- #
#                         user-defined summary function                        #
# ---------------------------------------------------------------------------- #
# https://stats.stackexchange.com/questions/357401/getting-sensitivity-and-specificity-from-a-caret-model
#
# gmeanSummary <- function(data,
#                        lev = NULL,
#                        model = NULL) {

# # confusion matrix
# sens <- sensitivity(data$pred, data$obs)
# spec <- specificity(data$pred, data$obs)

# # gmean
#       out <- sqrt(sens * spec)
#       names(out) <- "gmean"
#       out
# }

## Do 10 repeats of 5-Fold CV for the data. We will fit
## a ranger model that evaluates 12 values of k and set the seed
## at each iteration.
set.seed(2024)
seeds <- vector(mode = "list", length = 51)

for(i in 1:50){seeds[[i]] <- sample.int(1000, 490)}

## For the last model:
set.seed(2024)
seeds[[51]] <- sample.int(1000, 1)
seeds

# Define the training control
set.seed(2024)

# fitControl <- trainControl(
#     method = "repeatedcv", # k-fold cross validation
#     number = 5, # number of folds
#     index = createFolds(train.set$invasion.status, 5),
#     repeats = 10,
#     savePredictions = "final", # saves predictions for optimal tuning parameter
#     allowParallel = TRUE,
#     classProbs = TRUE, # should class probabilities be returned
#     seeds = seeds, # seed at each iteration
#     summaryFunction = gmeanSummary # results summary function
# )

# Define the training control
fitControl <- trainControl(
      method = "repeatedcv", # k-fold cross validation
      number = 5, # number of folds
      index = createFolds(train.set$invasion.status, 5),
      repeats = 10,
      savePredictions = "final", # saves predictions for optimal tuning parameter
      allowParallel = TRUE,
      classProbs = TRUE, # should class probabilities be returned
      seeds = seeds, # seed at each iteration
      summaryFunction = twoClassSummary # results summary function
)

# ---------------------------------------------------------------------------- #
#                                      weight option                           #
# ---------------------------------------------------------------------------- #
# rowname: weights
# model.rpart: yes
# model.kknn: no
# model.nb: no
# model.earth: yes
# model.fda: yes
# model.gbm: no
# model.AdaBoost.M1: no
# model.gam: no
# model.nnet: yes
# model.svm: no
# model.glmStepAIC: no
# model.ranger: yes

# # ---------------------------------------------------------------------------- #
# #                                      glm                                     #
# # ---------------------------------------------------------------------------- #
# set.seed(2024)

# model.glm <- train(invasion.status ~ .,
#       data = train.set,
#       method = "glm",
#       family = binomial(link = "cloglog"),
#       # tuneLength = 10,
#       metric = "ROC",
#       # weights = model_weights,
#       trControl = fitControl
# )

# model.glm
# model.glm$finalModel

# ---------------------------------------------------------------------------- #
#                                      glmStepAIC                              #
# ---------------------------------------------------------------------------- #
set.seed(2024)
model.glmStepAIC <- train(invasion.status ~ .,
      data = train.set,
      method = "glmStepAIC",
      family = binomial(link = "cloglog"),
      # tuneLength = 10,
      metric = "ROC",
      # weights = model_weights,
      trControl = fitControl
)

model.glmStepAIC
model.glmStepAIC$finalModel

# ---------------------------------------------------------------------------- #
#                                      gam                                     #
# ---------------------------------------------------------------------------- #
set.seed(2024)

# # Define the training control
# fitControl.gam <- trainControl(
#       method = "repeatedcv", # k-fold cross validation
#       number = 2, # number of folds
#       index = createFolds(train.set$invasion.status, 2),
#       repeats = 10,
#       savePredictions = "final", # saves predictions for optimal tuning parameter
#       allowParallel = TRUE,
#       classProbs = TRUE, # should class probabilities be returned
#       summaryFunction = gmeanSummary # results summary function
# )

model.gam <- train(invasion.status ~ .,
      data = train.set,
      method = "gam",
      family = binomial(link = "cloglog"),
      tuneLength = 10,
      metric = "ROC",
      # weights = model_weights,
      trControl = fitControl
)

model.gam
model.gam$finalModel

# ---------------------------------------------------------------------------- #
#                                      gbm                                     #
# ---------------------------------------------------------------------------- #

set.seed(2024)
model.gbm <- train(invasion.status ~ .,
      data = train.set,
      method = "gbm",
      # preProcess = "zv",
      tuneLength = 10,
      metric = "ROC",
      # weights = model_weights,
      trControl = fitControl
)

# 模型
model.gbm
model.gbm$finalModel

# ---------------------------------------------------------------------------- #
#                                      rpart                                   #
# ---------------------------------------------------------------------------- #
set.seed(2024)

model.rpart <- train(invasion.status ~ .,
      data = train.set,
      method = "rpart",
      tuneLength = 10,
      metric = "ROC",
      weights = model_weights,
      trControl = fitControl
)

model.rpart
model.rpart$finalModel

# ---------------------------------------------------------------------------- #
#                                      nnet                                   #
# ---------------------------------------------------------------------------- #
set.seed(2024)

model.nnet <- train(invasion.status ~ .,
      data = train.set,
      method = "nnet",
      tuneLength = 10,
      metric = "ROC",
      weights = model_weights,
      trControl = fitControl
)

model.nnet
model.nnet$finalModel

# ---------------------------------------------------------------------------- #
#                                      fda                                     #
# ---------------------------------------------------------------------------- #
library(earth)
set.seed(2024)

model.fda <- train(invasion.status ~ .,
      data = train.set,
      method = "fda",
      tuneLength = 10,
      metric = "ROC",
      weights = model_weights,
      trControl = fitControl
)

model.fda
model.fda$finalModel

# # ---------------------------------------------------------------------------- #
# #                                      earth                                   #
# # ---------------------------------------------------------------------------- #
# set.seed(2024)

# model.earth <- train(invasion.status ~ .,
#       data = train.set,
#       method = "earth",
#       family = binomial,
#       tuneLength = 10,
#       metric = "ROC",
#       weights = model_weights,
#       trControl = fitControl
# )

# model.earth
# model.earth$finalModel

# ---------------------------------------------------------------------------- #
#                                      ranger                                  #
# ---------------------------------------------------------------------------- #
set.seed(2024)

model.ranger <- train(invasion.status ~ .,
      data = train.set,
      method = "ranger",
      tuneLength = 10,
      metric = "ROC",
      weights = model_weights,
      importance = "permutation",
      trControl = fitControl
)

#
model.ranger
model.ranger$finalModel

# ---------------------------------------------------------------------------- #
#                            ranger unweighted                                 #
# ---------------------------------------------------------------------------- #
set.seed(2024)

model.ranger.unweighted <- train(invasion.status ~ .,
      data = train.set,
      method = "ranger",
      tuneLength = 10,
      metric = "ROC",
      # weights = model_weights,
      importance = "permutation",
      trControl = fitControl
)

#
model.ranger.unweighted
model.ranger.unweighted$finalModel

# ---------------------------------------------------------------------------- #
#                                      svm                                     #
# ---------------------------------------------------------------------------- #
set.seed(2024)

# # Define the training control
# fitControl.svm <- trainControl(
#       method = "repeatedcv", # k-fold cross validation
#       number = 3, # number of folds
#       index = createFolds(train.set$invasion.status, 3),
#       repeats = 10,
#       savePredictions = "final", # saves predictions for optimal tuning parameter
#       allowParallel = TRUE,
#       classProbs = TRUE, # should class probabilities be returned
#       summaryFunction = gmeanSummary # results summary function
# )

model.svm <- train(invasion.status ~ .,
      data = train.set,
      method = "svmRadial",
      tuneLength = 10,
      metric = "ROC",
      # weights = model_weights,
      trControl = fitControl
)

# 模型
model.svm
model.svm$finalModel

# ---------------------------------------------------------------------------- #
#                                      kknn                                    #
# ---------------------------------------------------------------------------- #
set.seed(2024)

model.kknn <- train(invasion.status ~ .,
      data = train.set,
      method = "kknn",
      tuneLength = 10,
      metric = "ROC",
      # weights = model_weights,
      trControl = fitControl
)

# 模型
model.kknn
model.kknn$finalModel

# ---------------------------------------------------------------------------- #
#                                      nb                                      #
# ---------------------------------------------------------------------------- #
set.seed(2024)

model.nb <- train(invasion.status ~ .,
      data = train.set,
      method = "naive_bayes",
      tuneLength = 10,
      metric = "ROC",
      # weights = model_weights,
      trControl = fitControl
)

# 模型
model.nb
model.nb$finalModel

# ---------------------------------------------------------------------------- #
#                                      AdaBoost.M1                             #
# ---------------------------------------------------------------------------- #
set.seed(2024)

# configure multicore
library(doParallel)

ncores <- detectCores()
cl <- makePSOCKcluster(ncores - 2)
registerDoParallel(cl)

model.AdaBoost.M1 <- train(invasion.status ~ .,
      data = train.set,
      method = "AdaBoost.M1",
      tuneLength = 10,
      metric = "ROC",
      # weights = model_weights,
      trControl = fitControl
)

## When you are done:
stopCluster(cl)

# # Error in summary.connection(connection) : 链结不对
# # https://www.coder.work/article/7744303
unregister_dopar <- function() {
      env <- foreach:::.foreachGlobals
      rm(list = ls(name = env), pos = env)
}

unregister_dopar()

#
model.AdaBoost.M1
model.AdaBoost.M1$finalModel

# ---------------------------------------------------------------------------- #
#                               model comparison                               #
# ---------------------------------------------------------------------------- #

model.list <- list(
      model.glmStepAIC = model.glmStepAIC,
      model.gam = model.gam,
      model.gbm = model.gbm,
      model.rpart = model.rpart,
      model.nnet = model.nnet,
      model.fda = model.fda,
      # model.earth = model.earth,
      model.ranger = model.ranger,
      model.ranger.unweighted = model.ranger.unweighted,
      model.svm = model.svm,
      model.kknn = model.kknn,
      model.nb = model.nb,
      model.AdaBoost.M1 = model.AdaBoost.M1
)

names(model.list)

save(model.list,
      file = "./Results20240220/20240220.X002.model.list.Rdata"
)


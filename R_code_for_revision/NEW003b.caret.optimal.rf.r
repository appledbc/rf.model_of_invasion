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

# set your own working directory
file_path <- "G:/我的坚果云/2022_06.生物入侵_随机森林"

setwd(file_path)
getwd()


# save to .Rdata
load(file = "./Results20240220/20240220.X000.raw_invasion.data.Rdata")
load(file = "./Results20240220/20240220.X001.ml_invasion.data.Rdata")
# load(file = "./Results20240220/20240220.X002.model.list.Rdata")
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

# Step 3: Predict on test.set
predicted.ranger.test <- predict(model.ranger, test.set)

confusionMatrix(
    reference = test.set$invasion.status,
    data = predicted.ranger.test,
    mode = "everything",
    positive = "yes"
)

# # ---------------------------------------------------------------------------- #
# #                            ranger unweighted                                 #
# # ---------------------------------------------------------------------------- #
# set.seed(2024)

# model.ranger.unweighted <- train(invasion.status ~ .,
#       data = train.set,
#       method = "ranger",
#       tuneLength = 10,
#       metric = "ROC",
#       # weights = model_weights,
#       importance = "permutation",
#       trControl = fitControl
# )

# #
# model.ranger.unweighted
# model.ranger.unweighted$finalModel

# # Step 3: Predict on test.set
# predicted.ranger.unweighted.test <- predict(model.ranger.unweighted, test.set)

# confusionMatrix(
#     reference = test.set$invasion.status,
#     data = predicted.ranger.unweighted.test,
#     mode = "everything",
#     positive = "yes"
# )

# ==========================================================================
# plot confusion matrix
# ==========================================================================
# https://cran.r-project.org/web/packages/cvms/vignettes/Creating_a_confusion_matrix.html
# data
library(cvms)

# prediction for prob
pred.raw <- predict(model.ranger, test.set)
pred.raw

conf_mat.cvms <- confusion_matrix(targets = test.set$invasion.status,
                                  predictions = pred.raw)

conf_mat.cvms

# plot
conf.mat.plot <- plot_confusion_matrix(conf_mat.cvms$"Confusion Matrix"[[1]],
                      add_normalized = FALSE,
                      add_row_percentages = FALSE) +
theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(family = "serif", colour = "black", size = 14),
      axis.title = element_text(family = "serif", colour = "black", size = 14),
      axis.ticks = element_line(colour = "black", linetype = "solid", size = 1)
      ) +
scale_x_discrete(labels = c("yes" = "Invasive", "no" = "Naturalized"), position = "top") +
scale_y_discrete(labels = c("yes" = "Invasive", "no" = "Naturalized")) +
      # scale_color_igv() +
labs(x = "Observed", y = "Predicted")

conf.mat.plot

# ==========================================================================
# ROC plot
# ==========================================================================
# calculating roc
library(pROC)

# prediction for prob
pred.prob <- predict(model.ranger, test.set, type = "prob")[, "yes"]
pred.prob

# calculating auc
roc.data <- pROC::roc(
      response = test.set$invasion.status,
      predictor = pred.prob,
      levels = levels(test.set$invasion.status),
      na.rm = TRUE, ci = TRUE, ci.alpha = 0.95)

# roc
roc.plot <- ggroc(roc.data, linewidth = 1, color = "red", legacy.axes = TRUE) +
geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),, color = "grey", linewidth = 1, linetype = 6) +
theme_bw() +
theme(
      panel.background = element_rect(fill = "NA"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(family = "serif", colour = "black", size = 14),
      legend.text = element_text(family = "serif", colour = "black", size = 14),
      axis.line = element_line(size = 1, linetype = "solid"),
      axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
      axis.text = element_text(family = "serif", colour = "black", size = 14),
      axis.title.x.top = element_blank(),
      axis.title.y.right = element_blank(),
      axis.ticks.x.top = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.text.x.top = element_blank(),
      axis.text.y.right = element_blank(),
      axis.title = element_text(family = "serif", colour = "black", size = 14)
      ) +
scale_x_continuous(sec.axis = dup_axis(), expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
scale_y_continuous(sec.axis = dup_axis(), expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
labs(y = "Sensitivity", x = "1 - Specificity")

roc.plot

# ==========================================================================
# performance metric table
# ==========================================================================
# # https://rpubs.com/StatsGary/ConfusionTableR
# using ConfusionTableR to extract Confusion matrix
library(ConfusionTableR)

confusion.tableR <- ConfusionTableR::binary_class_cm(
            train_labels = pred.raw,
            truth_labels = test.set$invasion.status,
            positive = "yes" )

confusion.tableR

# https://www.r-bloggers.com/2021/05/mmtable2-ggplot2-for-tables/
library(ggpp)

record_level_cm <- confusion.tableR$record_level_cm %>%
                  data.frame() %>%
                  mutate(G_mean = sqrt(Sensitivity * Specificity)) %>%
                  mutate(AUC = roc.data$auc)

performance.table <- record_level_cm %>%
                     select(Accuracy, Sensitivity, Specificity, F1, G_mean, AUC) %>%
                     mutate_all(~round(., 3)) %>%
                     data.frame()

colnames(performance.table)[5] <- "G-mean"

performance.table.df <- tibble(x = 0, y = 0, tb = list(performance.table))


gg.table <- ggplot() +
theme_void() +
geom_table(data = performance.table.df, aes(x = x, y = y, label = tb),
           table.theme = ttheme_gtbw(base_family = "serif"))

gg.table

# gg.tab <- ggtexttable(performance.table02,
#                       rows = NULL,
#                       theme = ttheme(
#                       colnames.style = colnames_style(fill = "white", face = "plain"),
#                       tbody.style = tbody_style(fill = "white")))

# gg.tab02 <- gg.tab %>%
#  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2)

layout <- "
AB
CC
"

conf.mat.plots <- conf.mat.plot +
roc.plot +
gg.table +
plot_layout(widths = c(1, 1), heights = c(4, 1), design = layout) +
plot_annotation(tag_levels = "A")

conf.mat.plots

# export fig.2
ggexport(conf.mat.plots,
      filename = "./Results20240220/20240220.conf.mat.plots.png",
      width =  2000,
      height = 1250,
      pointsize = 12,
      res = 300
)



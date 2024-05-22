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
load(file = "./Results20240220/20240220.X002.model.list.Rdata")
ls()

# prediction for raw
pred.raw.list <- map(
      names(model.list),
      ~ predict(model.list[[.x]], test.set)
)

names(pred.raw.list) <- names(model.list)
pred.raw.list

# confusion matrix
confusion.mat.list <- map(
      names(model.list),
      ~ confusionMatrix(
            data = pred.raw.list[[.x]],
            reference = test.set$invasion.status,
            positive = "yes"
      )
)

names(confusion.mat.list) <- names(model.list)

confusion.mat.list

# 使用ConfusionTableR提取Confusion matrix
library(ConfusionTableR)

# https://rpubs.com/StatsGary/ConfusionTableR

confusion.tableR.list <- map(
      names(model.list),
      ~ ConfusionTableR::binary_class_cm(
            train_labels = pred.raw.list[[.x]],
            truth_labels = test.set$invasion.status,
            positive = "yes"
      )
)

confusion.tableR.list
names(confusion.tableR.list) <- names(model.list)

# 清理Confusion matrix
confusion.tableR.cm.list <- map(
      names(model.list),
      ~ glimpse(confusion.tableR.list[[.x]]$record_level_cm)
)

names(confusion.tableR.cm.list) <- names(model.list)

library(rlist)
confusion.tableR.cm.df <- rlist::list.rbind(confusion.tableR.cm.list) %>%
      tibble::rownames_to_column()

View(confusion.tableR.cm.df)

# 计算roc
library(pROC)

# prediction for prob
pred.prob.list <- map(
      names(model.list),
      ~ predict(model.list[[.x]], test.set, type = "prob")[, "yes"]
)

names(pred.prob.list) <- names(model.list)

pred.prob.list

roc.list <- map(names(model.list), ~ pROC::roc(
      response = test.set$invasion.status,
      predictor = pred.prob.list[[.x]],
      levels = levels(test.set$invasion.status)
))

names(roc.list) <- names(model.list)

auc.data <- roc.list %>%
      list.select(auc) %>%
      list.rbind() %>%
      as.data.frame() %>%
      tibble::rownames_to_column()

auc.data$auc <- as.numeric(auc.data$auc)

# output aucs
confusion.tableR.cm.df01 <- confusion.tableR.cm.df %>%
      left_join(auc.data, by = "rowname")

# output gmean
confusion.tableR.cm.df02 <- confusion.tableR.cm.df01 %>%
      rowwise() %>%
      mutate(gmean = sqrt(Sensitivity * Specificity)) %>%
      ungroup()

str(confusion.tableR.cm.df02)
View(confusion.tableR.cm.df02)

# 保存数据
write.csv(confusion.tableR.cm.df02,
      file = "./Results20240220/20240220.X003.confusion.tableR.cm.df.csv",
      row.names = FALSE
)

save(confusion.tableR.cm.df02,
      file = "./Results20240220/20240220.X003.confusion.tableR.cm.df.Rdata"
)

# prediction for prob
combn.model.names <- data.frame(t(combn(names(model.list), 2)))

combn.model.names$combination <- as.character(paste(combn.model.names[, 1],
      combn.model.names[, 2],
      sep = "_x_"
))

roc.test.list <- map2(
      combn.model.names[, 1], combn.model.names[, 2],
      ~ roc.test(roc.list[[.x]], roc.list[[.y]])
)

library(rlist)
names(roc.test.list) <- combn.model.names$combination

library(stringr)
# select.names <- combn.model.names$combination[grepl("ranger", combn.model.names$combination)]
# roc.test.list[c(select.names)]


roc.test.tidy.list <- map(
      combn.model.names$combination,
      ~ broom::tidy(roc.test.list[[.x]])
)

names(roc.test.tidy.list) <- combn.model.names$combination

roc.test.tidy.list01 <- roc.test.tidy.list %>%
      list.rbind() %>%
      tibble::rownames_to_column() %>%
      data.frame()

View(roc.test.tidy.list01)

# roc.test.tidy.list0.ranger <- roc.test.tidy.list01 %>%
# filter(grepl("ranger", combn.model.names$combination))

# save data
write.csv(roc.test.tidy.list01, "./Results20240220/20240220.X004.roc.test.tidy.list01.csv", row.names = FALSE)

save(roc.test.tidy.list01,
      file = "./Results20240220/20240220.X004.roc.test.tidy.list01.Rdata"
)

# heatmaps
str(roc.test.tidy.list01)

roc.test.tidy.list02 <- roc.test.tidy.list01 %>%
left_join(combn.model.names, by = c("rowname" = "combination")) %>%
mutate_if(is.numeric, ~ round(., 4)) %>%
mutate(X1 = factor(X1, levels = unique(X1)),
       X2 = factor(X2, levels = unique(X2)),
       p.value02 = case_when(p.value > 0.05 | p.value == 0.05 ~ ">= 0.05",
                             p.value < 0.001 ~ "< 0.001",
                             p.value < 0.01 ~ "< 0.01",
                             p.value < 0.05 ~ "< 0.05"))

View(roc.test.tidy.list02)

# model matrices
library(stringr)

model.fullName <- c(model.glmStepAIC = "GLM with StepAIC",
                    model.gam = "GAM",
                    model.gbm = "GBM",
                    model.rpart = "CART",
                    model.nnet = "Neural Networks",
                    model.fda = "FDA",
                    # model.earth =  "MARS",
                    model.ranger = "Random Forest weighted",
                    model.ranger.unweighted = "Random Forest unweighted",
                    model.svm = "SVM",
                    model.kknn =  "k-Nearest Neighbors",
                    model.nb =  "Naive Bayes",
                    model.AdaBoost.M1 =  "AdaBoost.M1")

# Create a roc.heatmap
roc.heatmap <- ggplot(roc.test.tidy.list02, aes(x = X1, y = X2, color = p.value02, size = 1- p.value))+
  geom_point() +
  theme_minimal()+ # minimal theme
  theme(
      panel.background = element_rect(fill = NA),
      legend.position = c(0.8, 0.2),
      axis.line = element_line(size = 1, linetype = "solid"),
      axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
      axis.text = element_text(family = "serif", colour = "black", size = 14),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14),
      axis.title = element_text(family = "serif", colour = "black", size = 14)
      ) +
guides(size = "none") +
scale_colour_manual(values = c("brown3", "steelblue", "darkgreen", "orange"))+
scale_x_discrete(labels = model.fullName) +
scale_y_discrete(labels = model.fullName) +
labs(color = expression(paste(italic("p"), " values for Delong' test")), y = "", x = "")

roc.heatmap

# export fig.2
ggexport(roc.heatmap,
      filename = "./Results20240220/20240220.roc.heatmap.png",
      width = 2500,
      height = 2500,
      pointsize = 12,
      res = 300
)

# roc plot
roc.plot.list <- map(
      names(model.list),
      ~ ggroc(roc.list[[.x]])
)


names(roc.plot.list) <- names(model.list)

str(roc.plot.list$model.glmStepAIC)

# https://community.rstudio.com/t/extract-nested-list-from-all-elements-levels/60733
roc.plot.list01 <- map(roc.plot.list, ~ .[["data"]]) %>%
      list.rbind() %>%
      data.frame() %>%
      tibble::rownames_to_column()

roc.plot.list01 <- roc.plot.list01 %>%
      mutate_at(vars(rowname), ~ str_replace_all(., "[.][0-9]+$", ""))

roc.plot.list02 <- roc.plot.list01 %>%
      mutate(X1_sensitivity = 1 - sensitivity)

head(roc.plot.list02)

# # https://stackoverflow.com/questions/43403282/add-row-in-each-group-using-dplyr-and-add-row
# roc.plot.list03 <- roc.plot.list02 %>%
# group_by(rowname) %>%
# do(add_row(., .before = 0)) %>%
# do(tidyr::fill(., rowname, .direction = "up")) %>%
# dplyr::ungroup()

# roc.plot.list03 <- roc.plot.list03 %>%
# mutate_at(vars(specificity, X1_sensitivity), ~case_when(is.na(.) ~ 0,
# !is.na(.) ~.)) %>%
# mutate_at(vars(sensitivity), ~case_when(is.na(.) ~ 1,
# !is.na(.) ~.)) %>%
# mutate_at(vars(threshold), ~case_when(is.na(.) ~ -Inf,
# !is.na(.) ~.))

roc.plot.list03 <- roc.plot.list02 %>%
      filter(!c(specificity != 0 & X1_sensitivity == 0)) %>%
      group_by(rowname) %>%
      distinct(X1_sensitivity, .keep_all = TRUE) %>%
      dplyr::ungroup()

# Multiple Lines for roc
library(ggsci)
library(ggpubr)

# auc ranking
model.fullName <- c(model.glmStepAIC = "GLM with StepAIC",
                    model.gam = "GAM",
                    model.gbm = "GBM",
                    model.rpart = "CART",
                    model.nnet = "Neural Networks",
                    model.fda = "FDA",
                    # model.earth =  "MARS",
                    model.ranger = "Random Forest weighted",
                    model.ranger.unweighted = "Random Forest unweighted",
                    model.svm = "SVM",
                    model.kknn =  "k-Nearest Neighbors",
                    model.nb =  "Naive Bayes",
                    model.AdaBoost.M1 =  "AdaBoost.M1")

confusion.tableR.cm.df03 <- confusion.tableR.cm.df02 %>%
      distinct(rowname, auc) %>%
      mutate(model.fullName = model.fullName) %>%
      arrange(desc(auc)) %>%
      mutate(auc.label = sprintf("%s: %0.3f", model.fullName, auc)) %>%
      mutate_at(vars(auc.label), ~ factor(., levels = .))

roc.plot.list04 <- roc.plot.list03 %>% left_join(confusion.tableR.cm.df03, by = "rowname")
View(roc.plot.list04)

# roc plots using ggplot
roc.plot <- ggplot() +
      geom_line(data = roc.plot.list04, aes(x = X1_sensitivity, y = specificity, color = auc.label), linewidth = 1) +
      geom_line(aes(x = c(0, 1), y = c(0, 1)), color = "grey", linewidth = 1, linetype = 6) +
      theme_bw() +
      theme(
            panel.background = element_rect(fill = "NA"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_text(family = "serif", colour = "black", size = 14),
            legend.text = element_text(family = "serif", colour = "black", size = 14),
            legend.position = c(0.7, 0.3),
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
      scale_colour_manual(
            values = pal_igv("default", alpha = 1)(12),
            name = "Models"
      ) +
      # scale_color_igv() +
      labs(x = "1 - Sensitivity", y = "Specificity")

(roc.plot)

# export fig.2
ggexport(roc.plot,
      filename = "./Results20240220/20240220.roc.plot.png",
      width = 2500,
      height = 2500,
      pointsize = 12,
      res = 300
)

# ==========================================================================
# plot without random forest unweighted
# ==========================================================================

# roc plot without rf 20240220.roc.plot
roc.plot.list04a <- roc.plot.list04 %>% filter(rowname != "model.ranger.unweighted")


roc.plot02 <- ggplot() +
      geom_line(data = roc.plot.list04a, aes(x = X1_sensitivity, y = specificity, color = auc.label), linewidth = 1) +
      geom_line(aes(x = c(0, 1), y = c(0, 1)), color = "grey", linewidth = 1, linetype = 6) +
      theme_bw() +
      theme(
            panel.background = element_rect(fill = "NA"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_text(family = "serif", colour = "black", size = 14),
            legend.text = element_text(family = "serif", colour = "black", size = 14),
            legend.position = c(0.7, 0.3),
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
      scale_colour_manual(
            values = pal_igv("default", alpha = 1)(12),
            name = "Models"
      ) +
      # scale_color_igv() +
      labs(x = "1 - Sensitivity", y = "Specificity")

(roc.plot02)

# export fig.2
ggexport(roc.plot02,
      filename = "./Results20240220/20240220.roc.plot02.png",
      width = 2500,
      height = 2500,
      pointsize = 12,
      res = 300
)
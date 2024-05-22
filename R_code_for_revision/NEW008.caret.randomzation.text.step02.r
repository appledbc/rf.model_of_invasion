# Author:
# Date: Tue Jan 10 2023
# Last Modified by:
# Last Modified time: Tue Jan 10 2023
# Email:
# Description: randomization test for proportion of variable importance.

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
# load(file = "./Results20240220/20240220.x002.model.list.Rdata")
load(file = "./Results20240220/20240220.x006.vi.plot.Rdata")
load(file = "./Results20240220/20240220.x005.fi.ranger_combins.sample.Rdata")
ls()

# ==========================================================================
# observed variable importance
# ==========================================================================
summary.fi.ranger.group01

fi.ranger.proportion_observed <- summary.fi.ranger.group01 %>%
select(var01, proportion) %>%
mutate(proportion = proportion) %>%
rename(group.variable = var01,
       proportion.observed = proportion)

# ==========================================================================
# data of variable importance from randomly sampling
# ==========================================================================
fi.ranger_combins.sample.df <- rbindlist(fi.ranger_combins.sample)
glimpse(fi.ranger_combins.sample.df)

head(fi.ranger_combins.sample.df)

# extracting the grouped variable name
fi.ranger_combins.sample.df01 <- fi.ranger_combins.sample.df %>%
mutate(group.variable =  gsub("(.*)_x_(.*)" , "\\1", variable),
       seed = gsub("(.*)_x_(.*)" , "\\2", variable)) %>%
data.frame()

str(fi.ranger_combins.sample.df01)
table(fi.ranger_combins.sample.df01$group.variable)

# averaging 1000-time permuted loss value of grouped variables
# this is not for sampling distribution, this is only for calculation of drop_loss value
fi.ranger_combins.sample.df02 <- fi.ranger_combins.sample.df01 %>%
group_by(group.variable, seed) %>%
summarise(dropout_loss.mean = mean(dropout_loss, na.rm = TRUE)) %>%
ungroup()

# calculating the mean dropout loss value of full model in the random forest model
# full model means the model without permutation
# calculating the mean dropout loss value of baseline in the random forest model
# baseline means the variables of the model will be all permuted.
baseline <- fi.ranger_combins.sample.df02 %>% filter(group.variable == "_baseline_") %>% pull(dropout_loss.mean)
fullmodel <- fi.ranger_combins.sample.df02 %>% filter(group.variable == "_full_model_") %>% pull(dropout_loss.mean)

baseline
fullmodel

# calculating the proportion of relative importance
fi.ranger_combins.sample.df03 <- fi.ranger_combins.sample.df02 %>%
mutate(proportion = (dropout_loss.mean - fullmodel) / (baseline - fullmodel) * 100)

View(fi.ranger_combins.sample.df03)

# remove the values of "_baseline_", "_full_model_"
fi.ranger_combins.sample.df04 <- fi.ranger_combins.sample.df03 %>%
filter(!(group.variable %in% c("_baseline_", "_full_model_"))) %>%
left_join(fi.ranger.proportion_observed, by = "group.variable")

str(fi.ranger_combins.sample.df04)

# summary of feature importances
n.sampling <- 1000

summary.fi.ranger.group.sample <- fi.ranger_combins.sample.df04 %>%
group_by(group.variable) %>%
dplyr::summarise(n = n(),
                 mean = mean(dropout_loss.mean, na.rm = TRUE),
                 sd = sd(dropout_loss.mean),
                 se = sd/sqrt(n),
                 min = min(dropout_loss.mean, na.rm = TRUE),
                 q1 = quantile(dropout_loss.mean, 0.25),
                 median = median(dropout_loss.mean, na.rm = TRUE),
                 q3 = quantile(dropout_loss.mean, 0.75),
                 max  = max(dropout_loss.mean, na.rm = TRUE),
                 proportion.observed = unique(proportion.observed),
                 p.value.higher = sum(unique(proportion.observed) > proportion)/n.sampling,
                 p.value.lower = sum(unique(proportion.observed) <= proportion)/n.sampling
                 ) %>%
ungroup()


# calculating the p value
summary.fi.ranger.group.sample <- summary.fi.ranger.group.sample %>%
mutate(p.signif = case_when(p.value.lower >= 0.025 & p.value.higher <= 0.975 ~ "ns",
                         p.value.lower < 0.0005 | p.value.higher > 0.9995 ~ "***",
                         p.value.lower < 0.005 | p.value.higher > 0.995 ~ "**",
                         p.value.lower < 0.025 | p.value.higher > 0.975 ~ "*")) %>%
mutate(color = case_when(p.value.lower >= 0.025 & p.value.higher <= 0.975 ~ "blue",
                            p.value.lower < 0.0005 | p.value.higher > 0.9995 ~ "red",
                            p.value.lower < 0.005 | p.value.higher > 0.995 ~ "red",
                            p.value.lower < 0.025 | p.value.higher > 0.975 ~ "red"))

View(summary.fi.ranger.group.sample)

# adding color value into the data.frame
summary.fi.ranger.group.sample02 <- summary.fi.ranger.group.sample %>%
select(group.variable, color)

fi.ranger_combins.sample.df05 <- fi.ranger_combins.sample.df04 %>%
left_join(summary.fi.ranger.group.sample02, by = "group.variable")

str(fi.ranger_combins.sample.df05)


# loading packages
library(ggthemes)

# define a user-defined function for plotting histogram
plot_histogram <- function(group.variable, data) {

# 提取数据
data.filtered <- data %>% filter(group.variable == {{group.variable}})
proportion.observed <- data.filtered %>% pull(proportion.observed) %>% unique()
color <- data.filtered %>% pull(color) %>% unique()

  ggplot(data = data.filtered, aes(x = proportion)) +
    geom_histogram(color = "black", fill = "grey", bins = 50) +
    geom_vline(xintercept = proportion.observed, color = color, linewidth = 1) +
    theme_classic2() +
    theme(
      legend.position = "none",
      axis.line = element_line(size = 1, linetype = "solid"),
      axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
      axis.text = element_text(family = "serif", colour = "black", size = 14),
      axis.title = element_text(family = "serif", colour = "black", size = 14)) +
    labs(x = "Relative importance (%)", y = "Frequency", title = group.variable)

}

# using map function to plotting the histogram
group.variable <- unique(fi.ranger_combins.sample.df05$group.variable)

plot_histogram.list <- map(group.variable, ~plot_histogram(group.variable = .x, data = fi.ranger_combins.sample.df05))
names(plot_histogram.list) <- group.variable

# checking the plots
library(gridExtra)
do.call("grid.arrange", c(plot_histogram.list, ncol = 2))

# # rename the names of plots
# plot_histogram.list$Phylogenetic.relatedness <- plot_histogram.list$'Phylogenetic relatedness'
# plot_histogram.list$Species.traits <- plot_histogram.list$'Species traits'

# change x-axis values
# https://github.com/tidyverse/ggplot2/issues/2887
plot_histogram.list$Environmental.niches <- plot_histogram.list$Environmental.niches + scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 20), oob = function(x, limits) x) + scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, 20)) + ggtitle("Environmental niches")
plot_histogram.list$Propagule.pressure <- plot_histogram.list$Propagule.pressure + scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 20), oob = function(x, limits) x) + scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, 20)) + ggtitle("Propagule pressure")
plot_histogram.list$Species.traits <- plot_histogram.list$Species.traits + scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 20), oob = function(x, limits) x) + scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, 20)) + ggtitle("Species traits")
# plot_histogram.list$Phylogenetic.relatedness <- plot_histogram.list$Phylogenetic.relatedness + scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2), oob = function(x, limits) x) + scale_y_continuous(expand = c(0, 0), limits = c(0, 200), breaks = seq(0, 200, 50)) + ggtitle("Phylogenetic relatedness")
# plot_histogram.list$Introduction.history <- plot_histogram.list$Introduction.history + scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.5), oob = function(x, limits) x) + scale_y_continuous(expand = c(0, 0), limits = c(0, 300), breaks = seq(0, 300, 50)) + ggtitle("Introduction history")

# scale_x_continuous(expand = c(0, 0), limits = c(0, 60), breaks = seq(0, 60, 10)) +
# scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, 20)) +

# combining the individual plots
library(patchwork)
plot_histogram.list02 <- plot_histogram.list$Environmental.niches +
                         plot_histogram.list$Species.traits +
                         plot_histogram.list$Propagule.pressure +
                         # plot_histogram.list$Phylogenetic.relatedness +
                         # plot_histogram.list$Introduction.history +
                         plot_layout(ncol = 1) +
                         plot_annotation(tag_levels = "A")

plot_histogram.list02

# storing the plots
ggexport(plot_histogram.list02, filename = "./Results20240220/20240220.plot_histogram.list02.png",
         width = 1200,
         height = 2700,
         pointsize = 12,
         res = 300)

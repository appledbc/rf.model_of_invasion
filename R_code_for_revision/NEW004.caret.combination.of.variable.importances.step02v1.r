# Author:
# Date: Tue Jan 10 2023
# Last Modified by:
# Last Modified time: Tue Jan 10 2023
# Email:
# Description: relative importance plot

# cleaning memory
cat("\014")
rm(list = ls())
gc()

# loading packages
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
file_path <- "D:/我的坚果云/2022_06.生物入侵_随机森林"

setwd(file_path)
getwd()

# save to .Rdata
load(file = "./Results20240220/20240220.X000.raw_invasion.data.Rdata")
load(file = "./Results20240220/20240220.X001.ml_invasion.data.Rdata")
# load(file = "./Results20240220/20240220.x002.model.list.Rdata")
load(file = "./Results20240220/20240220.x005.fi.ranger_combins.Rdata")

ls()

#
str(fi.ranger_combins)

# combination of variables
feature.df <- data.frame(
    variable = c(
# propagule pressure，
              "boga.scaled",
              "prov.scaled",
              "eco.use_AF",
              "eco.use_EU",
              "eco.use_FU",
              "eco.use_GS",
              "eco.use_HF",
              "eco.use_IF",
              "eco.use_MA",
              "eco.use_ME",
              "eco.use_PO",
              "eco.use_SU",
              "eco.use_num.scaled",
# introduction history,
              # "res.time.scaled",
# climatic suitability,
              "clim.suit.scaled",
              "tdwg.X1",
              "tdwg.X2",
              "tdwg.X3",
              "tdwg.X4",
              "tdwg.X5",
              "tdwg.X6",
              "tdwg.X7",
              "tdwg.X8",
              "tdwg.X9",
              "tdwg.level03.num.scaled",
# species traits,
              "lf.short_herb",
              "lf.long_herb",
              "lf.woody",
              "prop.seed",
              "prop.veg",
              "prop.both",
              "max.height.scaled",
# phylogenetic relatedness,
              "pd.mean.scaled",
              "pd.min.scaled",
              "pd.wmean.scaled"),
    variable.fullName = c(
# propagule pressure，
              "No. of botanical gardens",
              "No. of provinces",
              "EU: animal food",
              "EU: environmental uses",
              "EU: fuels",
              "EU: gene sources",
              "EU: human food",
              "EU: invertebrate food",
              "EU: materials",
              "EU: medicine",
              "EU: poisons",
              "EU: social uses",
              "No. of economic uses (EU)",
# introduction history,
              # "Min. residence time",
# climatic suitability,
              "Climatic suitability",
              "NR: Europe",
              "NR: Africa",
              "NR: Asia-Temp.",
              "NR: Asia-Trop.",
              "NR: Australasia",
              "NR: Pacific Isl.",
              "NR: North. America",
              "NR: South. America",
              "NR: Antarctic",
              "Native range (NR) size",
# species traits,
              "Life form: short-lived",
              "Life form: long-lived",
              "Life form: woody",
              "Prop. mode: seed",
              "Prop. mode: vegetative",
              "Prop. mode: both",
              "Max. height",
# phylogenetic relatedness,
              "MPD",
              "NNPD",
              "Weighted MPD"
    ),
    variable.group = c(
# propagule pressure，
              "Propagule.pressure",
              "Propagule.pressure",
              "Propagule.pressure",
              "Propagule.pressure",
              "Propagule.pressure",
              "Propagule.pressure",
              "Propagule.pressure",
              "Propagule.pressure",
              "Propagule.pressure",
              "Propagule.pressure",
              "Propagule.pressure",
              "Propagule.pressure",
              "Propagule.pressure",
# introduction history,
              # "Introduction.history",
# climatic suitability,
              "Environmental.niches",
              "Environmental.niches",
              "Environmental.niches",
              "Environmental.niches",
              "Environmental.niches",
              "Environmental.niches",
              "Environmental.niches",
              "Environmental.niches",
              "Environmental.niches",
              "Environmental.niches",
              "Environmental.niches",
# species traits,
              "Species.traits",
              "Species.traits",
              "Species.traits",
              "Species.traits",
              "Species.traits",
              "Species.traits",
              "Species.traits",
# phylogenetic relatedness,
              "Species.traits",
              "Species.traits",
              "Species.traits"
    ),
    variable.group.fullName = c(
# propagule pressure,
              "Propagule pressure",
              "Propagule pressure",
              "Propagule pressure",
              "Propagule pressure",
              "Propagule pressure",
              "Propagule pressure",
              "Propagule pressure",
              "Propagule pressure",
              "Propagule pressure",
              "Propagule pressure",
              "Propagule pressure",
              "Propagule pressure",
              "Propagule pressure",
# introduction history,
              # "Introduction history",
# climatic suitability,
              "Environmental niches",
              "Environmental niches",
              "Environmental niches",
              "Environmental niches",
              "Environmental niches",
              "Environmental niches",
              "Environmental niches",
              "Environmental niches",
              "Environmental niches",
              "Environmental niches",
              "Environmental niches",
# species traits,
              "Species traits",
              "Species traits",
              "Species traits",
              "Species traits",
              "Species traits",
              "Species traits",
              "Species traits",
# phylogenetic relatedness,
              "Species traits",
              "Species traits",
              "Species traits"
    )
)

(feature.df)

# as factor value
feature.var.df <- feature.df %>%
select(variable, variable.fullName) %>%
mutate_at(vars(variable), ~factor(., levels = c(
# propagule pressure,
              "boga.scaled",
              "prov.scaled",
              "eco.use_AF",
              "eco.use_EU",
              "eco.use_FU",
              "eco.use_GS",
              "eco.use_HF",
              "eco.use_IF",
              "eco.use_MA",
              "eco.use_ME",
              "eco.use_PO",
              "eco.use_SU",
              "eco.use_num.scaled",
# introduction history,
              # "res.time.scaled",
# climatic suitability,
              "clim.suit.scaled",
              "tdwg.X1",
              "tdwg.X2",
              "tdwg.X3",
              "tdwg.X4",
              "tdwg.X5",
              "tdwg.X6",
              "tdwg.X7",
              "tdwg.X8",
              "tdwg.X9",
              "tdwg.level03.num.scaled",
# species traits,
              "lf.short_herb",
              "lf.long_herb",
              "lf.woody",
              "prop.seed",
              "prop.veg",
              "prop.both",
              "max.height.scaled",
# phylogenetic relatedness,
              "pd.mean.scaled",
              "pd.min.scaled",
              "pd.wmean.scaled"))) %>%
arrange(variable)

levels(feature.var.df$variable)

# ranking the grouped variable
feature.group.df <- feature.df %>%
distinct(variable.group, variable.group.fullName) %>%
mutate_at(vars(variable.group), ~factor(., levels = c(
# propagule pressure,
              "Propagule.pressure",
# climatic suitability,
              "Environmental.niches",
# species traits,
              "Species.traits"
              ))) %>% arrange(variable.group)

levels(feature.group.df$variable.group)

# keep data of fi.ranger_combins with permutation != 0
fi.ranger.df <- fi.ranger_combins %>%
rlist::list.rbind() %>%
data.frame() %>%
mutate_at(vars(variable), ~ as.factor(.))

levels(fi.ranger.df$variable)

# calculating the mean dropout loss value of full model in the random forest model
# full model means the model without permutation
# calculating the mean dropout loss value of baseline in the random forest model
# baseline means the variables of the model will be all permuted.
baseline <- mean(fi.ranger.df$dropout_loss[which(fi.ranger.df$variable == "_baseline_")])
fullmodel <- mean(fi.ranger.df$dropout_loss[which(fi.ranger.df$variable == "_full_model_")])

baseline
fullmodel

# summary of feature importances
summary.fi.ranger <- fi.ranger.df %>%
group_by(variable) %>%
dplyr::summarise(n = n(),
                 mean = mean(dropout_loss, na.rm = TRUE),
                 sd = sd(dropout_loss),
                 se = sd/sqrt(n),
                 min = min(dropout_loss, na.rm = TRUE),
                 q1 = quantile(dropout_loss, 0.25),
                 median = median(dropout_loss, na.rm = TRUE),
                 q3 = quantile(dropout_loss, 0.75),
                 max  = max(dropout_loss, na.rm = TRUE),
                 p.value.lower = sum(dropout_loss <= fullmodel)/n,
                 p.value.higher = sum(dropout_loss > fullmodel)/n
                 ) %>%
ungroup()

View(summary.fi.ranger)

# four decimal places
summary.fi.ranger <- summary.fi.ranger %>%
mutate_at(vars(mean:p.value.higher), ~round(., 4))

View(summary.fi.ranger)

# calculating the p value
summary.fi.ranger01 <- summary.fi.ranger %>%
mutate(p.signif = case_when(p.value.lower == 1 & p.value.higher == 0 ~ ">= 0.05",
                            p.value.lower >= 0.025 & p.value.higher <= 0.975 ~ ">= 0.05",
                            p.value.lower < 0.0005 | p.value.higher > 0.9995 ~ "< 0.001",
                            p.value.lower < 0.005 | p.value.higher > 0.995 ~ "< 0.01",
                            p.value.lower < 0.025 | p.value.higher > 0.975 ~ "< 0.05")) %>%
mutate(p.label = case_when(p.value.lower == 1 & p.value.higher == 0 ~ "ns",
                            p.value.lower >= 0.025 & p.value.higher <= 0.975 ~ "ns",
                            p.value.lower < 0.0005 | p.value.higher > 0.9995 ~ "***",
                            p.value.lower < 0.005 | p.value.higher > 0.995 ~ "**",
                            p.value.lower < 0.025 | p.value.higher > 0.975 ~ "*"))
View(summary.fi.ranger01)
str(summary.fi.ranger01$variable)

# ==========================================================================
# extracting the variable importance
# ==========================================================================
# calculating relative importance for single variables
(combins.var01)

summary.fi.ranger.var00 <- combins.var01 %>%
left_join(summary.fi.ranger01, by = c("combin.name" = "variable")) %>%
data.frame() %>%
mutate(proportion00 = (mean - fullmodel)/(baseline - fullmodel) * 100) %>%
# mutate(proportion = ifelse(proportion00 < 0, 0, proportion00)) %>%
mutate(proportion = proportion00) %>%
arrange(mean) %>%
mutate_at(vars(var01), ~factor(., levels = feature.var.df$variable)) %>%
arrange(var01)

levels(summary.fi.ranger.var00$var01)

# adding plot feature for single variables
summary.fi.ranger.var01 <- summary.fi.ranger.var00 %>%
left_join(feature.df, by  = c("var01" = "variable")) %>%
mutate_at(vars(variable.group), ~factor(., levels = c(
# propagule pressure,
              "Propagule.pressure",
# species traits,
              "Species.traits",
# climatic suitability,
              "Environmental.niches"
              ))) %>%
mutate(color = case_when(variable.group == "Environmental.niches" ~ "#FD7013",
                         variable.group == "Species.traits" ~ "#F6003C",
                         variable.group == "Propagule.pressure" ~ "#1A76C0",
                         ))

summary.fi.ranger.var01 <- summary.fi.ranger.var01 %>%
arrange(variable.group, mean) %>%
mutate_at(vars(var01), ~factor(., levels = var01))

levels(summary.fi.ranger.var01$var01)

summary.fi.ranger.var01 %>% group_by(variable.group.fullName) %>% summarise(proprotion01 = sum(proportion))

# sum(summary.fi.ranger.var01$proportion)
# summary.fi.ranger.var01 %>% arrange(mean)
# summary.fi.ranger.var01 %>% group_by(variable.group) %>% summarise(additive = sum(proportion))

# calculating relative importance for grouped variables
(combins.group01)

summary.fi.ranger.group00 <- combins.group01 %>%
left_join(summary.fi.ranger01, by = c("combin.name" = "variable")) %>%
data.frame() %>%
mutate(proportion00 = (mean - fullmodel)/(baseline - fullmodel) * 100) %>%
# mutate(proportion = ifelse(proportion00 < 0, 0, proportion00)) %>%
mutate(proportion = proportion00) %>%
arrange(mean) %>%
mutate_at(vars(var01), ~factor(., levels = feature.group.df$variable.group)) %>%
arrange(var01)

levels(summary.fi.ranger.group00$var01)

# adding plot feature for grouped variables
feature.df.group <- feature.df %>% distinct(variable.group, variable.group.fullName)

summary.fi.ranger.group01 <- summary.fi.ranger.group00 %>%
left_join(feature.df.group, by  = c("var01" = "variable.group")) %>%
mutate(color = case_when(var01 == "Environmental.niches" ~ "#FD7013",
                         var01 == "Species.traits" ~ "#F6003C",
                         var01 == "Propagule.pressure" ~ "#1A76C0"
                         ))

summary.fi.ranger.group01 <- summary.fi.ranger.group01 %>%
arrange(mean) %>%
mutate_at(vars(var01), ~factor(., levels = var01))

levels(summary.fi.ranger.group01$var01)

View(summary.fi.ranger.group01)


sum(summary.fi.ranger.group01$proportion)
summary.fi.ranger.group01 %>% arrange(mean)

# ==========================================================================
# plotting for feature importance
# ==========================================================================
# dropout values of single variables
plot.fi.ranger.var01 <- ggplot(
                         data = summary.fi.ranger.var01,
                         aes(x = mean, y = var01)
                         ) +
geom_vline(aes(xintercept = fullmodel), linetype = 5, col = "grey") +
geom_boxplot(
             aes(xmin = min, xlower = q1, xmiddle = median, xupper = q3, xmax = max),
             stat = "identity",
             # fill = summary.fi.ranger.var01$color,
             color = summary.fi.ranger.var01$color,
             width = 0.4
             ) +
geom_point(alpha = 0.8) +
theme_classic() +
theme(
      panel.background = element_rect(fill = NA),
      legend.position = "none",
      axis.line = element_line(size = 1, linetype = "solid"),
      axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
      axis.text = element_text(family = "serif", colour = "black", size = 14),
      axis.title = element_text(family = "serif", colour = "black", size = 14)
      ) +
labs(
     y = "",
     x = "Loss function: 1 - AUC"
     ) +
scale_x_continuous(
                   limits = c(0, 0.015),
                   breaks = seq(0, 0.015, by = 0.005)
                   ) +
scale_y_discrete(labels = summary.fi.ranger.var01$variable.fullName) +
annotate("text", hjust = 0, family = "serif",
         x = summary.fi.ranger.var01$max + 0.0015,
         y = 1:nrow(summary.fi.ranger.var01),
         label = summary.fi.ranger.var01$p.label)

plot.fi.ranger.var01

# relative importance of single variables
plot.prop.fi.ranger.var01 <- ggplot(
                              data = summary.fi.ranger.var01,
                              aes(x = proportion, y = var01)
                              ) +
geom_bar(
         stat = "identity",
         fill = summary.fi.ranger.var01$color
         ) +
theme_classic() +
theme(
      panel.background = element_rect(fill = NA),
      legend.position = "none",
      axis.line = element_line(size = 1, linetype = "solid"),
      axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
      axis.text = element_text(family = "serif", colour = "black", size = 14),
      axis.title = element_text(family = "serif", colour = "black", size = 14)
      ) +
labs(y = "", x = expression("Relative importance (%)")) +
scale_x_continuous(
                   limits = c(-0.05, 1.5),
                   breaks = seq(0, 1.5, by = 0.5)
                   ) +
scale_y_discrete(labels = summary.fi.ranger.var01$variable.fullName)

plot.prop.fi.ranger.var01

# dropout values of grouped variables
plot.fi.ranger.group01 <- ggplot(
                         data = summary.fi.ranger.group01,
                         aes(x = mean, y = var01)
                         ) +
geom_vline(aes(xintercept = fullmodel), linetype = 5, col = "grey") +
geom_boxplot(
             aes(xmin = min, xlower = q1, xmiddle = median, xupper = q3, xmax = max),
             stat = "identity",
             # fill = summary.fi.ranger.group01$color,
             color = summary.fi.ranger.group01$color,
             width = 0.4
             ) +
geom_point(alpha = 0.8) +
theme_classic() +
theme(
      panel.background = element_rect(fill = NA),
      legend.position = "none",
      axis.line = element_line(size = 1, linetype = "solid"),
      axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
      axis.text = element_text(family = "serif", colour = "black", size = 14),
      axis.title = element_text(family = "serif", colour = "black", size = 14)
      ) +
labs(
     y = "",
     x = "Loss function: 1 - AUC"
     ) +
scale_x_continuous(
                   limits = c(0, 0.40),
                   breaks = seq(0, 0.40, by = 0.1)
                   ) +
scale_y_discrete(labels = summary.fi.ranger.group01$variable.group.fullName) +
annotate("text", hjust = 0, family = "serif",
         x = summary.fi.ranger.group01$max + 0.02,
         y = 1:nrow(summary.fi.ranger.group01),
         label = summary.fi.ranger.group01$p.label)


plot.fi.ranger.group01

# relative importance of grouped variables
plot.prop.fi.ranger.group01 <- ggplot(
                              data = summary.fi.ranger.group01,
                              aes(x = proportion, y = var01)
                              ) +
geom_bar(
         stat = "identity",
         fill = summary.fi.ranger.group01$color
         ) +
theme_classic() +
theme(
      panel.background = element_rect(fill = NA),
      legend.position = "none",
      axis.line = element_line(size = 1, linetype = "solid"),
      axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
      axis.text = element_text(family = "serif", colour = "black", size = 14),
      axis.title = element_text(family = "serif", colour = "black", size = 14)
      ) +
labs(y = "", x = expression("Relative importance (%)")) +
scale_x_continuous(
                   limits = c(0, 40),
                   breaks = seq(0, 40, by = 10)
                   ) +
scale_y_discrete(labels = summary.fi.ranger.group01$variable.group.fullName)

plot.prop.fi.ranger.group01

# storing as .Rdata
save(
feature.var.df,
feature.group.df,
summary.fi.ranger,
baseline,
summary.fi.ranger.var01,
summary.fi.ranger.group01,
plot.fi.ranger.var01,
plot.prop.fi.ranger.var01,
plot.fi.ranger.group01,
plot.prop.fi.ranger.group01,
file = "./Results20240220/20240220.x006.vi.plot.Rdata")

# combing the individual plots
library(patchwork)
library(ggpubr)

plots.fi.ranger <- plot.fi.ranger.var01 +
                    plot.prop.fi.ranger.var01 +
                    plot.fi.ranger.group01 +
                    plot.prop.fi.ranger.group01 +
                    plot_layout(ncol = 2) +
                    plot_annotation(tag_levels = 'A')

plots.fi.ranger

ggexport(plots.fi.ranger, filename = "./Results20240220/20240220.plots.fi.ranger.png",
         width = 3600,
         height = 3600,
         pointsize = 12,
         res = 300)

# plots.raw
ggexport(plot.fi.ranger.var01, filename = "./Results20240220/20240220.plot.fi.ranger.var01.png",
         width = 1800,
         height = 2400,
         pointsize = 12,
         res = 300)

ggexport(plot.fi.ranger.group01, filename = "./Results20240220/20240220.plot.fi.ranger.group01.png",
         width = 1800,
         height = 1800,
         pointsize = 12,
         res = 300)

# plots.prop
ggexport(plot.prop.fi.ranger.var01, filename = "./Results20240220/20240220.plot.prop.fi.ranger.var01.png",
         width = 1800,
         height = 2400,
         pointsize = 12,
         res = 300)

ggexport(plot.prop.fi.ranger.group01, filename = "./Results20240220/20240220.plot.prop.fi.ranger.group01.png",
         width = 1800,
         height = 1800,
         pointsize = 12,
         res = 300)

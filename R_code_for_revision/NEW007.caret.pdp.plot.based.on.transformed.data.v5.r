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
library(DALEX)
library(ingredients)

# set your own working directory
file_path <- "G:/我的坚果云/2022_06.生物入侵_随机森林"

setwd(file_path)
getwd()

# save to .Rdata
load(file = "./Results20240220/20240220.X000.raw_invasion.data.Rdata")
load(file = "./Results20240220/20240220.X001.ml_invasion.data.Rdata")
# load(file = "./Results20240220/20240220.x002.model.list.Rdata")
load(file = "./Results20240220/20240220.x005.fi.ranger_combins.Rdata")

ls()

# ??partial_dependence
(explainer.ranger)

# # 修改No. of Economic use为No. of economic use
# feature.df$variable.fullName[which(feature.df$variable.fullName == "No. of Economic use")] <- "No. of economic use"
# feature.df$variable.fullName[which(feature.df$variable.fullName == "Climatic suitbaility")] <- "Climatic suitability"
# feature.df$variable.fullName[which(feature.df$variable.fullName == "Life form: short-lived")] <- "Life form: short-lived herb"
# feature.df$variable.fullName[which(feature.df$variable.fullName == "Life form: long-lived")] <- "Life form: long-lived herb"
# feature.df$variable.fullName[which(feature.df$variable.fullName == "Life form: Woody")] <- "Life form: woody"

# as.factor
feature.var.df <- feature.df %>%
select(variable, variable.fullName) %>%
mutate_at(vars(variable), ~factor(., levels = c(
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
# mrt,
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

# calculating pdp value

set.seed(2024)
mp.ranger <- DALEX::model_profile(explainer = explainer.ranger, N = NULL)
str(mp.ranger)

cp.ranger.df <- data.frame(mp.ranger$cp_profiles)
str(cp.ranger.df)

# summary.pdp.list <- list()

# for(i in 1: nrow(feature.var.df)){

# variable <- as.character(feature.var.df$variable[i])
# print(variable)

# summary.pdp.list[[i]] <- cp.ranger.df %>%
# filter(X_vname_ == variable) %>%
# dplyr::group_by(.[variable]) %>%
# rstatix::get_summary_stats(X_yhat_) %>%
# ungroup()

# names(summary.pdp.list[i]) <- variable

# }

# (summary.pdp.list)

# user defined function
summary.pdp <- function(data, vars){

vars <- as.character(vars)
print(vars)

sub.data <- data %>%
filter(X_vname_ == vars) %>%
filter(!is.na(.[vars]))

out <- sub.data %>%
filter(X_vname_ == vars) %>%
dplyr::group_by(.[vars]) %>%
rstatix::get_summary_stats(X_yhat_) %>%
mutate(target = vars) %>%
rename(x = colnames(.)[1],
       y = colnames(.)[2]) %>%
ungroup()

return(out)
}


# summarize cp.ranger.df values
head(cp.ranger.df)

feature.var.df.scaled <- data.frame(
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
# mrt,
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
# mrt,
              # "Min. residence time (year)",
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
              "Native range (NR) size\n(no. TDWG level-3 regions)",
# species traits,
              "Life form: short-lived herb",
              "Life form: long-lived herb",
              "Life form: woody",
              "Prop. mode: seed",
              "Prop. mode: vegetative",
              "Prop. mode: both",
              "Max. height (m)",
# phylogenetic relatedness,
              "MPD (Mya)",
              "NNPD (Mya)",
              "Weighted MPD (Mya)"
    ))


summary.pdp.list <- map(feature.var.df.scaled$variable, ~ summary.pdp(data = cp.ranger.df,
                                                               vars = .x))


names(summary.pdp.list) <- feature.var.df.scaled$variable

summary.pdp.df <- summary.pdp.list %>%
rlist::list.rbind() %>%
data.frame()

str(summary.pdp.df)
View(summary.pdp.df)

# numeric values

pdp.numeric.plot <- function(data, features, vars){

sub.data <- data %>% filter(target == vars)

x.title <- features %>%
filter(variable == vars) %>%
.["variable.fullName"] %>%
 as.character()

print(vars)

plot <- ggplot(
      data = sub.data,
      aes(x = x, y = mean)
) +
geom_line(linewidth = 1) +
geom_ribbon(aes(ymin = mean - ci, ymax = mean + ci), alpha = 0.15) +
geom_hline(aes(yintercept = 0.5), linetype = 5) +
      theme_classic() +
      theme(
            panel.background = element_rect(fill = NA),
            legend.position = "none",
            axis.line = element_line(linewidth = 1, linetype = "solid", ),
            axis.ticks = element_line(colour = "black", linetype = "solid", linewidth = 1),
            axis.text = element_text(family = "serif", colour = "black", size = 16),
            axis.title = element_text(family = "serif", colour = "black", size = 16)
      ) +
      scale_y_continuous(
            limits = c(0.3, 0.6),
            breaks = seq(0.3, 0.6, by = 0.1)
      ) +
      labs(
            x = x.title,
            y = "Invasion success"
      )
}

feature.numeric <- rev(c("boga.scaled",
                         "prov.scaled",
                         "eco.use_num.scaled",
                         # "res.time.scaled",
                         "clim.suit.scaled",
                         "tdwg.level03.num.scaled",
                         "max.height.scaled",
                         "pd.mean.scaled",
                         "pd.min.scaled",
                         "pd.wmean.scaled"
                         ))




plot.pdp.numeric.list <- map(feature.numeric, ~ pdp.numeric.plot(data = summary.pdp.df,
                                                                  features = feature.var.df.scaled,
                                                                  vars = .x))


names(plot.pdp.numeric.list) <- feature.numeric

# stack of plots
library(gridExtra)
do.call("grid.arrange", c(plot.pdp.numeric.list, ncol = 3))


# factor values

pdp.factor.plot <- function(data, features, vars){

sub.data <- data %>%
filter(target == vars & x %in% c(0, 1)) %>%
mutate_at(vars(x), ~ case_when(. == 0 ~ "no",
                               . == 1 ~ "yes"))

sub.data$x <- as.factor(sub.data$x)

x.title <- features %>%
filter(variable == vars) %>%
.["variable.fullName"] %>%
as.character()

print(vars)

plot <- ggplot(
      data = sub.data,
      aes(x = x, y = mean)) +
# geom_bar(stat = "identity") +
geom_point(stat = "identity", size = 2) +
geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1) +
geom_hline(aes(yintercept = 0.5), linetype = 5) +
      theme_classic() +
      theme(
            panel.background = element_rect(fill = NA),
            legend.position = "none",
            axis.line = element_line(linewidth = 1, linetype = "solid"),
            axis.ticks = element_line(colour = "black", linetype = "solid", linewidth = 1),
            axis.text = element_text(family = "serif", colour = "black", size = 16),
            axis.title = element_text(family = "serif", colour = "black", size = 16)
      ) +
      scale_y_continuous(
            limits = c(0.3, 0.6),
            breaks = seq(0.3, 0.6, by = 0.1)
      ) +
      labs(
            x = x.title,
            y = "Invasion success"
      )

}

feature.factor <- rev(c("eco.use_AF",
                        "eco.use_EU",
                        "eco.use_FU",
                        "eco.use_GS",
                        "eco.use_HF",
                        "eco.use_IF",
                        "eco.use_MA",
                        "eco.use_ME",
                        "eco.use_PO",
                        "eco.use_SU",
                        "tdwg.X1",
                        "tdwg.X2",
                        "tdwg.X3",
                        "tdwg.X4",
                        "tdwg.X5",
                        "tdwg.X6",
                        "tdwg.X7",
                        "tdwg.X8",
                        "tdwg.X9",
                        "lf.short_herb",
                        "lf.long_herb",
                        "lf.woody",
                        "prop.seed",
                        "prop.veg",
                        "prop.both"
                        ))

plot.pdp.factor.list <- map(feature.factor, ~ pdp.factor.plot(data = summary.pdp.df,
                                                                    features = feature.var.df.scaled,
                                                                    vars = .x))

names(plot.pdp.factor.list) <- feature.factor

# stack of plots
library(gridExtra)
do.call("grid.arrange", c(plot.pdp.factor.list, ncol = 3))


# colors
# "Propagule.pressure" ~ "#1A76C0"
# "Introduction.history" ~ "#C659A2"
# "Environmental.niches" ~ "#FFE131"
# "Species traits" ~ "#F6003C"
# "Phylogenetic relatedness" ~ "#936229"


# "Propagule.pressure" ~ "#1A76C0"
# scale_fill_manual does not work for plot list
# so repeatedly add layers
plot.pdp.numeric.list$boga.scaled <- plot.pdp.numeric.list$boga.scaled + geom_line(colour = "#1A76C0", linewidth = 1) + geom_ribbon(aes(ymin = mean - ci, ymax = mean + ci), fill = "#1A76C0", alpha = 0.15)
plot.pdp.numeric.list$prov.scaled <- plot.pdp.numeric.list$prov.scaled + geom_line(colour = "#1A76C0", linewidth = 1) + geom_ribbon(aes(ymin = mean - ci, ymax = mean + ci), fill = "#1A76C0", alpha = 0.15)
plot.pdp.numeric.list$eco.use_num.scaled <- plot.pdp.numeric.list$eco.use_num.scaled + geom_line(colour = "#1A76C0", linewidth = 1) + geom_ribbon(aes(ymin = mean - ci, ymax = mean + ci), fill = "#1A76C0", alpha = 0.15)
plot.pdp.factor.list$eco.use_AF <- plot.pdp.factor.list$eco.use_AF + geom_point(fill = "#1A76C0", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#1A76C0")
plot.pdp.factor.list$eco.use_EU <- plot.pdp.factor.list$eco.use_EU + geom_point(fill = "#1A76C0", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#1A76C0")
plot.pdp.factor.list$eco.use_FU <- plot.pdp.factor.list$eco.use_FU + geom_point(fill = "#1A76C0", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#1A76C0")
plot.pdp.factor.list$eco.use_GS <- plot.pdp.factor.list$eco.use_GS + geom_point(fill = "#1A76C0", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#1A76C0")
plot.pdp.factor.list$eco.use_HF <- plot.pdp.factor.list$eco.use_HF + geom_point(fill = "#1A76C0", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#1A76C0")
plot.pdp.factor.list$eco.use_IF <- plot.pdp.factor.list$eco.use_IF + geom_point(fill = "#1A76C0", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#1A76C0")
plot.pdp.factor.list$eco.use_MA <- plot.pdp.factor.list$eco.use_MA + geom_point(fill = "#1A76C0", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#1A76C0")
plot.pdp.factor.list$eco.use_ME <- plot.pdp.factor.list$eco.use_ME + geom_point(fill = "#1A76C0", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#1A76C0")
plot.pdp.factor.list$eco.use_PO <- plot.pdp.factor.list$eco.use_PO + geom_point(fill = "#1A76C0", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#1A76C0")
plot.pdp.factor.list$eco.use_SU <- plot.pdp.factor.list$eco.use_SU + geom_point(fill = "#1A76C0", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#1A76C0")

# # "Introduction.history" ~ "#C659A2"
# plot.pdp.numeric.list$res.time.scaled <- plot.pdp.numeric.list$res.time.scaled + geom_line(colour = "#C659A2", linewidth = 1) + geom_ribbon(aes(ymin = mean - ci, ymax = mean + ci), fill = "#C659A2", alpha = 0.15)

# "Environmental.niches" ~ "#FFE131"
plot.pdp.numeric.list$clim.suit.scaled <- plot.pdp.numeric.list$clim.suit.scaled + geom_line(colour = "#FD7013", linewidth = 1) + geom_ribbon(aes(ymin = mean - ci, ymax = mean + ci), fill = "#FD7013", alpha = 0.15)
plot.pdp.numeric.list$tdwg.level03.num.scaled <- plot.pdp.numeric.list$tdwg.level03.num.scaled + geom_line(colour = "#FD7013", linewidth = 1) + geom_ribbon(aes(ymin = mean - ci, ymax = mean + ci), fill = "#FD7013", alpha = 0.15)
plot.pdp.factor.list$tdwg.X1 <- plot.pdp.factor.list$tdwg.X1 + geom_point(fill = "#FD7013", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#FD7013")
plot.pdp.factor.list$tdwg.X2 <- plot.pdp.factor.list$tdwg.X2 + geom_point(fill = "#FD7013", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#FD7013")
plot.pdp.factor.list$tdwg.X3 <- plot.pdp.factor.list$tdwg.X3 + geom_point(fill = "#FD7013", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#FD7013")
plot.pdp.factor.list$tdwg.X4 <- plot.pdp.factor.list$tdwg.X4 + geom_point(fill = "#FD7013", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#FD7013")
plot.pdp.factor.list$tdwg.X5 <- plot.pdp.factor.list$tdwg.X5 + geom_point(fill = "#FD7013", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#FD7013")
plot.pdp.factor.list$tdwg.X6 <- plot.pdp.factor.list$tdwg.X6 + geom_point(fill = "#FD7013", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#FD7013")
plot.pdp.factor.list$tdwg.X7 <- plot.pdp.factor.list$tdwg.X7 + geom_point(fill = "#FD7013", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#FD7013")
plot.pdp.factor.list$tdwg.X8 <- plot.pdp.factor.list$tdwg.X8 + geom_point(fill = "#FD7013", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#FD7013")
plot.pdp.factor.list$tdwg.X9 <- plot.pdp.factor.list$tdwg.X9 + geom_point(fill = "#FD7013", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#FD7013")

# "Species traits" ~ "#F6003C"
plot.pdp.numeric.list$max.height.scaled <- plot.pdp.numeric.list$max.height.scaled + geom_line(colour = "#F6003C", linewidth = 1) + geom_ribbon(aes(ymin = mean - ci, ymax = mean + ci), fill = "#F6003C", alpha = 0.15)
plot.pdp.factor.list$lf.short_herb <- plot.pdp.factor.list$lf.short_herb + geom_point(fill = "#F6003C", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#F6003C")
plot.pdp.factor.list$lf.long_herb <- plot.pdp.factor.list$lf.long_herb + geom_point(fill = "#F6003C", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#F6003C")
plot.pdp.factor.list$lf.woody <- plot.pdp.factor.list$lf.woody + geom_point(fill = "#F6003C", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#F6003C")
plot.pdp.factor.list$prop.seed <- plot.pdp.factor.list$prop.seed + geom_point(fill = "#F6003C", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#F6003C")
plot.pdp.factor.list$prop.veg <- plot.pdp.factor.list$prop.veg + geom_point(fill = "#F6003C", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#F6003C")
plot.pdp.factor.list$prop.both <- plot.pdp.factor.list$prop.both + geom_point(fill = "#F6003C", stat = "identity") + geom_pointrange(aes(ymin = mean - ci, ymax = mean + ci), linewidth = 1, color = "#F6003C")

# "Phylogenetic relatedness" ~ "#936229"
plot.pdp.numeric.list$pd.mean.scaled <- plot.pdp.numeric.list$pd.mean.scaled + geom_line(colour = "#F6003C", linewidth = 1) + geom_ribbon(aes(ymin = mean - ci, ymax = mean + ci), fill = "#F6003C", alpha = 0.15)
plot.pdp.numeric.list$pd.min.scaled <- plot.pdp.numeric.list$pd.min.scaled + geom_line(colour = "#F6003C", linewidth = 1) + geom_ribbon(aes(ymin = mean - ci, ymax = mean + ci), fill = "#F6003C", alpha = 0.15)
plot.pdp.numeric.list$pd.wmean.scaled <- plot.pdp.numeric.list$pd.wmean.scaled + geom_line(colour = "#F6003C", linewidth = 1) + geom_ribbon(aes(ymin = mean - ci, ymax = mean + ci), fill = "#F6003C", alpha = 0.15)

#
plot.pdp.numeric.list$clim.suit.scaled <- plot.pdp.numeric.list$clim.suit.scaled
plot.pdp.numeric.list$tdwg.level03.num.scaled <- plot.pdp.numeric.list$tdwg.level03.num.scaled + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$tdwg.X1 <- plot.pdp.factor.list$tdwg.X1 + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$tdwg.X2 <- plot.pdp.factor.list$tdwg.X2 + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$tdwg.X3 <- plot.pdp.factor.list$tdwg.X3 + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$tdwg.X4 <- plot.pdp.factor.list$tdwg.X4 + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$tdwg.X5 <- plot.pdp.factor.list$tdwg.X5 + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
#
plot.pdp.factor.list$tdwg.X6 <- plot.pdp.factor.list$tdwg.X6
plot.pdp.factor.list$tdwg.X7 <- plot.pdp.factor.list$tdwg.X7 + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$tdwg.X8 <- plot.pdp.factor.list$tdwg.X8 + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$tdwg.X9 <- plot.pdp.factor.list$tdwg.X9 + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.numeric.list$boga.scaled <- plot.pdp.numeric.list$boga.scaled + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.numeric.list$prov.scaled <- plot.pdp.numeric.list$prov.scaled + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.numeric.list$eco.use_num.scaled <- plot.pdp.numeric.list$eco.use_num.scaled + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
#
plot.pdp.factor.list$eco.use_AF <- plot.pdp.factor.list$eco.use_AF
plot.pdp.factor.list$eco.use_EU <- plot.pdp.factor.list$eco.use_EU + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$eco.use_FU <- plot.pdp.factor.list$eco.use_FU + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$eco.use_GS <- plot.pdp.factor.list$eco.use_GS + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$eco.use_HF <- plot.pdp.factor.list$eco.use_HF + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$eco.use_IF <- plot.pdp.factor.list$eco.use_IF + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$eco.use_MA <- plot.pdp.factor.list$eco.use_MA + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
#
plot.pdp.factor.list$eco.use_ME <- plot.pdp.factor.list$eco.use_ME
plot.pdp.factor.list$eco.use_PO <- plot.pdp.factor.list$eco.use_PO + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$eco.use_SU <- plot.pdp.factor.list$eco.use_SU + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.numeric.list$max.height.scaled <- plot.pdp.numeric.list$max.height.scaled + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$lf.short_herb <- plot.pdp.factor.list$lf.short_herb + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$lf.long_herb <- plot.pdp.factor.list$lf.long_herb + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$lf.woody <- plot.pdp.factor.list$lf.woody + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
#
plot.pdp.factor.list$prop.seed <- plot.pdp.factor.list$prop.seed
plot.pdp.factor.list$prop.veg <- plot.pdp.factor.list$prop.veg + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.factor.list$prop.both <- plot.pdp.factor.list$prop.both + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.numeric.list$pd.mean.scaled <- plot.pdp.numeric.list$pd.mean.scaled + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.numeric.list$pd.min.scaled <- plot.pdp.numeric.list$pd.min.scaled + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot.pdp.numeric.list$pd.wmean.scaled <- plot.pdp.numeric.list$pd.wmean.scaled + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
# plot.pdp.numeric.list$res.time.scaled <- plot.pdp.numeric.list$res.time.scaled + theme(axis.title.y = element_blank(), axis.text.y = element_blank())


plot.pdp.numeric.list$clim.suit.scaled <- plot.pdp.numeric.list$clim.suit.scaled + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) +  annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.numeric.list$tdwg.level03.num.scaled <- plot.pdp.numeric.list$tdwg.level03.num.scaled + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05))+  annotate("text", x = -3, y = Inf, size = 7, family = "serif", label = "***", hjust = 0.1, vjust = 1)
plot.pdp.factor.list$tdwg.X1 <- plot.pdp.factor.list$tdwg.X1 + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$tdwg.X2 <- plot.pdp.factor.list$tdwg.X2 + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = " * ", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$tdwg.X3 <- plot.pdp.factor.list$tdwg.X3 + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$tdwg.X4 <- plot.pdp.factor.list$tdwg.X4 + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = " * ", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$tdwg.X5 <- plot.pdp.factor.list$tdwg.X5 + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "ns", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$tdwg.X6 <- plot.pdp.factor.list$tdwg.X6 + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "ns", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$tdwg.X7 <- plot.pdp.factor.list$tdwg.X7 + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "**", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$tdwg.X8 <- plot.pdp.factor.list$tdwg.X8 + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$tdwg.X9 <- plot.pdp.factor.list$tdwg.X9 + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "ns", hjust = -0.5, vjust = 1)
plot.pdp.numeric.list$boga.scaled <- plot.pdp.numeric.list$boga.scaled + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.numeric.list$prov.scaled <- plot.pdp.numeric.list$prov.scaled + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.numeric.list$eco.use_num.scaled <- plot.pdp.numeric.list$eco.use_num.scaled + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -1.2, y = Inf, size = 7, family = "serif", label = "***", hjust = 1, vjust = 1)
plot.pdp.factor.list$eco.use_AF <- plot.pdp.factor.list$eco.use_AF + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$eco.use_EU <- plot.pdp.factor.list$eco.use_EU + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$eco.use_FU <- plot.pdp.factor.list$eco.use_FU + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "**", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$eco.use_GS <- plot.pdp.factor.list$eco.use_GS + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$eco.use_HF <- plot.pdp.factor.list$eco.use_HF + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$eco.use_IF <- plot.pdp.factor.list$eco.use_IF + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "ns", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$eco.use_MA <- plot.pdp.factor.list$eco.use_MA + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$eco.use_ME <- plot.pdp.factor.list$eco.use_ME + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$eco.use_PO <- plot.pdp.factor.list$eco.use_PO + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$eco.use_SU <- plot.pdp.factor.list$eco.use_SU + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "ns", hjust = -0.5, vjust = 1)
plot.pdp.numeric.list$max.height.scaled <- plot.pdp.numeric.list$max.height.scaled + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$lf.short_herb <- plot.pdp.factor.list$lf.short_herb + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$lf.long_herb <- plot.pdp.factor.list$lf.long_herb + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$lf.woody <- plot.pdp.factor.list$lf.woody + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$prop.seed <- plot.pdp.factor.list$prop.seed + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "ns", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$prop.veg <- plot.pdp.factor.list$prop.veg + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.factor.list$prop.both <- plot.pdp.factor.list$prop.both + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.numeric.list$pd.mean.scaled <- plot.pdp.numeric.list$pd.mean.scaled + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.numeric.list$pd.min.scaled <- plot.pdp.numeric.list$pd.min.scaled + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)
plot.pdp.numeric.list$pd.wmean.scaled <- plot.pdp.numeric.list$pd.wmean.scaled + scale_y_continuous(limits = c(0.38, 0.55), breaks = seq(0.4, 0.55, by = 0.05)) + annotate("text", x = -Inf, y = Inf, size = 7, family = "serif", label = "***", hjust = -0.5, vjust = 1)



# plots
# set labels as AA、AB、AC、AD... and so on
tags <- c(LETTERS, paste0("A", LETTERS))

plot.pdp.list <-
plot.pdp.numeric.list$clim.suit.scaled +
plot.pdp.numeric.list$tdwg.level03.num.scaled +
plot.pdp.factor.list$tdwg.X1 +
plot.pdp.factor.list$tdwg.X2 +
plot.pdp.factor.list$tdwg.X3 +
plot.pdp.factor.list$tdwg.X4 +
plot.pdp.factor.list$tdwg.X5 +
plot.pdp.factor.list$tdwg.X6 +
plot.pdp.factor.list$tdwg.X7 +
plot.pdp.factor.list$tdwg.X8 +
plot.pdp.factor.list$tdwg.X9 +
plot.pdp.numeric.list$boga.scaled +
plot.pdp.numeric.list$prov.scaled +
plot.pdp.numeric.list$eco.use_num.scaled +
plot.pdp.factor.list$eco.use_AF +
plot.pdp.factor.list$eco.use_EU +
plot.pdp.factor.list$eco.use_FU +
plot.pdp.factor.list$eco.use_GS +
plot.pdp.factor.list$eco.use_HF +
plot.pdp.factor.list$eco.use_IF +
plot.pdp.factor.list$eco.use_MA +
plot.pdp.factor.list$eco.use_ME +
plot.pdp.factor.list$eco.use_PO +
plot.pdp.factor.list$eco.use_SU +
plot.pdp.numeric.list$max.height.scaled +
plot.pdp.factor.list$lf.short_herb +
plot.pdp.factor.list$lf.long_herb +
plot.pdp.factor.list$lf.woody +
plot.pdp.factor.list$prop.seed +
plot.pdp.factor.list$prop.veg +
plot.pdp.factor.list$prop.both +
plot.pdp.numeric.list$pd.mean.scaled +
plot.pdp.numeric.list$pd.min.scaled +
plot.pdp.numeric.list$pd.wmean.scaled +
# plot.pdp.numeric.list$res.time.scaled +
plot_layout(ncol = 7, tag_level = 'new', guides = "collect") +
plot_annotation(tag_levels = list(tags)) # &
# theme(plot.margin = margin(r = 8))

plot.pdp.list

# save plots
ggexport(plot.pdp.list, filename = "./Results20240220/20240511.plot.pdp.list.scaled.png",
         width = 7000,
         height = 3200,
         pointsize = 12,
         res = 300)
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

# # loading dataset of invasion
# invasion_list <- read_excel("./chklist.ivasion2018_20230110_v14_mvk_dong.xlsx", sheet = "data")
# invasion_list <- read_csv("./chklist.ivasion2018_20230929_v23.csv")
invasion_list <- read_csv("./chklist.ivasion2018_20230929_v24.csv")
# skimr::skim(invasion_list)

colnames(invasion_list)

# check duplicated data
table(duplicated(invasion_list$TPL_names))

# selection of columns
invasion_list01 <- invasion_list %>%
dplyr::select(
              TPL_names,
              invasion.status.2022,
# propagule pressure，
              bg_num,
              chklst_prov_num,
              wcup_eco_use_AF,
              wcup_eco_use_EU,
              wcup_eco_use_FU,
              wcup_eco_use_GS,
              wcup_eco_use_HF,
              wcup_eco_use_IF,
              wcup_eco_use_MA,
              wcup_eco_use_ME,
              wcup_eco_use_PO,
              wcup_eco_use_SU,
              wcup_eco_use_num,
# climatic suitability,
              hab_suit_mean,
              X1,
              X2,
              X3,
              X4,
              X5,
              X6,
              X7,
              X8,
              X9,
              no.NativeRange.level03,
# species traits,
              lf_short.herb,
              lf_long.herb,
              lf_woody,
              prop_type.seed.v2,
              prop_type.veg.v2,
              prop_type.both.v2,
              max.height.new2023,
              pd.mean.to_native,
              pd.min.to_native,
              pd.wmean.to_native) %>%
mutate_at(vars(invasion.status.2022:pd.wmean.to_native), ~ as.numeric(.)) %>%
mutate_at(vars(hab_suit_mean, max.height.new2023, pd.wmean.to_native), ~ round(., 3))

skimr::skim(invasion_list01)

# check missing data
# https://cloud.tencent.com/developer/article/1656428
# install.packages("VIM")
library(VIM)
aggr(invasion_list01, prop = FALSE, numbers = TRUE)

# rename columns
invasion_list01 <- invasion_list01 %>%
      rename(
              invasion.status = invasion.status.2022,
# propagule pressure，
              boga = bg_num,
              prov = chklst_prov_num,
              eco.use_AF = wcup_eco_use_AF,
              eco.use_EU = wcup_eco_use_EU,
              eco.use_FU = wcup_eco_use_FU,
              eco.use_GS = wcup_eco_use_GS,
              eco.use_HF = wcup_eco_use_HF,
              eco.use_IF = wcup_eco_use_IF,
              eco.use_MA = wcup_eco_use_MA,
              eco.use_ME = wcup_eco_use_ME,
              eco.use_PO = wcup_eco_use_PO,
              eco.use_SU = wcup_eco_use_SU,
              eco.use_num = wcup_eco_use_num,
# climatic suitability,
              clim.suit = hab_suit_mean,
              tdwg.X1 = X1,
              tdwg.X2 = X2,
              tdwg.X3 = X3,
              tdwg.X4 = X4,
              tdwg.X5 = X5,
              tdwg.X6 = X6,
              tdwg.X7 = X7,
              tdwg.X8 = X8,
              tdwg.X9 = X9,
              tdwg.level03.num = no.NativeRange.level03,
# species traits,
              lf.short_herb = lf_short.herb,
              lf.long_herb = lf_long.herb,
              lf.woody = lf_woody,
              prop.seed = prop_type.seed.v2,
              prop.veg = prop_type.veg.v2,
              prop.both = prop_type.both.v2,
              max.height = max.height.new2023,
              pd.mean = pd.mean.to_native,
              pd.min = pd.min.to_native,
              pd.wmean = pd.wmean.to_native)

# remove rows with any NAs
invasion_list02 <- invasion_list01 %>%
      tidyr::drop_na()

# ==========================================================================
# imputation
# ==========================================================================
library(randomForest)

invasion_list01a <- invasion_list01 %>%
select(-TPL_names) %>%
mutate_at(vars(invasion.status, eco.use_AF:eco.use_SU, tdwg.X1:tdwg.X9, lf.short_herb:prop.both), ~as.factor(.))

skimr::skim(invasion_list01a)

set.seed(2024)
invasion_list01.imputed <- rfImpute(invasion.status ~ ., invasion_list01a)

invasion_list02 <- invasion_list01.imputed %>%
mutate(invasion.status = as.numeric(as.character(invasion.status)),
       TPL_names = invasion_list01$TPL_names,
       # res.time = round(res.time, 0),
       prop.both = case_when(prop.seed == 1 & prop.veg == 1 ~ 1,
                             TRUE ~ 0)) %>%
mutate(prop.both = as.factor(prop.both)) %>%
relocate(invasion.status, .before = everything()) %>%
relocate(TPL_names, .before = everything())

skimr::skim(invasion_list02)

# ---------------------------- data transformation --------------------------- #
# standardized the data after log-transformation
# rename columns
invasion_list03 <- invasion_list02 %>%
      mutate(
# propagule pressure，
              boga.scaled = scale(log(boga + 1)),
              prov.scaled = scale(log(prov + 1)),
              eco.use_num.scaled = scale(log(eco.use_num + 1)),
# climatic suitability,
              clim.suit.scaled = scale(log(clim.suit + 0.001)),
              tdwg.level03.num.scaled = scale(log(tdwg.level03.num + 1)),
# species traits,
              pd.mean.scaled = scale(log(pd.mean)),
              pd.min.scaled = scale(log(pd.min)),
              pd.wmean.scaled = scale(log(pd.wmean)))

# based on category of Life_form_new，log-transform and scale max.height
invasion_list03$max.height.scaled <- invasion_list03$max.height
invasion_list03$max.height.scaled[which(invasion_list03$lf.short_herb == 1 & invasion_list03$lf.woody == 0)] <- scale(log(invasion_list03$max.height.scaled[which(invasion_list03$lf.short_herb == 1 & invasion_list03$lf.woody == 0)]))
invasion_list03$max.height.scaled[which(invasion_list03$lf.long_herb == 1 & invasion_list03$lf.woody == 0)] <- scale(log(invasion_list03$max.height.scaled[which(invasion_list03$lf.long_herb == 1 & invasion_list03$lf.woody == 0)]))
invasion_list03$max.height.scaled[which(invasion_list03$lf.woody == 1)] <- scale(log(invasion_list03$max.height.scaled[which(invasion_list03$lf.woody == 1)]))

invasion_list03 <- invasion_list03 %>%
select(
              invasion.status,
# propagule pressure，
              boga.scaled,
              prov.scaled,
              eco.use_AF,
              eco.use_EU,
              eco.use_FU,
              eco.use_GS,
              eco.use_HF,
              eco.use_IF,
              eco.use_MA,
              eco.use_ME,
              eco.use_PO,
              eco.use_SU,
              eco.use_num.scaled,
# climatic suitability,
              clim.suit.scaled,
              tdwg.X1,
              tdwg.X2,
              tdwg.X3,
              tdwg.X4,
              tdwg.X5,
              tdwg.X6,
              tdwg.X7,
              tdwg.X8,
              tdwg.X9,
              tdwg.level03.num.scaled,
# species traits,
              lf.short_herb,
              lf.long_herb,
              lf.woody,
              prop.seed,
              prop.veg,
              prop.both,
              max.height.scaled,
              pd.mean.scaled,
              pd.min.scaled,
              pd.wmean.scaled)

invasion_list04 <- invasion_list03 %>%
mutate(invasion.status = as.factor(ifelse(invasion_list03$invasion.status == 1, "yes", "no"))) %>%
mutate_at(vars(eco.use_AF:eco.use_SU, tdwg.X1:tdwg.X9, lf.short_herb:prop.both), ~as.numeric(as.character(.))) %>%
# mutate_at(vars(boga.scaled, prov.scaled, eco.use_num.scaled, clim.suit.scaled, tdwg.level03.num.scaled, max.height.scaled, pd.mean.scaled, pd.min.scaled, pd.wmean.scaled), ~as.numeric(.))
mutate_at(vars(-invasion.status), ~as.numeric(.))

str(invasion_list04)

# save to .Rdata
save(invasion_list01,
     invasion_list02,
     invasion_list03,
     invasion_list04,
     file = "./Results20240220/20240220.X000.raw_invasion.data.Rdata"
)

# ==========================================================================
# 相关性分析结果
# ==========================================================================
library(ggcorrplot)

# Calculate correlation matrix
correlation_matrix <- invasion_list04 %>% select(-invasion.status) %>% cor()
correlation_matrix

# Calculate p-values for correlations
p_values <- invasion_list04 %>% select(-invasion.status) %>% cor_pmat()

# Create correlation plot with p-values
pair.plot <- ggcorrplot(correlation_matrix,
           method = "square",
           # hc.order = TRUE,
           type = "upper",
           p.mat = p_values)

pair.plot01 <- pair.plot +
theme(panel.background = element_rect(fill = NA),
      # axis.line = element_line(size = 1, linetype = "solid"),
      # axis.ticks = element_line(colour = "black", linetype = "solid"),
      axis.text = element_text(family = "serif", colour = "black", size = 14),
      legend.text = element_text(family = "serif", colour = "black", size = 14),
      legend.title = element_text(family = "serif", colour = "black", size = 14))

var.fullName <- c(
boga.scaled = "No. of botanical gardens",
prov.scaled = "No. of provinces",
eco.use_AF = "EU: animal food",
eco.use_EU = "EU: environmental uses",
eco.use_FU = "EU: fuels",
eco.use_GS = "EU: gene sources",
eco.use_HF = "EU: human food",
eco.use_IF = "EU: invertebrate food",
eco.use_MA = "EU: materials",
eco.use_ME = "EU: medicine",
eco.use_PO = "EU: poisons",
eco.use_SU = "EU: social uses",
eco.use_num.scaled = "No. of economic uses (EU)",
clim.suit.scaled = "Climatic suitability",
tdwg.X1 = "NR: Europe",
tdwg.X2 = "NR: Africa",
tdwg.X3 = "NR: Asia-Temp.",
tdwg.X4 = "NR: Asia-Trop.",
tdwg.X5 = "NR: Australasia",
tdwg.X6 = "NR: Pacific Isl.",
tdwg.X7 = "NR: North. America",
tdwg.X8 = "NR: South. America",
tdwg.X9 = "NR: Antarctic",
tdwg.level03.num.scaled = "Native range (NR) size",
lf.short_herb = "Life form: short-lived",
lf.long_herb = "Life form: long-lived",
lf.woody = "Life form: woody",
prop.seed = "Prop. mode: seed",
prop.veg = "Prop. mode: vegetative",
prop.both = "Prop. mode: both",
max.height.scaled = "Max. height",
pd.mean.scaled = "MPD",
pd.min.scaled = "NNPD",
pd.wmean.scaled = "Weighted MPD"
              )

pair.plot01 <- pair.plot01 +
scale_x_discrete(labels = var.fullName) +
scale_y_discrete(labels = var.fullName)

pair.plot01

# Figure.pair.plot
ggexport(pair.plot01, filename = "./Results20240220/20240220.correlation.plot.png",
                     width = 3000,
                     height = 3000,
                     pointsize = 12,
                     res = 300)
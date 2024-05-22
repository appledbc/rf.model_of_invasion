# Author:
# Date: Tue Jan 10 2023
# Last Modified by:
# Last Modified time: Tue Jan 10 2023
# Email:
# Description: randomzation test for observed relative importance

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

# calculating roc
library(pROC)

# prediction for prob
pred.prob <- predict(model.ranger, test.set, type = "prob")[, "yes"]

pred.prob

pROC::roc(
    response = test.set$invasion.status,
    predictor = pred.prob,
    levels = levels(test.set$invasion.status)
)

# ---------------------------------------------------------------------------- #
#                               model comparison                               #
# ---------------------------------------------------------------------------- #
library(ingredients)
library(DALEXtra)
library(DALEX)

# probability function
p_fun <- function(object, newdata) {
    predict(object, newdata = newdata, type = "prob")[, "yes"]
}

# 1. create a data frame with just the features
features <- train.set[colnames(train.set) != "invasion.status"]
skim(features)

# 2. Create a vector with the actual responses
train.set01 <- train.set
train.set01$invasion.status <- as.numeric(ifelse(train.set$invasion.status == "yes", 1, 0))

# https://www.rdocumentation.org/packages/ingredients/versions/2.3.0/topics/feature_importance
set.seed(2024)
explainer.ranger <- DALEX::explain(model.ranger,
    label = "model.ranger",
    data = features,
    y = train.set01$invasion.status,
    predict_function = p_fun
)

# ==========================================================================
# combination of relative importance from randomly sampling
# ==========================================================================
# base function
# #?? feature_importance
# set.seed(2024)
# fi.ranger <- feature_importance(explainer.ranger, B = 1) # 1 replicate
# plot(fi.ranger)
# fi.ranger.df <- data.frame(fi.ranger)
#
# save(explainer.ranger,
#      fi.ranger,
#      file = "./Results20240220/x005.fi.ranger1000.Rdata")

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
              "Economic use: AF",
              "Economic use: EU",
              "Economic use: FU",
              "Economic use: GS",
              "Economic use: HF",
              "Economic use: IF",
              "Economic use: MA",
              "Economic use: ME",
              "Economic use: PO",
              "Economic use: SU",
              "No. of economic use",
# introduction history,
              # "Min. residence time",
# climatic suitability,
              "Climatic suitbaility",
              "Native range: Europe",
              "Native range: Africa",
              "Native range: Asia-Temp.",
              "Native range: Asia-Trop.",
              "Native range: Australasia",
              "Native range: Pacific Isl.",
              "Native range: North. America",
              "Native range: South. America",
              "Native range: Antarctic",
              "Native range size",
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
# propagule pressure，
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

# ==========================================================================
# combination of relative importance from randomly sampling
# this process repeated 1000 times.
# ==========================================================================
# creating a sequence of 35 variables
sample_population <- seq(nrow(feature.df))
sample_population

# set variable number for each category
sample_sizes <- feature.df %>%
group_by(variable.group) %>%
summarise(sizes = n())

sample_sizes
# set a number of replicates
num_seeds <- 999

# set a sequence of replicates
seeds <- 1:num_seeds

# set a null data frame for storing output from all combination of randomly sampling
out <- data.frame(seed = numeric(),
                  variable.group = character(),
                  sizes = numeric(),
                  index = numeric(),
                  variable = character())


# loop for randomly sampling
for(j in 1:num_seeds){

  message(j)

  # set seed for each loop
  set.seed(seeds[j])

  # # set a null data frame for storing output from one randomly sampling
  sample_out <- data.frame(seed = numeric(),
                           variable.group = character(),
                           sizes = numeric(),
                           index = numeric(),
                           variable = character())

for (i in seq_len(nrow(sample_sizes))){


    # obtaining the sample size of the current category
    cur_size <- sample_sizes$sizes[i]

    # removing the variables that are last sampled
    available_population <- setdiff(sample_population, sample_out$index)

    # sampling "cur_size" variables from the current remained variables
    cur_sample <- sample(available_population, size = cur_size, replace = FALSE)
    cur_variable <- feature.df$variable[cur_sample]

    # storing the sampling output for the current category
    sample_out <- rbind(sample_out, cbind(seed = seeds[j],
                                          pseudo.variable.group = sample_sizes$variable.group[i],
                                          sizes = cur_size,
                                          index = cur_sample,
                                          variable = cur_variable))
  }

  # combine all the output of 1000 replicates
  out <- rbind(out, sample_out)

}


feature.sample <- as.data.frame(out, stringsAsFactors = FALSE)
feature.sample <- feature.sample %>%
mutate_at(vars(seed, sizes, index), ~as.numeric(.)) %>%
mutate(combin.name = paste(pseudo.variable.group, seed, sep = "_x_"))

head(feature.sample)
View(feature.sample)

# convert data.frame to list
# extracting grouped variables from from randomly sampling
combins.sample.group <- feature.sample %>%
distinct(seed, pseudo.variable.group, combin.name) %>%
mutate(var01 = combin.name,
       var02 = NA,
       combin.type = "sample.group")

head(combins.sample.group)
str(combins.sample.group)

# creating a list of combinations of variables within each category
combins.list.sample <- list()

for (i in 1:nrow(combins.sample.group)) {

    message(i)

    # single variables
        var.group01 <- feature.sample %>%
            filter(combin.name == combins.sample.group$var01[i])

        combins.list.sample[[i]] <- c(var.group01$variable)

        names(combins.list.sample)[i] <- (combins.sample.group)$combin.name[i]

}

# checking the results
str(combins.list.sample)
head(combins.list.sample)

# ==========================================================================
# parallel computation
# calculating the dropout_loss value using feature_importance
# ==========================================================================
# loading packages
library(doSNOW)
library(parallel)
library(rlist)
library(doRNG)
library(Rmpi)

# preparation for parallel
# set.seed(2024)
detectCores()
detectCores(logical = F)

# 28 cores here, but you should set core number base on your own computer.
cl <- parallel::makeCluster(28)
registerDoSNOW(cl)

# set progress bar
reps <- 1000
pb <- txtProgressBar(max = reps, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

set.seed(2024)

# main code: parallel for machine learning
out <- foreach(
    i = 1:reps,
    .packages = c("caret", "ingredients", "DALEX"),
    .options.snow = opts,
    .options.RNG = 2022
) %dorng% {
    # calculating feature_importance
    out <- feature_importance(explainer.ranger,
        variable_groups = combins.list.sample,
        B = 1,
        label = i
    )
    return(out)
}

# close parallel
close(pb)
stopCluster(cl)

fi.ranger_combins.sample <- out

head(fi.ranger_combins.sample)

str(fi.ranger_combins.sample)

# storing data as .Rdata
save(
    # gmeanSummary,
    model.ranger,
    p_fun,
    features,
    train.set01,
    explainer.ranger,
    feature.df,
    feature.sample,
    combins.list.sample,
    fi.ranger_combins.sample,
    file = "./Results20240220/20240220.x005.fi.ranger_combins.sample.Rdata"
)

# load("./Results20240220/20240220.x005.fi.ranger_combins.sample.Rdata")

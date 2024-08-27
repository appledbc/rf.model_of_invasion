# Author:
# Date: Tue Jan 10 2023
# Last Modified by:
# Last Modified time: Tue Jan 10 2023
# Email:
# Description: Self-Organizing Map (SOM) Analysis
# https://sebnemer.github.io/english/courses/unsupervised2020/self-organising-maps.html
# cleaning memory
cat("\014")
rm(list = ls())
gc()

#  loading packages
library(tidyverse)
library(kohonen)
library(ggplot2)
library(patchwork)
library(gridExtra)
library(ggpubr)
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
#                        Self-Organizing Map (SOM) Analysis                    #
# ---------------------------------------------------------------------------- #
# 1. Data Preprocessing
som_data <- invasion_list04 %>%
  select(-invasion.status) %>%  # Remove invasion.status column
  select_if(is.numeric)

# Convert data to matrix
som_data_matrix <- as.matrix(som_data)

# 2. SOM Model Training
# Set hexagonal grid
grid_size <- ceiling(sqrt(5 * sqrt(nrow(som_data_matrix))))
som_grid <- kohonen::somgrid(xdim = grid_size, ydim = grid_size, topo = "hexagonal")

# Train SOM
set.seed(2024)  # For reproducibility
som_model <- kohonen::som(som_data_matrix,
                          grid = som_grid,
                          rlen = 1000,      # Number of iterations
                          alpha = c(0.05, 0.01),  # Learning rate
                          keep.data = TRUE)

# 3. Visualization and Interpretation
# Function to plot SOM
plot_som <- function(som_model, plot_type, main_title) {
  plot(som_model, type = plot_type, main = main_title)
}

# Cluster SOM nodes
set.seed(2024)  # For reproducibility
som_clusters <- cutree(hclust(dist(som_model$codes[[1]])), 6)  # 6 clusters, adjust as needed
som_clusters

# plot(som_model, type = "mapping", labels = invasion_list04$invasion.status)

# Plot U-matrix and other plots
plot_types <- c("changes", "count", "quality")
plot_titles <- c("Training Progress", "Node Counts", "Quality")

par(mfrow = c(2, 2))
for (i in seq_along(plot_types)) {
  plot_som(som_model, plot_types[i], plot_titles[i])
}

# Define color function
coolBlueHotRed <- colorRampPalette(c("blue", "white", "red"))

# Define feature variables and their full names
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


plot_som_component <- function(som_model, component, vars, palette_func, features) {

  # extract grid information
  grid <- som_model$grid$pts

  # ensure the grid is hexagonal
  if (som_model$grid$topo != "hexagonal") {
    stop("SOM model must use hexagonal topology for honeycomb plot")
  }

  # extract values for the specified component
  values <- som_model$codes[[1]][, component]

  # create a data frame for plotting
  plot_data <- data.frame(
                          x = grid[, 1],
                          y = grid[, 2],
                          value = values
                          )

# set main title
  main_title <- features %>%
  filter(variable == vars) %>%
  pull(variable.fullName)

# main plotting function
  p <- ggplot(plot_data, aes(x = x, y = y, fill = value)) +
  geom_hex(stat = "identity") +
  scale_fill_gradientn(colours = palette_func(100)) +
  coord_equal() +
  ggtitle(main_title) +
  theme_minimal() +
  theme(
        legend.position = "right",
        legend.title = element_text(family = "serif", colour = "black", size = 14),
        plot.title = element_text(family = "serif", colour = "black", size = 14),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
        )

  return(p)
}

# create plots for all components
# define a color palette function from blue to white to red
coolBlueHotRed <- colorRampPalette(c("blue", "white", "red"))

# number of variables
n_vars <- ncol(som_data)

# create plots for each component
som_plots <- purrr::map(seq_len(n_vars), function(i) {
  plot_som_component(som_model, i, names(som_data)[i], coolBlueHotRed, feature.var.df.scaled)
})

# name the plots with the corresponding variable names
names(som_plots) <- names(som_data)

# combine all plots into a single grid
do.call(grid.arrange, c(som_plots, ncol = ceiling(sqrt(n_vars))))

# plots
# set labels as AA、AB、AC、AD... and so on
tags <- c(LETTERS, paste0("A", LETTERS))

som.plot.list <-
som_plots$clim.suit.scaled +
som_plots$tdwg.level03.num.scaled +
som_plots$tdwg.X1 +
som_plots$tdwg.X2 +
som_plots$tdwg.X3 +
som_plots$tdwg.X4 +
som_plots$tdwg.X5 +
som_plots$tdwg.X6 +
som_plots$tdwg.X7 +
som_plots$tdwg.X8 +
som_plots$tdwg.X9 +
som_plots$boga.scaled +
som_plots$prov.scaled +
som_plots$eco.use_num.scaled +
som_plots$eco.use_AF +
som_plots$eco.use_EU +
som_plots$eco.use_FU +
som_plots$eco.use_GS +
som_plots$eco.use_HF +
som_plots$eco.use_IF +
som_plots$eco.use_MA +
som_plots$eco.use_ME +
som_plots$eco.use_PO +
som_plots$eco.use_SU +
som_plots$max.height.scaled +
som_plots$lf.short_herb +
som_plots$lf.long_herb +
som_plots$lf.woody +
som_plots$prop.seed +
som_plots$prop.veg +
som_plots$prop.both +
som_plots$pd.mean.scaled +
som_plots$pd.min.scaled +
som_plots$pd.wmean.scaled +
plot_layout(ncol = 7, tag_level = 'new') +
plot_annotation(tag_levels = list(tags))

som.plot.list

# save plots
ggexport(som.plot.list, filename = "./Results20240220/20240722.som.plot.list.scaled.png",
         width = 7000,
         height = 3200,
         pointsize = 12,
         res = 300)


#
# Assuming som_model is your trained SOM model
# som_distances <- object.distances(som_model, type = "codes")
som_cluster <- cutree(hclust(dist(som_model$codes[[1]])), 6)  # For 6 clusters

plot(som_model, type="mapping", main = "Clusters", cex = .5)

add.cluster.boundaries(som_model, som_cluster)

# Get grid coordinates
grid <- som_model$grid$pts

# Create data frame
plot_data01 <- data.frame(
  x = grid[, 1],
  y = grid[, 2],
  cluster = factor(som_cluster)
)

# 创建一个颜色映射，确保每个聚类有唯一的颜色
cluster_colors <- scales::hue_pal()(length(unique(plot_data01$cluster)))
names(cluster_colors) <- levels(plot_data01$cluster)

# 将颜色添加到 plot_data01
plot_data01$cluster_colors <- cluster_colors[as.character(plot_data01$cluster)]

plot_data01$cluster_label <- paste0("C", plot_data01$cluster)

# Create visualization
hits_plot <- ggplot(plot_data01, aes(x = x, y = y, fill = cluster)) +
  geom_hex(stat = "identity", color = "black", fill = plot_data01$cluster_colors) +
  geom_text(aes(label = cluster), color = "black", size = 3) +
  scale_fill_manual(values = cluster_colors) +
  coord_equal() +
  theme_minimal() +
  theme(
        legend.position = "none",
        legend.title = element_text(family = "serif", colour = "black", size = 14),
        plot.title = element_text(family = "serif", colour = "black", size = 14),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
        )

# Display the plot
print(hits_plot)

# save plots
ggexport(hits_plot, filename = "./Results20240220/20240722.hits_plot.scaled.png",
         width = 7000,
         height = 3200,
         pointsize = 12,
         res = 300)


som.plot.list02 <- som.plot.list + hits_plot

# save plots
ggexport(som.plot.list02, filename = "./Results20240220/20240722.som.plot.list02.scaled.png",
         width = 7000,
         height = 3200,
         pointsize = 12,
         res = 300)

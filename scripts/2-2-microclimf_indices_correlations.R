
rm(list = ls())

library(tidyverse)
library(corrplot)
library(ggfortify)
library(ggrepel)

microclimf_indices <- read.csv("data/2-microclimf/microclimf_indices.csv", row.names = 1)

# Add index categories
index_categories <- data.frame(
  index = colnames(select(microclimf_indices, - Plot_ID)),
  category = c("A", "A", "A", "V", "V", "V", "V", "V", "V", "V",
               "E(max)", "E(max)", "E(max)", "E(max)", "E(max)", "E(max)", "E(max)",
               "E(min)", "E(min)", "E(min)", "E(min)", "E(min)", "E(min)", "E(min)"),
  # set names for figures
  index_names <- c(
    "Mean offset",
    "Median offset",
    "Equilibrium",
    "Offset of SDs",
    "Mean daily\noffset of SDs",
    "Amplitude offset (5%)",
    "Amplitude offset (2.5%)",
    "Mean daily\namplitude offset",
    "Offset of CV",
    "Slope",
    "Offset of\nmaxima (95%)",
    "Offset of\n maxima (97.5%)",
    "Offset of\nmaxima (100%)",
    "Mean daily offset\nof maxima (95%)",
    "Mean daily offset\nof maxima (97.5%)",
    "Mean daily offset\nof maxima (100%)",
    "p95 of daily\noffsets of maxima",
    "Offset of\nminima (5%)",
    "Offset of\nminima (2.5%)",
    "Offset of\nminima (0%)",
    "Offset of\nminima (5%)",
    "Mean daily offset\nof minima (2.5%)",
    "Mean daily offset\nof minima (0%)",
    "p5 of daily\noffsets of minima"
  )
)
index_categories$category <- factor(index_categories$category, levels = c("A", "V", "E(max)", "E(min)"))

# create correlation table
index_cor <- cor(select(microclimf_indices, -Plot_ID))
colnames(index_cor) <- index_categories$index_names
rownames(index_cor) <- index_categories$index_names

# create figure for all indices
custom_palette <- colorRampPalette(c("grey70", "white", "grey70"))(100)
tiff("plots/index_correlations_microclimf.tiff",
     height = 40, width = 40, units = "cm",
     compression = "lzw", res = 600
)
corrplot(index_cor,
         type = "lower", method = "color", addCoef.col = "grey20",number.cex=0.8,
         tl.col = c("#A4A23C", "#C96666", "#C96666", "#C96666", "#C96666", "#4C5B88", "#4C5B88", "#4C5B88", "#C96666",
                    "#A4A23C","#A4A23C","#A4A23C","#A4A23C","#A4A23C","#A4A23C", "#560404","#560404","#C96666","#C96666",
                    "#560404","#560404","#560404","#560404","#560404"
                    ),
         addgrid.col = "gray", tl.srt = 90, col = custom_palette,
         order = "hclust"
)
dev.off()

png("plots/index_correlations_microclimf.png",
    height = 40, width = 40, units = "cm", res = 600
)
corrplot(index_cor,
         type = "lower", method = "color", addCoef.col = "grey20",number.cex=0.8,
         tl.col = c("#A4A23C", "#C96666", "#C96666", "#C96666", "#C96666", "#4C5B88", "#4C5B88", "#4C5B88", "#C96666",
                    "#A4A23C","#A4A23C","#A4A23C","#A4A23C","#A4A23C","#A4A23C", "#560404","#560404","#C96666","#C96666",
                    "#560404","#560404","#560404","#560404","#560404"
         ),
         addgrid.col = "gray", tl.srt = 90, col = custom_palette,
         order = "hclust"
)
dev.off()


# ordination

indices_matrix <- as.matrix(select(microclimf_indices, -Plot_ID))
indices_matrix <- scale(indices_matrix)
pca_data <- prcomp(indices_matrix)
autoplot(
  pca_data,
  data = as.data.frame(indices_matrix),
  loadings = TRUE,
  loadings.label = TRUE,
  loadings.label.size = 3
)

# extract PC axes for plotting
PCAvalues <- data.frame(pca_data$x)

# extract loadings of the variables
PCAloadings <- data.frame(index = rownames(pca_data$rotation), pca_data$rotation)

# get explained variance
var_exp <- summary(pca_data)$importance[2, 1:2] * 100

# make labels for axes
x_lab <- paste0("PC1 (", round(var_exp[1], 1), "%)")
y_lab <- paste0("PC2 (", round(var_exp[2], 1), "%)")

PCAloadings <- left_join(PCAloadings, index_categories, by = "index")

# assign colours
cols <- c("A" = "#4C5B88", "V" = "#A4A23C", "E(max)" = "#560404", "E(min)" = "#C96666")

ggplot(PCAvalues, aes(x = PC1, y = PC2)) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, color = category, xend = (PC1*15),
                                       yend = (PC2*15)), arrow = arrow(length = unit(1/2, "picas"))) +
  geom_point(size = 2, alpha = 0.3) +
  geom_label_repel(
    data = PCAloadings,
    aes(x = PC1*15, y = PC2*15, label = index_names, color = category),
    max.overlaps = 50, size = 3
  ) +
  scale_color_manual(values = cols, name = "Target\nmicroclimate\nfacet") +
  labs(x = x_lab, y = y_lab) +
  theme_minimal()

ggsave("plots/microclimf_indices_pca.png", width = 6000, height = 5000, unit = "px", dpi = 600)

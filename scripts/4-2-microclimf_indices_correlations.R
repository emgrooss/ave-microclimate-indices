
rm(list = ls())

library(tidyverse)
library(corrplot)
library(ggfortify)
library(ggrepel)

microclimf_indices <- read.csv("data/microclimf/microclimf_indices.csv", row.names = 1)



# create correlation table
index_cor <- cor(select(microclimf_indices, -Plot_ID))

# create figure for all indices
custom_palette <- colorRampPalette(c("grey50", "white", "grey50"))(100)
tiff("plots/index_correlations_microclimf.tiff",
     height = 40, width = 40, units = "cm",
     compression = "lzw", res = 300
)
corrplot(index_cor,
         type = "lower", method = "color", addCoef.col = "black",
         tl.col = "black", addgrid.col = "gray", tl.srt = 45, col = custom_palette
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

# Extract PC axes for plotting
PCAvalues <- data.frame(pca_data$x)

# Extract loadings of the variables
PCAloadings <- data.frame(index = rownames(pca_data$rotation), pca_data$rotation)

# Add index categories
index_categories <- data.frame(
  index = colnames(indices_matrix),
  category = c("A", "A", "A", "V", "V", "V", "V", "V", "V", "V",
               "Emax", "Emax", "Emax", "Emax", "Emax", "Emax", "Emax",
               "Emin", "Emin", "Emin", "Emin", "Emin", "Emin", "Emin")
)
# Assign colours
cols <- c("A" = "#4C5B88", "V" = "#C9C766", "Emax" = "#560404", "Emin" = "#C96666")

PCAloadings <- left_join(PCAloadings, index_categories, by = "index")


ggplot(PCAvalues, aes(x = PC1, y = PC2)) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, color = category, xend = (PC1*15),
                                       yend = (PC2*15)), arrow = arrow(length = unit(1/2, "picas"))) +
  geom_point(size = 3) +
  geom_text_repel(
    data = PCAloadings,
    aes(x = PC1*15, y = PC2*15, label = index, color = category),
    max.overlaps = 50
  ) +
  scale_color_manual(values = cols)+
  theme_bw()

ggsave("plots/microclimf_indices_pca.jpg", width = 3500, height = 3500, unit = "px")

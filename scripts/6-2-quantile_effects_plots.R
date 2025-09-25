# ===============================================================================================================================
# Title       : 6-quantile_effects.R
# Description : Calculate & plot index quality for quantile-dependent indices at different quantile levels
# Author      : ###
# Affiliation : ###
# Contact     : ###
# Date        : 2025-09-24
# Version     : 1.0
# License     : MIT
# Notes       : Supplementary code for "Average, Variability, and Extremes: A framework to quantify microclimate temperature modulation"
# ===============================================================================================================================

rm(list = ls())

library(tidyverse)
library(RColorBrewer)

#--Plots-------------------------------------------------------------------------------------

# offset of maxima

max_offset_performance <- read.csv("data/5-index-performance/quantiles/offset_of_maxima_quantiles_lm.csv")

index_names <- max_offset_performance$index
max_offset_performance$index <- factor(max_offset_performance$index, levels = index_names)

colors <- c("a" = "#4C5B88", "v" = "#C9C766", "e" = "#C96666", err = "#000", "total" = "#AAA")

(max_offset_marginal_r2 <- ggplot(max_offset_performance) +
    geom_point(aes(x = index, y = a_marg_r2, col = "a")) +
    geom_line(aes(x = index, y = a_marg_r2, col = "a"), group = 1) +
    geom_point(aes(x = index, y = v_marg_r2, col = "v")) +
    geom_line(aes(x = index, y = v_marg_r2, col = "v"), group = 1) +
    geom_point(aes(x = index, y = e_marg_r2, col = "e")) +
    geom_line(aes(x = index, y = e_marg_r2, col = "e"), group = 1) +
    geom_point(aes(x = index, y = err_marg_r2, col = "err")) +
    geom_line(aes(x = index, y = err_marg_r2, col = "err"), group = 1) +
    geom_point(aes(x = index, y = ave_marg_r2, col = "total")) +
    geom_line(aes(x = index, y = ave_marg_r2, col = "total"), group = 1) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = colors, breaks = c("a", "v", "e", "err", "total")) +
    labs(
      x = "Quantile", y = "Marginal R²", color = "Explanatory\nvariable",
      title = "\na) Offset of maxima\n"
    ) +
    theme_bw()+
    theme(legend.position = "none")
)


# offset of minima

min_offset_performance <- read.csv("data/5-index-performance/quantiles/offset_of_minima_quantiles_lm.csv")

index_names <- min_offset_performance$index
min_offset_performance$index <- factor(min_offset_performance$index, levels = index_names)

# plot of slopes ~ quantiles
(min_offset_marginal_r2 <- ggplot(min_offset_performance) +
    geom_point(aes(x = index, y = a_marg_r2, col = "a")) +
    geom_line(aes(x = index, y = a_marg_r2, col = "a"), group = 1) +
    geom_point(aes(x = index, y = v_marg_r2, col = "v")) +
    geom_line(aes(x = index, y = v_marg_r2, col = "v"), group = 1) +
    geom_point(aes(x = index, y = e_marg_r2, col = "e")) +
    geom_line(aes(x = index, y = e_marg_r2, col = "e"), group = 1) +
    geom_point(aes(x = index, y = err_marg_r2, col = "err")) +
    geom_line(aes(x = index, y = err_marg_r2, col = "err"), group = 1) +
    geom_point(aes(x = index, y = ave_marg_r2, col = "total")) +
    geom_line(aes(x = index, y = ave_marg_r2, col = "total"), group = 1) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = colors, breaks = c("a", "v", "e", "err", "total")) +
    labs(
      x = "Quantile", y = "Marginal R²", color = "Explanatory\nvariable",
      title = "\nb) Offset of minima\n"
    ) +
    theme_bw()+
    theme(legend.position = "none"))

min_max_offset <- grid.arrange(max_offset_marginal_r2, min_offset_marginal_r2, nrow = 1)


# amplitude offset

amplitude_offset_performance <- read.csv("data/5-index-performance/quantiles/amplitude_offset_quantiles_lm.csv")

index_names <- amplitude_offset_performance$index
amplitude_offset_performance$index <- factor(amplitude_offset_performance$index, levels = index_names)

(amplitude_offset_marginal_r2 <- ggplot(amplitude_offset_performance) +
    geom_point(aes(x = index, y = a_marg_r2, col = "a")) +
    geom_line(aes(x = index, y = a_marg_r2, col = "a"), group = 1) +
    geom_point(aes(x = index, y = v_marg_r2, col = "v")) +
    geom_line(aes(x = index, y = v_marg_r2, col = "v"), group = 1) +
    geom_point(aes(x = index, y = e_marg_r2, col = "e")) +
    geom_line(aes(x = index, y = e_marg_r2, col = "e"), group = 1) +
    geom_point(aes(x = index, y = err_marg_r2, col = "err")) +
    geom_line(aes(x = index, y = err_marg_r2, col = "err"), group = 1) +
    geom_point(aes(x = index, y = ave_marg_r2, col = "total")) +
    geom_line(aes(x = index, y = ave_marg_r2, col = "total"), group = 1) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = colors, breaks = c("a", "v", "e", "err", "total")) +
    labs(
      x = "Quantile", y = "Marginal R²", color = "Explanatory\nvariable",
      title = "\nc) Amplitude offset \n"
    ) +
    theme_bw()+
    theme(legend.position = "none")
)

# function to get legend from plot
get_legend <- function(myggplot) {
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


(legend_plot <- ggplot(amplitude_offset_performance) +
    geom_point(aes(x = index, y = a_marg_r2, col = "a")) +
    geom_line(aes(x = index, y = a_marg_r2, col = "a"), group = 1) +
    geom_point(aes(x = index, y = v_marg_r2, col = "v")) +
    geom_line(aes(x = index, y = v_marg_r2, col = "v"), group = 1) +
    geom_point(aes(x = index, y = e_marg_r2, col = "e")) +
    geom_line(aes(x = index, y = e_marg_r2, col = "e"), group = 1) +
    geom_point(aes(x = index, y = err_marg_r2, col = "err")) +
    geom_line(aes(x = index, y = err_marg_r2, col = "err"), group = 1) +
    geom_point(aes(x = index, y = ave_marg_r2, col = "total")) +
    geom_line(aes(x = index, y = ave_marg_r2, col = "total"), group = 1) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = colors, breaks = c("a", "v", "e", "err", "total")) +
    labs(
      x = "Quantile", y = "Marginal R²", color = "Explanatory\nvariable",
      title = "\nc) Amplitude offset \n"
    ) +
    theme_bw()
)

legend = get_legend(legend_plot)

# save combined plot
quantile_plots <- grid.arrange(max_offset_marginal_r2, min_offset_marginal_r2, amplitude_offset_marginal_r2, legend,
                               nrow = 1, widths = c(0.3, 0.3, 0.3, 0.1))

ggsave("plots/quantile_performance.png", quantile_plots, width = 8000, height = 2600, unit = "px", dpi = 600)

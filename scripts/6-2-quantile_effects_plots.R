#--Plots-------------------------------------------------------------------------------------

# offset of maxima

max_offset_performance <- read.csv("data/5-index-performance/quantiles/offset_of_maxima_quantiles_lm.csv")

index_names <- max_offset_performance$index
max_offset_performance$index <- factor(max_offset_performance$index, levels = index_names)

colors <- c("a" = "#30406E", "v" = "#A4A23C", "e" = "#A43C3C", "total" = "#AAA")

(max_offset_marginal_r2 <- ggplot(max_offset_performance) +
    geom_point(aes(x = index, y = a_marg_r2, col = "a")) +
    geom_line(aes(x = index, y = a_marg_r2, col = "a"), group = 1) +
    geom_point(aes(x = index, y = v_marg_r2, col = "v")) +
    geom_line(aes(x = index, y = v_marg_r2, col = "v"), group = 1) +
    geom_point(aes(x = index, y = e_marg_r2, col = "e")) +
    geom_line(aes(x = index, y = e_marg_r2, col = "e"), group = 1) +
    geom_point(aes(x = index, y = ave_marg_r2, col = "total")) +
    geom_line(aes(x = index, y = ave_marg_r2, col = "total"), group = 1) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = colors, breaks = c("a", "v", "e", "total")) +
    labs(
      x = "Quantile", y = "Marginal R²", color = "Explanatory\nvariable",
      title = "\na) Offset of maxima\n"
    ) +
    theme_bw()
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
    geom_point(aes(x = index, y = ave_marg_r2, col = "total")) +
    geom_line(aes(x = index, y = ave_marg_r2, col = "total"), group = 1) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = colors, breaks = c("a", "v", "e", "total")) +
    labs(
      x = "Quantile", y = "Marginal R²", color = "Explanatory\nvariable",
      title = "\nb) Offset of minima\n"
    ) +
    theme_bw())

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
    geom_point(aes(x = index, y = ave_marg_r2, col = "total")) +
    geom_line(aes(x = index, y = ave_marg_r2, col = "total"), group = 1) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = colors, breaks = c("a", "v", "e", "total")) +
    labs(
      x = "Quantile", y = "Marginal R²", color = "Explanatory\nvariable",
      title = "\nc) Amplitude offset \n"
    ) +
    theme_bw()
)


# amplitude ratio

amplitude_ratio_performance <- read.csv("data/5-index-performance/quantiles/amplitude_ratio_quantiles_lm.csv")

index_names <- amplitude_ratio_performance$index
amplitude_ratio_performance$index <- factor(amplitude_ratio_performance$index, levels = index_names)

(amplitude_ratio_marginal_r2 <- ggplot(amplitude_ratio_performance) +
    geom_point(aes(x = index, y = a_marg_r2, col = "a")) +
    geom_line(aes(x = index, y = a_marg_r2, col = "a"), group = 1) +
    geom_point(aes(x = index, y = v_marg_r2, col = "v")) +
    geom_line(aes(x = index, y = v_marg_r2, col = "v"), group = 1) +
    geom_point(aes(x = index, y = e_marg_r2, col = "e")) +
    geom_line(aes(x = index, y = e_marg_r2, col = "e"), group = 1) +
    geom_point(aes(x = index, y = ave_marg_r2, col = "total")) +
    geom_line(aes(x = index, y = ave_marg_r2, col = "total"), group = 1) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = colors, breaks = c("a", "v", "e", "total")) +
    labs(
      x = "Quantile", y = "Marginal R²", color = "Explanatory\nvariable",
      title = "\nd) Amplitude ratio \n"
    ) +
    theme_bw()
)

amplitudes <- grid.arrange(amplitude_offset_marginal_r2, amplitude_ratio_marginal_r2, nrow = 1)

# save combined plot
quantile_plots <- grid.arrange(min_max_offset, amplitudes, nrow = 2)

ggsave("plots/quantile_performance.png", quantile_plots, width = 3000, height = 3000, unit = "px")

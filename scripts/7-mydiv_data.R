# Calculate microclimate indices for real climate data (MyDiv)

rm(list = ls())

library(tidyverse)
library(gridExtra)
library(grid)
library(ggsignif)

# read in index functions
source("scripts/0-index-functions.R")

# microclimate data
mydiv <- read.csv("data/5-example-data/mydiv_plot_temp.csv")
# macroclimate data from stations near the
macro <- read.csv("data/5-example-data/mydiv_macro_temp.csv")

# write a table of plots + species
mydiv_overview <- mydiv %>% count(plot, myc, composition)
write.csv(mydiv_overview, "data/5-example-data/mydiv_plots_species_overview.csv")

# get plot names
plotnames <- mydiv_overview$plot

# filter data from one climate station (Southwest) and resample to hourly measurements
macro_sw <- macro %>%
  filter(rep == "SW") %>%
  filter(between(as.Date(datetime), as.Date("2024-06-01"), as.Date("2024-09-30"))) %>%
  filter(!grepl(":30", datetime)) %>% # remove half hour measurements
  mutate(datetime = as_datetime(datetime)) %>%
  select(datetime, temperature)

# convert date format and remove missing data
mydiv <- mydiv %>%
  mutate(datetime = as_datetime(datetime)) %>%
  filter(!is.na(Air_Temperature_o)) %>%
  select(plot, myc, datetime, Air_Temperature_o)

# calculate indices

indices_table <- data.frame(
  plot = numeric(),
  myc = character(),
  mean_offset = numeric(),
  median_offset = numeric(),
  amplitude_offset = numeric(),
  change_ratio = numeric(),
  offset_maxima = numeric(),
  offset_minima = numeric()
)

for (i in plotnames) {
  plot <- i
  plot_data <- mydiv %>%
    filter(plot == i) %>%
    inner_join(macro_sw, by = "datetime")

  # get mycorrhiza type of plot
  myc <- plot_data$myc[1]

  # assign date column
  datetime <- plot_data$datetime

  # assign microclimate and macroclimate data
  microclim <- plot_data$Air_Temperature_o
  macroclim <- plot_data$temperature

  indices <- best_microclimate_indices(macroclimate = macroclim, microclimate = microclim, time.index = datetime)

  indices_table <- indices_table %>% add_row(tibble_row(
    plot = plot,
    myc = myc,
    mean_offset = indices["mean offset"],
    median_offset = indices["median offset"],
    amplitude_offset = indices["amplitude offset(p5-p95)"],
    change_ratio = indices["change ratio"],
    offset_maxima = indices["offset of maxima (p97.5)"],
    offset_minima = indices["offset of minima (p2.5)"]
  ))
}

write.csv(indices_table, "data/5-example-data/mydiv_indices.csv")


# significance tests and plots

indices <- read.csv("data/5-example-data/mydiv_indices.csv")

# t test, calculate mean and SD
t.test(mean_offset ~ myc, data = indices)
indices %>%
  group_by(myc) %>%
  summarise(mean = mean(mean_offset), sd = sd(mean_offset))

t.test(median_offset ~ myc, data = indices)
indices %>%
  group_by(myc) %>%
  summarise(mean = mean(median_offset), sd = sd(median_offset))

t.test(change_ratio ~ myc, data = indices)
indices %>%
  group_by(myc) %>%
  summarise(mean = mean(change_ratio), sd = sd(change_ratio))

t.test(amplitude_offset ~ myc, data = indices)
indices %>%
  group_by(myc) %>%
  summarise(mean = mean(amplitude_offset), sd = sd(amplitude_offset))

t.test(offset_maxima ~ myc, data = indices)
indices %>%
  group_by(myc) %>%
  summarise(mean = mean(offset_maxima), sd = sd(offset_maxima))

t.test(offset_minima ~ myc, data = indices)
indices %>%
  group_by(myc) %>%
  summarise(mean = mean(offset_minima), sd = sd(offset_minima))

# plots
mean <- ggplot(indices, aes(x = myc, y = mean_offset)) +
  geom_boxplot(aes(group = myc),
    col = "#30406E", outliers = F,
    fill = "#7581A5", alpha = 0.2, width = 0.5
  ) +
  geom_jitter(shape = 21, fill = "#7581A5", col = "#30406E", width = 0.05) +
  geom_signif(
    comparisons = list(c("AMF", "EMF")), map_signif_level = TRUE, test = "t.test",
    tip_length = 0, margin_top = -0.1, extend_line = -0.1
  ) +
  labs(x = "Mycorrhiza type", y = "Mean offset") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(0.9)))

median <- ggplot(indices, aes(x = myc, y = median_offset)) +
  geom_boxplot(aes(group = myc),
    col = "#30406E", outliers = F,
    fill = "#7581A5", alpha = 0.2, width = 0.5
  ) +
  geom_jitter(shape = 21, fill = "#7581A5", col = "#30406E", width = 0.05) +
  geom_signif(
    comparisons = list(c("AMF", "EMF")), map_signif_level = TRUE, test = "t.test",
    tip_length = 0, margin_top = -0.1, extend_line = -0.1
  ) +
  labs(x = "Mycorrhiza type", y = "Median offset") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(0.9)))

avgs <- grid.arrange(mean, median,
  nrow = 1,
  top = textGrob("\n(a) Average modulation", hjust = 1.4)
)

cr <- ggplot(indices, aes(x = myc, y = change_ratio)) +
  geom_boxplot(aes(group = myc),
    col = "#807E1C", outliers = F,
    fill = "#C9C766", alpha = 0.2, width = 0.5
  ) +
  geom_jitter(shape = 21, fill = "#C9C766", col = "#807E1C", width = 0.05) +
  geom_signif(
    comparisons = list(c("AMF", "EMF")), map_signif_level = TRUE, test = "t.test",
    tip_length = 0, margin_top = -0.1, extend_line = -0.1
  ) +
  labs(x = "Mycorrhiza type", y = "Change ratio") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(0.9)))

amp <- ggplot(indices, aes(x = myc, y = amplitude_offset)) +
  geom_boxplot(aes(group = myc),
    col = "#807E1C", outliers = F,
    fill = "#C9C766", alpha = 0.2, width = 0.5
  ) +
  geom_jitter(shape = 21, fill = "#C9C766", col = "#807E1C", width = 0.05) +
  geom_signif(
    comparisons = list(c("AMF", "EMF")), map_signif_level = TRUE, test = "t.test",
    tip_length = 0, margin_top = -0.1, extend_line = -0.1
  ) +
  labs(x = "Mycorrhiza type", y = "Amplitude offset (p5 to p95)") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(0.9)))

vars <- grid.arrange(cr, amp,
  nrow = 1,
  top = textGrob("\n(b) Variability modulation", hjust = 1.3)
)


max <- ggplot(indices, aes(x = myc, y = offset_maxima)) +
  geom_boxplot(aes(group = myc),
    col = "#801C1C", outliers = F,
    fill = "#C96666", alpha = 0.2, width = 0.5
  ) +
  geom_jitter(shape = 21, fill = "#C96666", col = "#801C1C", width = 0.05) +
  geom_signif(
    comparisons = list(c("AMF", "EMF")), map_signif_level = TRUE, test = "t.test",
    tip_length = 0, margin_top = -0.1, extend_line = -0.1
  ) +
  labs(x = "Mycorrhiza type", y = "Offset of maxima (p97.5)") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(0.9)))

min <- ggplot(indices, aes(x = myc, y = offset_minima)) +
  geom_boxplot(aes(group = myc),
    col = "#801C1C", outliers = F,
    fill = "#C96666", alpha = 0.2, width = 0.5
  ) +
  geom_jitter(shape = 21, fill = "#C96666", col = "#801C1C", width = 0.05) +
  geom_signif(
    comparisons = list(c("AMF", "EMF")), map_signif_level = TRUE, test = "t.test",
    tip_length = 0, margin_top = -0.1, extend_line = -0.1
  ) +
  labs(x = "Mycorrhiza type", y = "Offset of minima (p2.5)") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(0.9)))

exts <- grid.arrange(max, min,
  nrow = 1,
  top = textGrob("\n(c) Extreme modulation", hjust = 1.4)
)

all_plots <- grid.arrange(avgs, vars, exts, ncol = 1)

ggsave("plots/mydiv_indices.jpg", all_plots, width = 2000, height = 2800, unit = "px")

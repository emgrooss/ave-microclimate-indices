# ===============================================================================================================================
# Title       : 4-indices_calculation.R
# Description : Calculate microclimate indices for all simulated microclimates
# Author      : ###
# Affiliation : ###
# Contact     : ###
# Date        : 2025-04-13
# Version     : 1.0
# License     : MIT
# Notes       : Supplementary code for "A framework to quantify microclimate modulation using average, variability, and extremes"
# ===============================================================================================================================

rm(list = ls())

library(tidyverse)
source("scripts/0-index-functions.R")

microclimf <- readRDS("data/microclimf/microclimf_HAEHN.RDS")

# filter for sensor 3
microclimf <- microclimf %>% filter(sensor_ID == "T3")

# convert temperatures to Kelvin
microclimf$macro_temp <- microclimf$temp + 273.15
microclimf$micro_temp <- microclimf$climate_value + 273.15
microclimf$micro_pred <- microclimf$T_pred + 273.15
microclimf$PlotObservationID <- as.character(microclimf$PlotObservationID)

microclimf <- select(microclimf, c(PlotObservationID, obs_time, macro_temp, micro_temp))

plot_ids <- unique(microclimf$PlotObservationID)


indices_df <- data.frame("Plot_ID" = character(),
                         "mean_offset" = numeric(),
                         "median_offset" = numeric(),
                         "equilibrium" = numeric(),
                         "sd_offset" = numeric(),
                         "sd_offset_mean_daily" = numeric(),
                         "amplitude_offset.95" = numeric(),
                         "amplitude_offset.975"= numeric(),
                         "amplitude_offset_mean_daily"= numeric(),
                         "CV_offset" = numeric(),
                         "slope"= numeric(),
                         "offset_of_maxima.95"= numeric(),
                         "offset_of_maxima.975"= numeric(),
                         "offset_of_maxima1.00"= numeric(),
                         "offset_of_maxima_mean_daily.95" = numeric(),
                         "offset_of_maxima_mean_daily.975" = numeric(),
                        "offset_of_maxima_mean_daily1.00" = numeric(),
                        "p95_daily_maxima_offset" = numeric(),
                        "offset_of_minima.05" = numeric(),
                        "offset_of_minima.025" = numeric(),
                        "offset_of_minima.00" = numeric(),
                        "offset_of_minima_mean_daily.05" = numeric(),
                        "offset_of_minima_mean_daily.025" = numeric(),
                        "offset_of_minima_mean_daily.00" = numeric(),
                        "p5_daily_minima_offset" = numeric()
)

for(i in 1:length(plot_ids)){
  data = filter(microclimf, PlotObservationID == plot_ids[i])
  plot = plot_ids[i]
  indices_v = microclimate_indices(macroclimate = data$macro_temp, microclimate = data$micro_temp, time.index = data$obs_time)
  indices_df[i,] = c(plot, indices_v)
}

write.csv(indices_df, "data/microclimf/microclimf_indices.csv")


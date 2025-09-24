# ===============================================================================================================================
# Title       : 4-indices_calculation.R
# Description : Calculate microclimate indices for all simulated microclimates
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
source("scripts/0-index-functions.R")

files <- list.files("data/3-microclimate-simulations/")

indices <- map_dfr(1:length(files), function(i) {
  filename <- files[i]
  data <- read_rds(paste("data/3-microclimate-simulations/", filename, sep = ""))
  climates <- data[["Data"]]
  parameters <- data[["Parameters"]]
  index_vector <- microclimate_indices(
    macroclimate = climates$macro, microclimate = climates$micro,
    time.index = climates$date
  )
  indices <- c(
    parameters["site"], parameters["a"],
    parameters["v"], parameters["e"],
    parameters["err"],
    index_vector
  )
})

write.csv(indices, "data/4-indices/indices.csv")
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

files <- list.files("data/2-microclimate-simulations/microclimate-simulations/")

indices <- map_dfr(1:length(files), function(i) {
  filename <- files[i]
  data <- read_rds(paste("data/2-microclimate-simulations/microclimate-simulations/", filename, sep = ""))
  climates <- data[["Data"]]
  parameters <- data[["Parameters"]]
  index_vector <- microclimate_indices(
    macroclimate = climates$macro, microclimate = climates$micro,
    time.index = climates$date
  )
  indices <- c(
    parameters["site"], parameters["a"],
    parameters["v"], parameters["e"],
    index_vector
  )
})

write.csv(indices, "data/3-indices/indices.csv")


indices1 <- map_dfr(1:10000, function(i) {
  filename <- files[i]
  data <- read_rds(paste("data/2-microclimate-simulations/microclimate-simulations/", filename, sep = ""))
  climates <- data[["Data"]]
  parameters <- data[["Parameters"]]
  index_vector <- microclimate_indices(
    macroclimate = climates$macro, microclimate = climates$micro,
    time.index = climates$date
  )
  indices <- c(
    parameters["site"], parameters["a"],
    parameters["v"], parameters["e"],
    index_vector
  )
})
write.csv(indices1, "data/3-indices/indices10k.csv")


indices2 <- map_dfr(10001:20000, function(i) {
  filename <- files[i]
  data <- read_rds(paste("data/2-microclimate-simulations/microclimate-simulations/", filename, sep = ""))
  climates <- data[["Data"]]
  parameters <- data[["Parameters"]]
  index_vector <- microclimate_indices(
    macroclimate = climates$macro, microclimate = climates$micro,
    time.index = climates$date
  )
  indices <- c(
    parameters["site"], parameters["a"],
    parameters["v"], parameters["e"],
    index_vector
  )
})
write.csv(indices2, "data/3-indices/indices20k.csv")


indices3 <- map_dfr(20001:30000, function(i) {
  filename <- files[i]
  data <- read_rds(paste("data/2-microclimate-simulations/microclimate-simulations/", filename, sep = ""))
  climates <- data[["Data"]]
  parameters <- data[["Parameters"]]
  index_vector <- microclimate_indices(
    macroclimate = climates$macro, microclimate = climates$micro,
    time.index = climates$date
  )
  indices <- c(
    parameters["site"], parameters["a"],
    parameters["v"], parameters["e"],
    index_vector
  )
})
write.csv(indices3, "data/3-indices/indices30k.csv")


indices4 <- map_dfr(30001:40000, function(i) {
  filename <- files[i]
  data <- read_rds(paste("data/2-microclimate-simulations/microclimate-simulations/", filename, sep = ""))
  climates <- data[["Data"]]
  parameters <- data[["Parameters"]]
  index_vector <- microclimate_indices(
    macroclimate = climates$macro, microclimate = climates$micro,
    time.index = climates$date
  )
  indices <- c(
    parameters["site"], parameters["a"],
    parameters["v"], parameters["e"],
    index_vector
  )
})
write.csv(indices4, "data/3-indices/indices40k.csv")


indices5 <- map_dfr(40001:50000, function(i) {
  filename <- files[i]
  data <- read_rds(paste("data/2-microclimate-simulations/microclimate-simulations/", filename, sep = ""))
  climates <- data[["Data"]]
  parameters <- data[["Parameters"]]
  index_vector <- microclimate_indices(
    macroclimate = climates$macro, microclimate = climates$micro,
    time.index = climates$date
  )
  indices <- c(
    parameters["site"], parameters["a"],
    parameters["v"], parameters["e"],
    index_vector
  )
})
write.csv(indices5, "data/3-indices/indices50k.csv")


indices6 <- map_dfr(50001:60000, function(i) {
  filename <- files[i]
  data <- read_rds(paste("data/2-microclimate-simulations/microclimate-simulations/", filename, sep = ""))
  climates <- data[["Data"]]
  parameters <- data[["Parameters"]]
  index_vector <- microclimate_indices(
    macroclimate = climates$macro, microclimate = climates$micro,
    time.index = climates$date
  )
  indices <- c(
    parameters["site"], parameters["a"],
    parameters["v"], parameters["e"],
    index_vector
  )
})
write.csv(indices6, "data/3-indices/indices60k.csv")


indices7 <- map_dfr(60001:length(files), function(i) {
  filename <- files[i]
  data <- read_rds(paste("data/2-microclimate-simulations/microclimate-simulations/", filename, sep = ""))
  climates <- data[["Data"]]
  parameters <- data[["Parameters"]]
  index_vector <- microclimate_indices(
    macroclimate = climates$macro, microclimate = climates$micro,
    time.index = climates$date
  )
  indices <- c(
    parameters["site"], parameters["a"],
    parameters["v"], parameters["e"],
    index_vector
  )
})
write.csv(indices7, "data/3-indices/indices66k.csv")

indices_all <- rbind(indices1, indices2, indices3, indices4, indices5, indices6, indices7)

write.csv(indices_all, "data/3-indices/indices.csv")

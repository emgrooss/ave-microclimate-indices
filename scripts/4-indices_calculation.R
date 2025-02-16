rm(list=ls())

library(tidyverse)
source("2-code/2-4-1-2-index-functions.R")

files = list.files("1-data/1-2-microclimate-simulations/1-2-1-3-microclimate-simulations_FINAL/")

indices = map_dfr(1:length(files), function(i) {
  
  filename = files[i]
  data = read_rds(paste("1-data/1-2-microclimate-simulations/1-2-1-3-microclimate-simulations_FINAL/", filename, sep=""))
  climates = data[["Data"]]
  parameters = data[["Parameters"]]
  index_vector = microclimate_indices(macroclimate = climates$macro, microclimate = climates$micro,
                                      time.index = climates$date)
  indices = c(parameters["site"], parameters["a"],
              parameters["v"],parameters["e"],
              index_vector)
}) 

write.csv(indices, "1-data/1-3-indices/1-3-1-3-indices_FINAL.csv")


indices1 = map_dfr(1:10000, function(i) {
  
  filename = files[i]
  data = read_rds(paste("1-data/1-2-microclimate-simulations/1-2-1-3-microclimate-simulations_FINAL/", filename, sep=""))
  climates = data[["Data"]]
  parameters = data[["Parameters"]]
  index_vector = microclimate_indices(macroclimate = climates$macro, microclimate = climates$micro,
                                      time.index = climates$date)
  indices = c(parameters["site"], parameters["a"],
              parameters["v"],parameters["e"],
              index_vector)
}) 
write.csv(indices1, "1-data/1-3-indices/1-3-1-3-indices10k_FINAL.csv")


indices2 = map_dfr(10001:20000, function(i) {
  
  filename = files[i]
  data = read_rds(paste("1-data/1-2-microclimate-simulations/1-2-1-3-microclimate-simulations_FINAL/", filename, sep=""))
  climates = data[["Data"]]
  parameters = data[["Parameters"]]
  index_vector = microclimate_indices(macroclimate = climates$macro, microclimate = climates$micro,
                                      time.index = climates$date)
  indices = c(parameters["site"], parameters["a"],
              parameters["v"],parameters["e"],
              index_vector)
})
write.csv(indices2, "1-data/1-3-indices/1-3-1-3-indices20k_FINAL.csv")


indices3 = map_dfr(20001:30000, function(i) {
  
  filename = files[i]
  data = read_rds(paste("1-data/1-2-microclimate-simulations/1-2-1-3-microclimate-simulations_FINAL/", filename, sep=""))
  climates = data[["Data"]]
  parameters = data[["Parameters"]]
  index_vector = microclimate_indices(macroclimate = climates$macro, microclimate = climates$micro,
                                      time.index = climates$date)
  indices = c(parameters["site"], parameters["a"],
              parameters["v"],parameters["e"],
              index_vector)
})
write.csv(indices3, "1-data/1-3-indices/1-3-1-3-indices30k_FINAL.csv")


indices4 = map_dfr(30001:40000, function(i) {
  
  filename = files[i]
  data = read_rds(paste("1-data/1-2-microclimate-simulations/1-2-1-3-microclimate-simulations_FINAL/", filename, sep=""))
  climates = data[["Data"]]
  parameters = data[["Parameters"]]
  index_vector = microclimate_indices(macroclimate = climates$macro, microclimate = climates$micro,
                                      time.index = climates$date)
  indices = c(parameters["site"], parameters["a"],
              parameters["v"],parameters["e"],
              index_vector)
})
write.csv(indices4, "1-data/1-3-indices/1-3-1-3-indices40k_FINAL.csv")


indices5 = map_dfr(40001:50000, function(i) {
  
  filename = files[i]
  data = read_rds(paste("1-data/1-2-microclimate-simulations/1-2-1-3-microclimate-simulations_FINAL/", filename, sep=""))
  climates = data[["Data"]]
  parameters = data[["Parameters"]]
  index_vector = microclimate_indices(macroclimate = climates$macro, microclimate = climates$micro,
                                      time.index = climates$date)
  indices = c(parameters["site"], parameters["a"],
              parameters["v"],parameters["e"],
              index_vector)
})
write.csv(indices5, "1-data/1-3-indices/1-3-1-3-indices50k_FINAL.csv")


indices6 = map_dfr(50001:60000, function(i) {
  
  filename = files[i]
  data = read_rds(paste("1-data/1-2-microclimate-simulations/1-2-1-3-microclimate-simulations_FINAL/", filename, sep=""))
  climates = data[["Data"]]
  parameters = data[["Parameters"]]
  index_vector = microclimate_indices(macroclimate = climates$macro, microclimate = climates$micro,
                                      time.index = climates$date)
  indices = c(parameters["site"], parameters["a"],
              parameters["v"],parameters["e"],
              index_vector)
})
write.csv(indices6, "1-data/1-3-indices/1-3-1-3-indices60k_FINAL.csv")


indices7 = map_dfr(60001:length(files), function(i) {
  
  filename = files[i]
  data = read_rds(paste("1-data/1-2-microclimate-simulations/1-2-1-3-microclimate-simulations_FINAL/", filename, sep=""))
  climates = data[["Data"]]
  parameters = data[["Parameters"]]
  index_vector = microclimate_indices(macroclimate = climates$macro, microclimate = climates$micro,
                                      time.index = climates$date)
  indices = c(parameters["site"], parameters["a"],
              parameters["v"],parameters["e"],
              index_vector)
})
write.csv(indices7, "1-data/1-3-indices/1-3-1-3-indices66k_FINAL.csv")

indices_all = rbind(indices1, indices2, indices3, indices4, indices5, indices6, indices7)

write.csv(indices_all, "1-data/1-3-indices/1-3-1-3-indices_FINAL.csv")

rm(list=ls())
library(tidyverse)

# read in index functions
source("scripts/index-functions.R")

# microclimate data
mydiv = read.csv("data/5-example-data/mydiv_plot_temp.csv")
macro = read.csv("data/5-example-data/mydiv_macro_temp.csv")

# filter Southwest station data and resample to hourly measurements
macro_sw = macro %>% filter(rep == "SW") %>%
  filter(between(as.Date(datetime), as.Date("2024-06-01"), as.Date("2024-09-30"))) %>%
  filter(!grepl(":30", datetime)) %>% #remove half hour measurements
  mutate(datetime = as_datetime(datetime)) %>%
  select(datetime, temperature)

# write a table of plots + species
mydiv_overview = mydiv %>% count(plot, myc, composition)
plotnames = mydiv_overview$plot
write.csv(mydiv_overview, "data/5-example-data/mydiv_plots_species_overview.csv")

# convert date format and remove missing data
mydiv = mydiv %>%
  mutate(datetime = as_datetime(datetime)) %>%
  filter(!is.na(Air_Temperature_o)) %>%
  select(plot,myc,datetime, Air_Temperature_o)

# calculate indices

indices_table = data.frame(plot = numeric(),
                           myc = character(),
                           mean_offset = numeric(),
                           median_offset = numeric(),
                           amplitude_offset = numeric(),
                           change_ratio = numeric(),
                           offset_maxima = numeric(),
                           offset_minima = numeric()
)

for(i in plotnames){
  
  plot = i
  plot_data = mydiv %>% filter(plot == i) %>% inner_join(macro_sw, by="datetime")
  
  #get mycorrhiza type of plot
  myc = plot_data$myc[1]
  
  #assign date column
  datetime = plot_data$datetime
  
  #assign microclimate and macroclimate data
  microclim = plot_data$Air_Temperature_o
  macroclim = plot_data$temperature
  
  indices = best_microclimate_indices(macroclimate = macroclim, microclimate = microclim, time.index = datetime)
  
  indices_table = indices_table %>% add_row( tibble_row ( plot = plot,
                                                          myc = myc,
                                                          mean_offset = indices["mean offset"],
                                                          median_offset = indices["median offset"],
                                                          amplitude_offset = indices["amplitude offset(p5-p95)"],
                                                          change_ratio = indices["change ratio"],
                                                          offset_maxima = indices["offset of maxima (p97.5)"],
                                                          offset_minima = indices["offset of minima (p2.5)"] )
  )
}


write.csv(indices_table, "data/5-example-data/mydiv_indices.csv")

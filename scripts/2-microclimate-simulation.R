## Simulate microclimates from downloaded macroclimate data

rm(list=ls())

library(tidyverse)

# load functoions to simulate microclimate
source("scripts/ave-transformation-functions.R")

# get site names
sites = read.csv("data/1-noaa/2020-noaa-sample-sites.csv")
country <- paste0(sites$country, "-", sites$code)

# create dataframe of all possible combinations of AVE parameters
parameters = expand.grid(a = seq(-10, 10, 2), v = seq(-0.5, 0.5, 0.1), e = seq(-1, 1, 0.2))
parameters$ID = 1001:(1000+nrow(parameters)) #add ID

# apply transformation functions to all sites and parameter combinations
# to simulate the microclimates
for(site in country){
  
  #read in data 
  macro = read.csv(paste("data/1-noaa/noaa-sample-data/", site, ".csv", sep=""))
  
  #rename and order columns
  macro$macro = macro$t2m
  macro = select(macro, c(date, macro, site))
  
  # creating dataframes of simulated data for each combination of parameters
  simulations <- map(1:nrow(parameters), function(i) {
    a <- parameters$a[i]
    v <- parameters$v[i]
    e <- parameters$e[i]
    
    df <- macro
    df$micro <- change_average(climate.variable = df$macro, average.offset = a)
    df$micro <- change_variance(climate.variable = df$micro, time.variable = macro$date, variance.increase = v)
    df$micro <- change_extremes(climate.variable = df$micro, heat.stabilisation = e, cold.stabilisation = e)
    
    # create a list containing parameters and data
    final <- list(Parameters = c(site = site, a = a, v = v, e = e), Data = df)
    return(final)
    
  })
  
  # writing each dataframe to separate files
  walk2(simulations, 1:nrow(parameters), ~write_rds(.x, file = paste("1-data/2-microclimate-simulations/microclimate-simulations/", site, "_a", parameters$a[.y],"_v", parameters$v[.y], "_e", parameters$e[.y], ".rds", sep="")))
  
  print(paste0("Processed ", site))

}

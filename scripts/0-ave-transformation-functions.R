# ===============================================================================================================================
# Title       : 0-ave-transformation-functions.R
# Description : Functions to simulate microclimate by modifying average, variability and extremes
# Author      : ###
# Affiliation : ###
# Contact     : ###
# Date        : 2025-04-13
# Version     : 1.0
# License     : MIT
# Notes       : Supplementary code for "A framework to quantify microclimate modulation using average, variability, and extremes"
# ===============================================================================================================================

library(tidyverse)
library(lubridate)

# function to change average
change_average <- function(climate.variable, average.offset) {
  # create new variable that will contain simulated microclimate
  microclimate <- numeric(length(climate.variable))
  # microclimate = input climate + input offset
  microclimate <- climate.variable + average.offset
  # return vector of simulated microclimate values
  return(microclimate)
}


# function to change the variance of the data
change_variance <- function(climate.variable, time.variable, variance.increase) {
  # convert input to dataframe (necessary for grouping by month)
  df <- data.frame(time.variable = time.variable, climate.variable = climate.variable)
  df <- df %>%
    # group by month
    group_by(month = lubridate::month(time.variable)) %>%
    mutate(
      microclimate =
      # ( Tmacro - mean(Tmacro) ) * (1 + v) + mean(Tmacro)
        (climate.variable - mean(climate.variable, na.rm = TRUE)) * (1 + {{ variance.increase }})
          + mean(climate.variable, na.rm = TRUE)
    )
  # return vector of simulated microclimate values
  return(df$microclimate)
}


# function to change extremes
change_extremes <- function(climate.variable, heat.stabilisation = 0, cold.stabilisation = 0) {
  # get 5th and 95th percentile (limits for extremes)
  perc.05 <- quantile(climate.variable, 0.05, na.rm = TRUE)
  perc.95 <- quantile(climate.variable, 0.95, na.rm = TRUE)

  microclimate <- numeric(length(climate.variable))

  for (i in seq_along(climate.variable)) {
    if (is.na(climate.variable[i]) == TRUE) {
      # NA handling
      microclimate[i] <- NA
    } else if (climate.variable[i] > perc.95) {
      # values above 95th percentile are modified
      microclimate[i] <- climate.variable[i] - (climate.variable[i] - perc.95) * {{ heat.stabilisation }}
    } else if (climate.variable[i] < perc.05) {
      # values below 5th percentile are modified
      microclimate[i] <- climate.variable[i] + (perc.05 - climate.variable[i]) * {{ cold.stabilisation }}
    } else {
      # other values stay the same
      microclimate[i] <- climate.variable[i]
    }
  }
  return(microclimate)
}

# function to add error
add_error <- function(climate.variable, error.sd = 0, error.autocor = 0.8) {
  
  # generate autocorrelated error series
  e <- w <- rnorm(length(climate.variable), sd = error.sd)
  for (t in 2:length(climate.variable)) e[t] <- error.autocor*e[t-1] + w[t]
  
  # combine with temperature
  microclimate <- climate.variable + e
  
  return(microclimate)
}
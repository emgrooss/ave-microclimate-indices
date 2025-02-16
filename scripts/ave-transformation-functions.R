library(tidyverse)

#function to change average
change_average <- function(climate.variable, average.offset){
  microclimate <- numeric(length(climate.variable))
  microclimate <- climate.variable +average.offset
  return(microclimate)
}


#function to change the variance of the data
change_variance <- function(climate.variable, time.variable, variance.increase){
  df = data.frame(time.variable = time.variable, climate.variable = climate.variable)
  df = df %>% 
    group_by(month = lubridate::month(time.variable)) |>
    mutate(microclimate = 
           (climate.variable-mean(climate.variable, 
                                      na.rm=TRUE))*
           (1+{{variance.increase}}) + 
           mean(climate.variable, 
                na.rm=TRUE))
  return(df$microclimate)
}


# function to change extreme stabilisation
change_extremes <- function(climate.variable, heat.stabilisation = 0, cold.stabilisation = 0){
  
  perc.05 <- quantile(climate.variable, 0.05, na.rm=TRUE)
  perc.95 <- quantile(climate.variable, 0.95, na.rm=TRUE)
  
  microclimate <- numeric(length(climate.variable))
  
  for(i in seq_along(climate.variable)){
    if(is.na(climate.variable[i]) == TRUE){
      microclimate[i] <- NA
    } else if(climate.variable[i] > perc.95){
      microclimate[i] <- climate.variable[i] - (climate.variable[i] - perc.95)* {{heat.stabilisation}}
    } else if(climate.variable[i] < perc.05){
      microclimate[i] <- climate.variable[i] + (perc.05 - climate.variable[i])* {{cold.stabilisation}}
    } else {
      microclimate[i] <- climate.variable[i]
    }
  }
  return(microclimate)
}


# function to modify extremes (based on dataframe)
## (old version but keeping it in case it will be useful)
change_extremes_df <- function(df, climate.variable, heat.stabilisation = 0, cold.stabilisation = 0) {
  
  # So the column name doesnt have to be entered as a string:
  climate.variable <- deparse(substitute(climate.variable))
  
  # Calculate mean and standard deviation
  climate.variable_mean <- mean(df[[climate.variable]])
  climate.variable_sd <- sd(df[[climate.variable]])
  
  df <- df %>%
    mutate(microclimate = case_when(
      .data[[climate.variable]] > (climate.variable_mean + 2 * climate.variable_sd) ~ .data[[climate.variable]] - (.data[[climate.variable]] - (climate.variable_mean + 2*climate.variable_sd)) * {{heat.stabilisation}},
      .data[[climate.variable]] < (climate.variable_mean - 2 * climate.variable_sd) ~ .data[[climate.variable]] + ((climate.variable_mean - 2*climate.variable_sd)- .data[[climate.variable]]) * {{cold.stabilisation}},
      TRUE ~ .data[[climate.variable]]
    ))
  return(df$microclimate)
}


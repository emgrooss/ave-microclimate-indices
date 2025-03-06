library(lubridate)
library(dplyr)


### AVERAGE INDICES ###

# mean offset
mean_offset <- function(macroclimate, microclimate){
  offset <- microclimate - macroclimate
  mean_offset <- mean(offset, na.rm=TRUE)
  return(mean_offset)
}

# median offset
median_offset <- function(macroclimate, microclimate){
  offset <- microclimate - macroclimate
  median_offset <- median(offset, na.rm=TRUE)
  return(median_offset)
}

# sum of offsets
sum_of_offsets <- function(macroclimate, microclimate){
  offset <- microclimate - macroclimate
  return(sum(offset, na.rm=TRUE))
}

#equilibrium (Gril et al. 2023)
equilibrium <- function(macroclimate, microclimate){
  mod = lm(microclimate ~ macroclimate, na.action=na.omit) #create linear model
  cf = coef(mod) # get coefficients
  intercept = unname(cf[1])
  slope = unname(cf[2])
  
  equilibrium = intercept/(1-slope)
  return(equilibrium)
}


### VARIABILITY INDICES ###

# offset of SD
sd_offset <- function(macroclimate, microclimate){
  sd_micro = sd(microclimate, na.rm=TRUE)
  sd_macro = sd(macroclimate, na.rm=TRUE)
  return(sd_micro - sd_macro)
}

# mean offset of sd of daily mean 
sd_offset_mean_daily <- function(time.index, macroclimate, microclimate){
  df <- data.frame(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate)
  
  df <- df %>% dplyr::group_by(day = lubridate::day(time.index)) %>%
    dplyr::summarize(sd_macro = sd(macroclimate, na.rm=TRUE),
                     sd_micro = sd(microclimate, na.rm=TRUE))
  
  df$sd_offset = df$sd_micro - df$sd_macro
  return(mean(df$sd_offset, na.rm=TRUE))
}

# amplitude offset
amplitude_offset <- function(macroclimate, microclimate, percentile_min = .05, percentile_max = .95){
  macro_max <- quantile(macroclimate, percentile_max, na.rm=TRUE)
  micro_max <- quantile(microclimate, percentile_max, na.rm=TRUE)
  macro_min <- quantile(macroclimate, percentile_min, na.rm=TRUE)
  micro_min <- quantile(microclimate, percentile_min, na.rm=TRUE)
  
  amplitude_macro = macro_max - macro_min
  amplitude_micro = micro_max - micro_min
  amplitude_offset = amplitude_micro - amplitude_macro
  return(unname(amplitude_offset))
}

# mean offset of daily amplitude
amplitude_offset_mean_daily <- function(time.index, macroclimate, microclimate){
  df <- data.frame(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate)
  
  df <- df %>% dplyr::group_by(day = lubridate::yday(time.index)) %>%
    dplyr::summarize(max_macro = max(macroclimate, na.rm=TRUE), min_macro = min(macroclimate, na.rm=TRUE),
                     max_micro = max(microclimate, na.rm=TRUE), min_micro = min(microclimate, na.rm=TRUE))
  
  df$daily_amplitude_macro <- df$max_macro - df$min_macro
  df$daily_amplitude_micro <- df$max_micro - df$min_micro
  df$amplitude_offset <- df$daily_amplitude_micro - df$daily_amplitude_macro
  
  return(mean(df$amplitude_offset, na.rm=TRUE)) 
}

# amplitude ratio
amplitude_ratio <- function(macroclimate, microclimate, percentile_min = .05, percentile_max = .95){
  macro_max <- quantile(macroclimate, percentile_max, na.rm=TRUE)
  micro_max <- quantile(microclimate, percentile_max, na.rm=TRUE)
  macro_min <- quantile(macroclimate, percentile_min, na.rm=TRUE)
  micro_min <- quantile(microclimate, percentile_min, na.rm=TRUE)
  
  amplitude_macro = macro_max - macro_min
  amplitude_micro = micro_max - micro_min
  
  return(unname(amplitude_micro/amplitude_macro))
}

# CV offset
CV_offset <- function(macroclimate, microclimate){
  cv_macro = mean(macroclimate, na.rm=TRUE)/sd(macroclimate, na.rm=TRUE)
  cv_micro = mean(microclimate, na.rm=TRUE)/sd(microclimate, na.rm=TRUE)
  
  return(cv_micro - cv_macro)
}

# CV offset mean daily
CV_offset_mean_daily <- function(time.index, macroclimate, microclimate){
  df <- data.frame(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate)
  
  df <- df %>% dplyr::group_by(day = lubridate::yday(time.index)) %>%
    dplyr::summarize(mean_macro = mean(macroclimate, na.rm=TRUE), sd_macro = sd(macroclimate, na.rm=TRUE),
                     mean_micro = mean(microclimate, na.rm=TRUE), sd_micro = sd(microclimate, na.rm=TRUE))
  
  df$cv_macro = df$mean_macro/df$sd_macro
  df$cv_micro = df$mean_micro/df$sd_micro
  return(mean(df$cv_micro - df$cv_macro, na.rm=TRUE))
}

# CV ratio
CV_ratio <- function(macroclimate, microclimate){
  cv_macro = mean(macroclimate, na.rm=TRUE)/sd(macroclimate, na.rm=TRUE)
  cv_micro = mean(microclimate, na.rm=TRUE)/sd(microclimate, na.rm=TRUE)
  
  return(cv_micro/cv_macro)
}

# slope (Gril et al. 2023)
slope <- function(macroclimate, microclimate){
  mod = lm(microclimate ~ macroclimate, na.action=na.omit) #create linear model
  cf = coef(mod) # get coefficients
  slope = unname(cf[2])
  
  return(slope)
}

# ratio of change between two time points in microclimate to change in macroclimate
change_ratio <- function(macroclimate, microclimate){
  
  macro = macroclimate[!is.na(macroclimate)]
  micro = microclimate[!is.na(microclimate)]
  
  if(length(macro) != length(micro)){
    print("Error: Macroclimate and microclimate do not contain the same number of observations. Please correct.")
  }
  
  else {
    ratios <- numeric(length(macroclimate) - 1)
    for (i in 2:(length(macroclimate) - 1)) {
      diff_macro <- round(macroclimate[i],2) - round(macroclimate[i - 1],2)
      diff_micro <- round(microclimate[i],2) - round(microclimate[i - 1],2)
      
      if (is.na(diff_macro)==TRUE){
        ratios[i - 1] <- NA
      } else if(diff_macro == 0) {
        ratios[i - 1] <- NA  # handle division by zero
      } else {
        ratios[i - 1] <- diff_micro / diff_macro
      }
    }
    return(median(ratios, na.rm=TRUE))
  }
}


### EXTREME INDICES ###

#### Maxima ####

# offset of maxima
offset_of_maxima <- function(macroclimate, microclimate, percentile = .95){
  macro_max <- unname(quantile(macroclimate, percentile, na.rm = TRUE)) #unname removes name (quantile) from value
  micro_max <- unname(quantile(microclimate, percentile, na.rm = TRUE))
  offset_of_maxima <- micro_max - macro_max
  return(offset_of_maxima) 
}

# mean offset of daily maxima
offset_of_maxima_mean_daily <- function(time.index, macroclimate, microclimate, percentile = 1.00){
  df <- data.frame(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate)
  
  df <- df %>% dplyr::group_by(day = lubridate::yday(time.index)) %>%
    dplyr::summarize(max_macro = unname(quantile(macroclimate, percentile, na.rm = TRUE)),
                     max_micro = unname(quantile(microclimate, percentile, na.rm = TRUE)))
  
  df$daily_max_offset <- df$max_micro - df$max_macro
  return(mean(df$daily_max_offset, na.rm=TRUE))
}

# 95th percentile of daily differences between the maxima of the microclimate and the macroclimate
p95_daily_maxima_offset <- function(time.index, macroclimate, microclimate){
  df <- data.frame(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate)
  
  df <- df %>% dplyr::group_by(day = lubridate::yday(time.index)) %>%
    dplyr::summarize(max_macro = max(macroclimate, na.rm=TRUE),
                     max_micro = max(microclimate, na.rm=TRUE))
  
  df$daily_max_offset <- df$max_micro - df$max_macro
  max_offset_p95 <- unname(quantile(df$daily_max_offset, 0.95, na.rm=TRUE))
  return(max_offset_p95)
}


#### Minima ####

# offset of minima

offset_of_minima <- function(macroclimate, microclimate, percentile = .05){
  macro_min <- unname(quantile(macroclimate, percentile, na.rm=TRUE))
  micro_min <- unname(quantile(microclimate, percentile, na.rm=TRUE))
  offset_of_minima <- micro_min - macro_min
  return(offset_of_minima)
}

# Mean offset of daily minima
offset_of_minima_mean_daily <- function(time.index, macroclimate, microclimate, percentile = .05){
  df <- data.frame(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate)
  
  df <- df %>% dplyr::group_by(day = lubridate::yday(time.index)) %>%
    dplyr::summarize(min_macro = unname(quantile(macroclimate, percentile, na.rm=TRUE)),
                     min_micro = unname(quantile(microclimate, percentile, na.rm=TRUE)))
  
  df$daily_min_offset <- df$min_micro - df$min_macro
  return(mean(df$daily_min_offset, na.rm=TRUE))
}

# 5th percentile of daily differences between the minima of the microclimate and the macroclimate
p5_daily_minima_offset <- function(time.index, macroclimate, microclimate){
  df <- data.frame(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate)
  
  df <- df %>% dplyr::group_by(day = lubridate::yday(time.index)) %>%
    dplyr::summarize(min_macro = min(macroclimate, na.rm=TRUE),
                     min_micro = min(microclimate, na.rm=TRUE))
  
  df$daily_min_offset <- df$min_micro - df$min_macro
  min_offset_p5 <- unname(quantile(df$daily_min_offset, 0.05, na.rm=TRUE))
  return(min_offset_p5)
}



### calculate all indices together ###

microclimate_indices <- function(macroclimate, microclimate, time.index){
  
  #calculate indices
  # the As
  mn_offset <- mean_offset(macroclimate = macroclimate, microclimate = microclimate)
  md_offset <- median_offset(macroclimate = macroclimate, microclimate = microclimate)
  total_mod <- sum_of_offsets(macroclimate = macroclimate, microclimate = microclimate)
  equilibrium <- equilibrium(macroclimate = macroclimate, microclimate = microclimate)
  
  # the Vs
  sd_offset <- sd_offset(macroclimate = macroclimate, microclimate = microclimate)
  sd_offset_daily <- sd_offset_mean_daily(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate)
  amplitude_offset.95 <- amplitude_offset(macroclimate = macroclimate, microclimate = microclimate, percentile_max = .95, percentile_min = .05)
  amplitude_offset.975 <- amplitude_offset(macroclimate = macroclimate, microclimate = microclimate, percentile_max = .975, percentile_min = .025)
  daily_amp <- amplitude_offset_mean_daily(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate)
  amplitude_r.95 <- amplitude_ratio(macroclimate = macroclimate, microclimate = microclimate, percentile_max = .95, percentile_min = .05)
  amplitude_r.975 <- amplitude_ratio(macroclimate = macroclimate, microclimate = microclimate, percentile_max = .975, percentile_min = .25)
  cv_offset <- CV_offset(macroclimate = macroclimate, microclimate = microclimate)
  cv_offset_daily <- CV_offset_mean_daily(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate)
  cv_r <- CV_ratio(macroclimate = macroclimate, microclimate = microclimate)
  slope <- slope(macroclimate = macroclimate, microclimate = microclimate)
  change_ratio <- change_ratio(macroclimate = macroclimate, microclimate = microclimate)
  corr <- cor(macroclimate, microclimate, use="complete.obs")
  
  #the Es
  ## Emax
  max_offset.95 <- offset_of_maxima(macroclimate = macroclimate, microclimate = microclimate, percentile = .95)
  max_offset.975 <- offset_of_maxima(macroclimate = macroclimate, microclimate = microclimate, percentile = .975)
  max_offset1.00 <- offset_of_maxima(macroclimate = macroclimate, microclimate = microclimate, percentile = 1)
  daily_max_offset.95 <- offset_of_maxima_mean_daily(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate, percentile = .95)
  daily_max_offset.975 <- offset_of_maxima_mean_daily(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate, percentile = .975)
  daily_max_offset1.00 <- offset_of_maxima_mean_daily(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate, percentile = 1)
  p95_daily_max <- p95_daily_maxima_offset(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate)
  ## Emin
  min_offset.05 <- offset_of_minima(macroclimate = macroclimate, microclimate = microclimate, percentile = .05)
  min_offset.025 <- offset_of_minima(macroclimate = macroclimate, microclimate = microclimate, percentile = .025)
  min_offset.00 <- offset_of_minima(macroclimate = macroclimate, microclimate = microclimate, percentile = 0)
  daily_min_offset.05 <- offset_of_minima_mean_daily(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate, percentile = .05)
  daily_min_offset.025 <- offset_of_minima_mean_daily(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate, percentile = .025)
  daily_min_offset.00 <- offset_of_minima_mean_daily(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate, percentile = 0)
  p5_daily_min <- p5_daily_minima_offset(time.index = time.index, macroclimate = macroclimate, microclimate = microclimate)
  
  #return named vector
  indices <- c(# the As
               "mean_offset" = mn_offset,
               "median_offset" = md_offset,
               "sum_of_offsets" = total_mod,
               "equilibrium" = equilibrium,
               # the Vs
               "sd_offset" = sd_offset,
               "sd_offset_mean_daily" = sd_offset_daily,
               "amplitude_offset.95" = amplitude_offset.95,
               "amplitude_offset.975" = amplitude_offset.975,
               "amplitude_offset_mean_daily" = daily_amp,
               "amplitude_ratio.95" = amplitude_r.95,
               "amplitude_ratio.975" = amplitude_r.975,
               "CV_offset" = cv_offset,
               "CV_offset_mean_daily" = cv_offset_daily,
               "CV_ratio" = cv_r,
               "slope" = slope,
               "change_ratio" = change_ratio,
               "correlation_micro_macro" = corr,
               # the Es
               ## Emax
               "offset_of_maxima.95" = max_offset.95,
               "offset_of_maxima.975" = max_offset.975,
               "offset_of_maxima1.00" = max_offset1.00,
               "offset_of_maxima_mean_daily.95" = daily_max_offset.95,
               "offset_of_maxima_mean_daily.975" = daily_max_offset.975,
               "offset_of_maxima_mean_daily1.00" = daily_max_offset1.00,
               "p95_daily_maxima_offset" = p95_daily_max,
               ## Emin
               "offset_of_minima.05" = min_offset.05,
               "offset_of_minima.025" = min_offset.025,
               "offset_of_minima.00" = min_offset.00,
               "offset_of_minima_mean_daily.05" = daily_min_offset.05,
               "offset_of_minima_mean_daily.025" = daily_min_offset.025,
               "offset_of_minima_mean_daily.00" = daily_min_offset.00,
               "p5_daily_minima_offset" = p5_daily_min) 

  return(indices) 
}





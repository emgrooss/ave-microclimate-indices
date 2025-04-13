# ===============================================================================================================================
# Title       : 1-2-noaa_data_import.R
# Description : Downloading data for the selected sample sites from NOAAs
# Author      : ###
# Affiliation : ###
# Contact     : ###
# Date        : 2025-04-13
# Version     : 1.0
# License     : MIT
# Notes       : Supplementary code for "A framework to quantify microclimate modulation using average, variability, and extremes"
# ===============================================================================================================================

rm(list = ls())
library(climate)
library(dplyr)

# read file that contains site codes
sites <- read.csv("data/1-noaa/2020-noaa-sample-sites.csv")
# vector of site codes
site_code <- sites$code
# country abbreviation + site code -> names for the files to download to
country <- paste0(sites$country, "-", sites$code)

# download data by site
macro.T <- NULL
for (i in 1:length(site_code)) {
  dtemp <- meteo_noaa_hourly(station = site_code[i], year = 2020)
  dtemp$site <- country[i]
  macro.T <- rbind(macro.T, dtemp)
}

macro <- select(macro.T, c(date, t2m, site))

# write data to files
for (i in country) {
  df <- macro %>% filter(site == i)
  write.csv(df, file = paste("data/1-noaa/noaa-sample-data/", i, ".csv", sep = ""))
  rm(df)
}

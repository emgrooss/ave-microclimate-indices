# ===============================================================================================================================
# Title       : 1-1-noaa_site_sample.R
# Description : Selection and sampling of suitable NOAA sites
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
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(climate)
library(R.utils)

#--selection of sites with hourly temperature data in 2020---------------------------------

# read file downloaded from NOAA website that contains history of stations
df <- read_delim("data/1-noaa/isd-history.csv",
  delim = ";", escape_double = FALSE,
  trim_ws = TRUE
)

# select stations that recorded data in 2020 with elevation between 0 and 1000 m
dd <- df |>
  filter(`ELEV(M)` < 1000 & `ELEV(M)` > 0) |>
  filter(BEGIN < 20200101) |>
  filter(END > 20201231)

# create dataframe of sites available in 2020
df.2020 <- readxl::read_xlsx("data/1-noaa/2020-available-noaa.xlsx")
ddd <- dd |>
  filter(paste0(USAF, "-", WBAN, "-2020.gz") %in% df.2020$`008415-99999-2020.gz`)

coordinates_sf <- st_as_sf(ddd, coords = c("LON", "LAT"), crs = 4326, agr = "constant")

# draw map to see available stations
world <- ne_countries(scale = "medium", returnclass = "sf")
coordinates_sf <- st_as_sf(dd,
  coords = c("LON", "LAT"),
  crs = 4326, agr = "constant"
)

noaa_map <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = coordinates_sf, size = 4, col = "red") +
  theme_bw()
noaa_map

# select stations with hourly data available
# and write as files in folder "potential stations"

for (i in 4000:nrow(ddd)) {
  print(i)

  tryCatch(
    expr = {
      withTimeout(
        expr = {
          d.temp <- meteo_noaa_hourly(station = paste0(ddd$USAF[i], "-", ddd$WBAN[i]), year = 2020)
          if (nrow(d.temp) > 0) {
            if (length(unique(d.temp$hour)) == 24) {
              print(paste0(ddd$USAF[i], "-", ddd$WBAN[i]))
              print(head(d.temp))
              write_csv(
                x = data.frame(
                  code = paste0(ddd$USAF[i], "-", ddd$WBAN[i]),
                  country = ddd$CTRY[i]
                ),
                file = paste0(
                  "data/1-1-noaa/potential-stations/",
                  paste0(ddd$USAF[i], "-", ddd$WBAN[i]),
                  ".csv"
                )
              )
            }
          }
        }, timeout = 30
      )
    }, TimeoutException = function(ex) {
      cat("Timeout\n")
    }, error = function(e) {
      cat("Error\n")
    }
  )
}

# get names of potential stations
sites <- list.files("data/1-noaa/potential-stations/") |>
  str_remove_all(".csv")
sites <- ddd |>
  filter(paste0(USAF, "-", WBAN) %in% sites)

# map of potential stations
coordinates_sf <- st_as_sf(sites, coords = c("LON", "LAT"), crs = 4326, agr = "constant")
noaa_map <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = coordinates_sf, size = 4, col = "red") +
  theme_bw()
noaa_map

write_csv(sites, "data/1-noaa/2020-available-noaa.csv")



#--sample 20 sites across globe------------------------------------------------------------

library(spsurvey)

sites <- read_csv("data/1-noaa/2020-available-noaa.csv")

sites_sf <- st_as_sf(sites, coords = c("LON", "LAT"), crs = 4326)

# select 20 sites evenly distributed across globe using spsurvey GRTS sampling
set.seed(37073)
sample <- spsurvey::grts(
  sframe = sites_sf,
  n_base = 20,
  DesignID = "WorldSample",
  projcrs_check = FALSE
)

# get sampled site info
selected_sites <- as.data.frame(sample$sites_base)
selected_sites <- selected_sites %>%
  select(USAF, WBAN, `STATION NAME`, CTRY, LON = X, LAT = Y)

write_csv(selected_sites, "data/1-noaa/2020-noaa-sample.csv") # this file contains full meta information about the sampled sites


# write file with only station code and country
selected_sites <- read_csv("data/1-noaa/2020-noaa-sample.csv")

sites_sample <- data.frame(
  code = character(),
  country = character()
)
for (i in 1:nrow(selected_sites)) {
  filename <- paste0(selected_sites[i, "USAF"], "-", selected_sites[i, "WBAN"])
  country <- selected_sites[i, "CTRY"]
  sites_sample[i, ] <- c(code = filename, country = country)
}
write_csv(sites_sample, "data/1-noaa/2020-noaa-sample-sites.csv")

# make plot of sample sites
world <- ne_countries(scale = "medium", returnclass = "sf")
coordinates_sample_sf <- st_as_sf(selected_sites, coords = c("LON", "LAT"), crs = 4326, agr = "constant")
sample_map <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = coordinates_sample_sf, size = 3, col = "black", fill = "red", shape = 21) +
  theme_bw()
sample_map
ggsave("plots/coord_map_sample.png", sample_map, width = 3000, height = 1500, unit = "px")
## Test the simulation by calculating its outputs

rm(list = ls())

library(tidyverse)

## Creating dataframes for each parameter with the others at 0

files <- list.files("data/2-microclimate-simulations/microclimate-simulations/")

# a varies, v = 0 and e = 0

simresults_a <- data.frame(
  a = numeric(), v = numeric(), e = numeric(),
  d_a = numeric(), d_v = numeric(), d_emax = numeric(), d_emin = numeric()
)
for (i in files) {
  data <- read_rds(paste("data/2-microclimate-simulations/microclimate-simulations/", i, sep = ""))
  climates <- data[["Data"]]
  parameters <- data[["Parameters"]]
  a <- as.numeric(parameters["a"])
  v <- as.numeric(parameters["v"])
  e <- as.numeric(parameters["e"])

  if (e == 0 && v == 0) {
    d_a <- mean(climates$micro, na.rm = TRUE) - mean(climates$macro, na.rm = TRUE)

    d_v <- climates %>%
      dplyr::group_by(month = lubridate::yday(date)) %>%
      dplyr::summarize(v_obs_monthly = (sd(micro, na.rm = TRUE) - sd(macro, na.rm = TRUE)) / sd(macro, na.rm = TRUE)) %>%
      summarize(v_obs = mean(v_obs_monthly)) %>%
      as.numeric()

    p95 <- unname(quantile(climates$macro, 0.95, na.rm = TRUE))
    d_emax <- climates %>%
      filter(macro > p95) %>%
      mutate(d_emax = (micro - macro) / (p95 - macro)) %>%
      summarise(mean(d_emax)) %>%
      as.numeric()

    p5 <- unname(quantile(climates$macro, 0.05, na.rm = TRUE))
    d_emin <- climates %>%
      filter(macro < p5) %>%
      mutate(d_emin = (micro - macro) / (p5 - macro)) %>%
      summarise(mean(d_emin)) %>%
      as.numeric()

    simresults_a <- simresults_a %>% add_row(a = a, v = v, e = e, d_a = d_a, d_v = d_v, d_emax = d_emax, d_emin = d_emin)
  }
}

write.csv(simresults_a, "data/2-microclimate-simulations/simulation-test/simresults_a.csv")

# v varies, a = 0 and e = 0

simresults_v <- data.frame(
  a = numeric(), v = numeric(), e = numeric(),
  d_a = numeric(), d_v = numeric(), d_emax = numeric(), d_emin = numeric()
)
for (i in files) {
  data <- read_rds(paste("data/2-microclimate-simulations/microclimate-simulations/", i, sep = ""))
  climates <- data[["Data"]]
  parameters <- data[["Parameters"]]
  a <- as.numeric(parameters["a"])
  v <- as.numeric(parameters["v"])
  e <- as.numeric(parameters["e"])

  if (a == 0 && e == 0) {
    d_a <- mean(climates$micro, na.rm = TRUE) - mean(climates$macro, na.rm = TRUE)

    d_v <- climates %>%
      dplyr::group_by(month = lubridate::yday(date)) %>%
      dplyr::summarize(v_obs_monthly = (sd(micro, na.rm = TRUE) - sd(macro, na.rm = TRUE)) / sd(macro, na.rm = TRUE)) %>%
      summarize(v_obs = mean(v_obs_monthly)) %>%
      as.numeric()

    p95 <- unname(quantile(climates$macro, 0.95, na.rm = TRUE))
    d_emax <- climates %>%
      filter(macro > p95) %>%
      mutate(d_emax = (micro - macro) / (p95 - macro)) %>%
      summarise(mean(d_emax)) %>%
      as.numeric()

    p5 <- unname(quantile(climates$macro, 0.05, na.rm = TRUE))
    d_emin <- climates %>%
      filter(macro < p5) %>%
      mutate(d_emin = (micro - macro) / (p5 - macro)) %>%
      summarise(mean(d_emin)) %>%
      as.numeric()

    simresults_v <- simresults_v %>% add_row(a = a, v = v, e = e, d_a = d_a, d_v = d_v, d_emax = d_emax, d_emin = d_emin)
  }
}

write.csv(simresults_v, "data/2-microclimate-simulations/simulation-test/simresults_v.csv")

# e varies, a = 0 and v = 0

simresults_e <- data.frame(
  a = numeric(), v = numeric(), e = numeric(),
  d_a = numeric(), d_v = numeric(), d_emax = numeric(), d_emin = numeric()
)
for (i in files) {
  data <- read_rds(paste("data/2-microclimate-simulations/microclimate-simulations/", i, sep = ""))
  climates <- data[["Data"]]
  parameters <- data[["Parameters"]]
  a <- as.numeric(parameters["a"])
  v <- as.numeric(parameters["v"])
  e <- as.numeric(parameters["e"])

  if (a == 0 && v == 0) {
    d_a <- mean(climates$micro, na.rm = TRUE) - mean(climates$macro, na.rm = TRUE)

    d_v <- climates %>%
      dplyr::group_by(month = lubridate::yday(date)) %>%
      dplyr::summarize(v_obs_monthly = (sd(micro, na.rm = TRUE) - sd(macro, na.rm = TRUE)) / sd(macro, na.rm = TRUE)) %>%
      summarize(v_obs = mean(v_obs_monthly)) %>%
      as.numeric()

    p95 <- unname(quantile(climates$macro, 0.95, na.rm = TRUE))
    d_emax <- climates %>%
      filter(macro > p95) %>%
      mutate(d_emax = (micro - macro) / (p95 - macro)) %>%
      summarise(mean(d_emax)) %>%
      as.numeric()

    p5 <- unname(quantile(climates$macro, 0.05, na.rm = TRUE))
    d_emin <- climates %>%
      filter(macro < p5) %>%
      mutate(d_emin = (micro - macro) / (p5 - macro)) %>%
      summarise(mean(d_emin)) %>%
      as.numeric()

    simresults_e <- simresults_e %>% add_row(a = a, v = v, e = e, d_a = d_a, d_v = d_v, d_emax = d_emax, d_emin = d_emin)
  }
}

write.csv(simresults_e, "data/2-microclimate-simulations/simulation-test/simresults_e.csv")

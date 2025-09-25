# ===============================================================================================================================
# Title       : 5-1-index_quality.R
# Description : Calculate index quality metrics for indices from microclimate simulations
# Author      : ###
# Affiliation : ###
# Contact     : ###
# Date        : 2025-09-24
# Version     : 1.0
# License     : MIT
# Notes       : Supplementary code for "Average, Variability, and Extremes: A framework to quantify microclimate temperature modulation"
# ===============================================================================================================================

rm(list = ls())

library(lmerTest)
library(tidyverse)
library(performance)
library(ggcorrplot)

indices <- read.csv("data/4-indices/indices.csv")

# z standardize all columns, excluding col 1 and 2 (index number and site)
indices[sapply(indices, is.infinite)] <- NA # turn infs into NA
indices_std <- indices
indices_std[, -c(1:2)] <- as.data.frame(lapply(indices[, -c(1:2)], function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)))

# list of index names: column names without the first 5 columns (index, site, a, v, e, err)
index_names <- colnames(indices_std[, -c(1:6)])


## variance partitioning
# calculations based on https://lytarhan.rbind.io/post/variancepartitioning/

index_variances <- data.frame(
  index = character(),
  a_marg_r2 = numeric(),
  v_marg_r2 = numeric(),
  e_marg_r2 = numeric(),
  err_marg_r2 = numeric(),
  ave_marg_r2 = numeric(),
  ave_site_r2 = numeric()
)

for (i in 1:length(index_names)) {
  index <- index_names[i]

  # linear model a
  formula_a <- reformulate("a + (1 | site)", response = index) # formula for linear model
  model_a <- lmer(formula_a, data = indices_std)
  # calculate r²s for model quality/site effect
  a_r2 <- performance::r2_nakagawa(model_a, tolerance = 0)
  a_marg_r2 <- a_r2$R2_marginal

  # linear model v
  formula_v <- reformulate("v + (1 | site)", response = index) # formula for linear model
  model_v <- lmer(formula_v, data = indices_std)
  # calculate r²s for model quality/site effect
  v_r2 <- performance::r2_nakagawa(model_v, tolerance = 0)
  v_marg_r2 <- v_r2$R2_marginal

  # linear model e
  formula_e <- reformulate("e + (1 | site)", response = index) # formula for linear model
  model_e <- lmer(formula_e, data = indices_std)
  # calculate r²s for model quality/site effect
  e_r2 <- performance::r2_nakagawa(model_e, tolerance = 0)
  e_marg_r2 <- e_r2$R2_marginal
  
  # linear model err
  formula_err <- reformulate("err + (1 | site)", response = index) # formula for linear model
  model_err <- lmer(formula_err, data = indices_std)
  # calculate r²s for model quality/site effect
  err_r2 <- performance::r2_nakagawa(model_err, tolerance = 0)
  err_marg_r2 <- err_r2$R2_marginal

  # linear model a+v+e+err
  formula_ave <- reformulate("a + v + e + err + (1 | site)", response = index) # formula for linear model
  model_ave <- lmer(formula_ave, data = indices_std)

  # calculate r²s for model quality/site effect
  ave_r2 <- performance::r2_nakagawa(model_ave, tolerance = 0)
  ave_marg_r2 <- ave_r2$R2_marginal
  ave_conditional_r2 <- ave_r2$R2_conditional
  ave_site_r2 <- ave_conditional_r2 - ave_marginal_r2


  index_variances[i, ] <- c(
    as.character(index),
    round(a_marg_r2, 5), round(v_marg_r2, 5), round(e_marg_r2, 5),
    round(err_marg_r2, 5), round(ave_marg_r2, 5), round(ave_site_r2, 5)
  )
}

write.csv(index_variances, file = "data/5-index-performance/index_performance.csv", row.names = FALSE)

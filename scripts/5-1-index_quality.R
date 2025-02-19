rm(list = ls())

library(lmerTest)
library(tidyverse)
library(performance)

indices = read.csv("data/3-indices/indices.csv")

# z standardize all columns, excluding col 1 and 2 (index number and site)
indices[sapply(indices, is.infinite)] <- NA # turn infs into NA
indices_std = indices
indices_std[,-c(1:2)] = as.data.frame(lapply(indices[,-c(1:2)], function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)))

#list of index names: column names without the first 5 columns (index, site, a, v, e)
index_names = colnames(indices_std[,-c(1:5)])


## variance partitioning
#calculations based on https://lytarhan.rbind.io/post/variancepartitioning/

index_variances = data.frame(index = character(),
                             a_marg_r2 = numeric(),
                             v_marg_r2 = numeric(),
                             e_marg_r2 = numeric(),
                             a_r2_unique = numeric(),
                             v_r2_unique = numeric(),
                             e_r2_unique = numeric(), 
                             av_marg_r2 = numeric(), 
                             ae_marg_r2 = numeric(), 
                             ve_marg_r2 = numeric(), 
                             ave_marg_r2 = numeric(),
                             ave_site_r2 = numeric()
                               )

for(i in 1:length(index_names)){
  
  index = index_names[i]
  
  #linear model a
  formula_a = reformulate("a + (1 | site)", response = index) #formula for linear model
  model_a = lmer(formula_a, data = indices_std)
  
  #calculate r²s for model quality/site effect
  a_r2 = performance::r2_nakagawa(model_a, tolerance = 0)
  a_marg_r2 = a_r2$R2_marginal
  
  
  #linear model v
  formula_v = reformulate("v + (1 | site)", response = index) #formula for linear model
  model_v = lmer(formula_v, data = indices_std)

  #calculate r²s for model quality/site effect
  v_r2 = performance::r2_nakagawa(model_v, tolerance = 0)
  v_marg_r2 = v_r2$R2_marginal
  
  
  #linear model e
  formula_e = reformulate("e + (1 | site)", response = index) #formula for linear model
  model_e = lmer(formula_e, data = indices_std)
  
  
  #calculate r²s for model quality/site effect
  e_r2 = performance::r2_nakagawa(model_e, tolerance = 0)
  e_marg_r2 = e_r2$R2_marginal
  
  
  #linear model a+v
  formula_av = reformulate("a+v + (1 | site)", response = index) #formula for linear model
  model_av = lmer(formula_av, data = indices_std)
  
  #calculate r²s for model quality/site effect
  av_r2 = performance::r2_nakagawa(model_av, tolerance = 0)
  av_marg_r2 = av_r2$R2_marginal
  
  
  #linear model a+e
  formula_ae = reformulate("a+e + (1 | site)", response = index) #formula for linear model
  model_ae = lmer(formula_ae, data = indices_std)
  
  #calculate r²s for model quality/site effect
  ae_r2 = performance::r2_nakagawa(model_ae, tolerance = 0)
  ae_marg_r2 = ae_r2$R2_marginal
  
  
  #linear model v+e
  formula_ve = reformulate("v+e + (1 | site)", response = index) #formula for linear model
  model_ve = lmer(formula_ve, data = indices_std)
  
  #calculate r²s for model quality/site effect
  ve_r2 = performance::r2_nakagawa(model_ve, tolerance = 0)
  ve_marg_r2 = ve_r2$R2_marginal
  
  
  #linear model a+v+e
  formula_ave = reformulate("a+v+e + (1 | site)", response = index) #formula for linear model
  model_ave = lmer(formula_ave, data = indices_std)
  
  #calculate r²s for model quality/site effect
  ave_r2 = performance::r2_nakagawa(model_ave, tolerance = 0)
  ave_marg_r2 = ave_r2$R2_marginal
  ave_conditional_r2 = ave_r2$R2_conditional
  ave_marginal_r2 = ave_r2$R2_marginal
  ave_site_r2 = ave_conditional_r2 - ave_marginal_r2
  
  #unique variance for each of the predictors 
  a_r2_unique = ave_marg_r2 - ve_marg_r2
  v_r2_unique = ave_marg_r2 - ae_marg_r2
  e_r2_unique = ave_marg_r2 - av_marg_r2
  
  
  index_variances[i,] = c(as.character(index),
                          round(a_marg_r2,5), round(v_marg_r2,5), round(e_marg_r2,5),
                          round(a_r2_unique,5), round(v_r2_unique,5), round(e_r2_unique,5),
                          round(av_marg_r2,5), round(ae_marg_r2,5), round(ve_marg_r2,5),
                          round(ave_marg_r2,5),
                          round(ave_site_r2,5))
  
}

write.csv(index_variances, file="data/4-index-performance/index_performance_var_partitioned.csv", row.names = FALSE)


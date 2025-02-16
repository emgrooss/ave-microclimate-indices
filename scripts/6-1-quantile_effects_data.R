library(tidyverse)
library(lmerTest)
library(performance)

source("2-code/2-4-1-2-index-functions.R")

files = list.files("1-data/1-2-microclimate-simulations/1-2-1-3-microclimate-simulations_FINAL/")


### OFFSET OF MAXIMA 

offset_of_maxima_q = data.frame(site = character(),
                                a = numeric(),
                                v = numeric(),
                                e = numeric(),
                                p92.5 = numeric(),
                                p93.75 = numeric(),
                                p95 = numeric(),
                                p96.25 = numeric(),
                                p97.5 = numeric(),
                                p98.75 = numeric(),
                                p100 = numeric())
  
  
for(filename in files){
  
  data = read_rds(paste("1-data/1-2-microclimate-simulations/1-2-1-3-microclimate-simulations_FINAL/", filename, sep=""))
  climates = data[["Data"]]
  parameters = data[["Parameters"]]
  
  p92.5 = offset_of_maxima(macroclimate = climates$macro, microclimate = climates$micro, percentile = .925)
  p93.75 = offset_of_maxima(macroclimate = climates$macro, microclimate = climates$micro, percentile = .9375)
  p95 = offset_of_maxima(macroclimate = climates$macro, microclimate = climates$micro, percentile = .95)
  p96.25 = offset_of_maxima(macroclimate = climates$macro, microclimate = climates$micro, percentile = .9625)
  p97.5 = offset_of_maxima(macroclimate = climates$macro, microclimate = climates$micro, percentile = .975)
  p98.75 = offset_of_maxima(macroclimate = climates$macro, microclimate = climates$micro, percentile = .9875)
  p100 = offset_of_maxima(macroclimate = climates$macro, microclimate = climates$micro, percentile = 1)
  
  offset_of_maxima_q = offset_of_maxima_q %>% add_row(site = parameters["site"], a = as.numeric(parameters["a"]),
                                                      v = as.numeric(parameters["v"]), e = as.numeric(parameters["e"]),
                                                      p92.5 = p92.5, p93.75 = p93.75, p95 = p95, p96.25 = p96.25,
                                                      p97.5 = p97.5, p98.75 = p98.75, p100 = p100)
} 

write.csv(offset_of_maxima_q, "1-data/1-4-index-performance/1-4-3-1-quantiles_FINAL/offset_of_maxima_quantiles.csv")


offset_of_maxima_q = read.csv("1-data/1-4-index-performance/1-4-3-1-quantiles_FINAL/offset_of_maxima_quantiles.csv")

# z standardize all columns, excluding col 1 and 2 (index and site)
offset_of_maxima_q_std = offset_of_maxima_q
offset_of_maxima_q_std[,-c(1:2)] = as.data.frame(lapply(offset_of_maxima_q[,-c(1:2)], function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)))
#offset_of_maxima_q_std = drop_na(offset_of_maxima_q_std) # remove possible NAs

#list of index names: column names without the first 5 columns (index, site, a, v, e)
index_names = colnames(offset_of_maxima_q_std[,-c(1:5)])

#write dataframe of lm results 
max_offset_performance = data.frame(index = character(),
                                    a_slope = numeric(),
                                    a_marg_r2 = numeric(),
                                    v_slope = numeric(), 
                                    v_marg_r2 = numeric(),
                                    e_slope = numeric(),
                                    e_marg_r2 = numeric(),
                                    a_r2_unique = numeric(),
                                    v_r2_unique = numeric(),
                                    e_r2_unique = numeric(),
                                    ave_site_r2 = numeric())

for(i in 1:length(index_names)){
  
  index = index_names[i]
  
  ## individual models ##
  
  #linear model a
  formula_a = reformulate("a + (1 | site)", response = index) #formula for linear model
  model_a = lmer(formula_a, data = filter(offset_of_maxima_q_std, v==0 & e==0))
  
  #calculate slopes for effect size
  coefficients_a = coef(summary(model_a))
  a_slope = coefficients_a["a","Estimate"]
  
  #calculate r²s for model quality/site effect
  a_r2 = performance::r2_nakagawa(model_a, tolerance = 0)
  a_marg_r2 = a_r2$R2_marginal
  
  
  #linear model v
  formula_v = reformulate("v + (1 | site)", response = index) #formula for linear model
  model_v = lmer(formula_v, data = filter(offset_of_maxima_q_std, a==0 & e==0))
  
  #calculate slopes for effect size
  coefficients_v = coef(summary(model_v))
  v_slope = coefficients_v["v","Estimate"]
  
  #calculate r²s for model quality/site effect
  v_r2 = performance::r2_nakagawa(model_v, tolerance = 0)
  v_marg_r2 = v_r2$R2_marginal
  
  
  #linear model e
  formula_e = reformulate("e + (1 | site)", response = index) #formula for linear model
  model_e = lmer(formula_e, data = filter(offset_of_maxima_q_std, a==0 & v==0))
  
  #calculate slopes for effect size
  coefficients_e = coef(summary(model_e))
  e_slope = coefficients_e["e","Estimate"]
  
  #calculate r²s for model quality/site effect
  e_r2 = performance::r2_nakagawa(model_e, tolerance = 0)
  e_marg_r2 = e_r2$R2_marginal
  
  
  ## unique variances ##
  
  #linear model a+v
  formula_av = reformulate("a+v + (1 | site)", response = index) #formula for linear model
  model_av = lmer(formula_av, data = offset_of_maxima_q_std)
  
  #calculate r²s for model quality/site effect
  av_r2 = performance::r2_nakagawa(model_av, tolerance = 0)
  av_marg_r2 = av_r2$R2_marginal
  
  
  #linear model a+e
  formula_ae = reformulate("a+e + (1 | site)", response = index) #formula for linear model
  model_ae = lmer(formula_ae, data = offset_of_maxima_q_std)
  
  #calculate r²s for model quality/site effect
  ae_r2 = performance::r2_nakagawa(model_ae, tolerance = 0)
  ae_marg_r2 = ae_r2$R2_marginal
  
  
  #linear model v+e
  formula_ve = reformulate("v+e + (1 | site)", response = index) #formula for linear model
  model_ve = lmer(formula_ve, data = offset_of_maxima_q_std)
  
  #calculate r²s for model quality/site effect
  ve_r2 = performance::r2_nakagawa(model_ve, tolerance = 0)
  ve_marg_r2 = ve_r2$R2_marginal
  
  
  #linear model a+v+e
  formula_ave = reformulate("a+v+e + (1 | site)", response = index) #formula for linear model
  model_ave = lmer(formula_ave, data = offset_of_maxima_q_std)
  
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
  
  
  max_offset_performance[i,] = c(as.character(index), a_slope, a_marg_r2, v_slope, v_marg_r2, e_slope, e_marg_r2,
                            a_r2_unique, v_r2_unique, e_r2_unique, ave_site_r2)
  
}

write.csv(max_offset_performance, "1-data/1-4-index-performance/1-4-3-2-quantiles_r2/offset_of_maxima_quantiles_lm.csv")



### SPECIFIC OFFSET OF MAXIMA

specific_offset_of_maxima_q = data.frame(site = character(),
                                a = numeric(),
                                v = numeric(),
                                e = numeric(),
                                p92.5 = numeric(),
                                p93.75 = numeric(),
                                p95 = numeric(),
                                p96.25 = numeric(),
                                p97.5 = numeric(),
                                p98.75 = numeric(),
                                p100 = numeric())


for(filename in files){
  
  data = read_rds(paste("1-data/1-2-microclimate-simulations/1-2-1-3-microclimate-simulations_FINAL/", filename, sep=""))
  climates = data[["Data"]]
  parameters = data[["Parameters"]]
  
  p92.5 = specific_offset_of_maxima(macroclimate = climates$macro, microclimate = climates$micro, percentile = .925)
  p93.75 = specific_offset_of_maxima(macroclimate = climates$macro, microclimate = climates$micro, percentile = .9375)
  p95 = specific_offset_of_maxima(macroclimate = climates$macro, microclimate = climates$micro, percentile = .95)
  p96.25 = specific_offset_of_maxima(macroclimate = climates$macro, microclimate = climates$micro, percentile = .9625)
  p97.5 = specific_offset_of_maxima(macroclimate = climates$macro, microclimate = climates$micro, percentile = .975)
  p98.75 = specific_offset_of_maxima(macroclimate = climates$macro, microclimate = climates$micro, percentile = .9875)
  p100 = specific_offset_of_maxima(macroclimate = climates$macro, microclimate = climates$micro, percentile = 1)
  
  specific_offset_of_maxima_q = specific_offset_of_maxima_q %>% add_row(site = parameters["site"], a = as.numeric(parameters["a"]),
                                                      v = as.numeric(parameters["v"]), e = as.numeric(parameters["e"]),
                                                      p92.5 = p92.5, p93.75 = p93.75, p95 = p95, p96.25 = p96.25,
                                                      p97.5 = p97.5, p98.75 = p98.75, p100 = p100)
} 

write.csv(specific_offset_of_maxima_q, "1-data/1-4-index-performance/1-4-3-1-quantiles_FINAL/specific_offset_of_maxima_quantiles.csv")


specific_offset_of_maxima_q = read.csv("1-data/1-4-index-performance/1-4-3-1-quantiles_FINAL/specific_offset_of_maxima_quantiles.csv")

# z standardize all columns, excluding col 1 and 2 (index and site)
specific_offset_of_maxima_q_std = specific_offset_of_maxima_q
specific_offset_of_maxima_q_std[,-c(1:2)] = as.data.frame(lapply(specific_offset_of_maxima_q[,-c(1:2)], function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)))
#specific_offset_of_maxima_q_std = drop_na(specific_offset_of_maxima_q_std) # remove possible NAs

#list of index names: column names without the first 5 columns (index, site, a, v, e)
index_names = colnames(specific_offset_of_maxima_q_std[,-c(1:5)])

#write dataframe of lm results 
spec_max_offset_performance = data.frame(index = character(),
                                         a_slope = numeric(),
                                         a_marg_r2 = numeric(),
                                         v_slope = numeric(), 
                                         v_marg_r2 = numeric(),
                                         e_slope = numeric(),
                                         e_marg_r2 = numeric(),
                                         a_r2_unique = numeric(),
                                         v_r2_unique = numeric(),
                                         e_r2_unique = numeric(),
                                         ave_site_r2 = numeric())

for(i in 1:length(index_names)){
  
  
  index = index_names[i]
  
  ## individual models ##
  
  #linear model a
  formula_a = reformulate("a + (1 | site)", response = index) #formula for linear model
  model_a = lmer(formula_a, data = filter(specific_offset_of_maxima_q_std, v==0 & e==0))
  
  #calculate slopes for effect size
  coefficients_a = coef(summary(model_a))
  a_slope = coefficients_a["a","Estimate"]
  
  #calculate r²s for model quality/site effect
  a_r2 = performance::r2_nakagawa(model_a, tolerance = 0)
  a_marg_r2 = a_r2$R2_marginal
  
  
  #linear model v
  formula_v = reformulate("v + (1 | site)", response = index) #formula for linear model
  model_v = lmer(formula_v, data = filter(specific_offset_of_maxima_q_std, a==0 & e==0))
  
  #calculate slopes for effect size
  coefficients_v = coef(summary(model_v))
  v_slope = coefficients_v["v","Estimate"]
  
  #calculate r²s for model quality/site effect
  v_r2 = performance::r2_nakagawa(model_v, tolerance = 0)
  v_marg_r2 = v_r2$R2_marginal
  
  
  #linear model e
  formula_e = reformulate("e + (1 | site)", response = index) #formula for linear model
  model_e = lmer(formula_e, data = filter(specific_offset_of_maxima_q_std, a==0 & v==0))
  
  #calculate slopes for effect size
  coefficients_e = coef(summary(model_e))
  e_slope = coefficients_e["e","Estimate"]
  
  #calculate r²s for model quality/site effect
  e_r2 = performance::r2_nakagawa(model_e, tolerance = 0)
  e_marg_r2 = e_r2$R2_marginal
  
  
  ## unique variances ##
  
  #linear model a+v
  formula_av = reformulate("a+v + (1 | site)", response = index) #formula for linear model
  model_av = lmer(formula_av, data = specific_offset_of_maxima_q_std)
  
  #calculate r²s for model quality/site effect
  av_r2 = performance::r2_nakagawa(model_av, tolerance = 0)
  av_marg_r2 = av_r2$R2_marginal
  
  
  #linear model a+e
  formula_ae = reformulate("a+e + (1 | site)", response = index) #formula for linear model
  model_ae = lmer(formula_ae, data = specific_offset_of_maxima_q_std)
  
  #calculate r²s for model quality/site effect
  ae_r2 = performance::r2_nakagawa(model_ae, tolerance = 0)
  ae_marg_r2 = ae_r2$R2_marginal
  
  
  #linear model v+e
  formula_ve = reformulate("v+e + (1 | site)", response = index) #formula for linear model
  model_ve = lmer(formula_ve, data = specific_offset_of_maxima_q_std)
  
  #calculate r²s for model quality/site effect
  ve_r2 = performance::r2_nakagawa(model_ve, tolerance = 0)
  ve_marg_r2 = ve_r2$R2_marginal
  
  
  #linear model a+v+e
  formula_ave = reformulate("a+v+e + (1 | site)", response = index) #formula for linear model
  model_ave = lmer(formula_ave, data = specific_offset_of_maxima_q_std)
  
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
  
  
  spec_max_offset_performance[i,] = c(as.character(index), a_slope, a_marg_r2, v_slope, v_marg_r2, e_slope, e_marg_r2,
                                 a_r2_unique, v_r2_unique, e_r2_unique, ave_site_r2)
  
}

write.csv(spec_max_offset_performance, "1-data/1-4-index-performance/1-4-3-2-quantiles_r2/specific_offset_of_maxima_quantiles_lm.csv")



### OFFSET OF MINIMA


offset_of_minima_q = data.frame(site = character(),
                                a = numeric(),
                                v = numeric(),
                                e = numeric(),
                                p7.5 = numeric(),
                                p6.25= numeric(),
                                p5= numeric(),
                                p3.75 = numeric(),
                                p2.5 = numeric(),
                                p1.25 = numeric(),
                                p0 = numeric())


for(filename in files){
  
  data = read_rds(paste("1-data/1-2-microclimate-simulations/1-2-1-3-microclimate-simulations_FINAL/", filename, sep=""))
  climates = data[["Data"]]
  parameters = data[["Parameters"]]
  
  p7.5 = offset_of_minima(macroclimate = climates$macro, microclimate = climates$micro, percentile = 0.075)
  p6.25 = offset_of_minima(macroclimate = climates$macro, microclimate = climates$micro, percentile = 0.0625)
  p5 = offset_of_minima(macroclimate = climates$macro, microclimate = climates$micro, percentile = 0.05)
  p3.75 = offset_of_minima(macroclimate = climates$macro, microclimate = climates$micro, percentile = 0.0375)
  p2.5 = offset_of_minima(macroclimate = climates$macro, microclimate = climates$micro, percentile = 0.025)
  p1.25 = offset_of_minima(macroclimate = climates$macro, microclimate = climates$micro, percentile = 0.0125)
  p0 = offset_of_minima(macroclimate = climates$macro, microclimate = climates$micro, percentile = 0)
  
  
  offset_of_minima_q = offset_of_minima_q %>% add_row(site = parameters["site"], a = as.numeric(parameters["a"]),
                                                      v = as.numeric(parameters["v"]), e = as.numeric(parameters["e"]),
                                                      p7.5 = p7.5, p6.25 = p6.25, p5 = p5, p3.75 = p3.75,
                                                      p2.5 = p2.5, p1.25 = p1.25, p0 = p0)
} 

write.csv(offset_of_minima_q, "1-data/1-4-index-performance/1-4-3-1-quantiles_FINAL/offset_of_minima_quantiles.csv")


offset_of_minima_q = read.csv("1-data/1-4-index-performance/1-4-3-1-quantiles_FINAL/offset_of_minima_quantiles.csv")

# z standardize all columns, excluding col 1 and 2 (index and site)
offset_of_minima_q_std = offset_of_minima_q
offset_of_minima_q_std[,-c(1:2)] = as.data.frame(lapply(offset_of_minima_q[,-c(1:2)], function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)))
#offset_of_minima_q_std = drop_na(offset_of_minima_q_std) # remove possible NAs

#list of index names: column names without the first 5 columns (index, site, a, v, e)
index_names = colnames(offset_of_minima_q_std[,-c(1:5)])

#write dataframe of lm results 
min_offset_performance = data.frame(index = character(),
                                    a_slope = numeric(),
                                    a_marg_r2 = numeric(),
                                    v_slope = numeric(), 
                                    v_marg_r2 = numeric(),
                                    e_slope = numeric(),
                                    e_marg_r2 = numeric(),
                                    a_r2_unique = numeric(),
                                    v_r2_unique = numeric(),
                                    e_r2_unique = numeric(),
                                    ave_site_r2 = numeric())

for(i in 1:length(index_names)){
  
  index = index_names[i]
  
  ## individual models ##
  
  #linear model a
  formula_a = reformulate("a + (1 | site)", response = index) #formula for linear model
  model_a = lmer(formula_a, data = filter(offset_of_minima_q_std, v==0 & e==0))
  
  #calculate slopes for effect size
  coefficients_a = coef(summary(model_a))
  a_slope = coefficients_a["a","Estimate"]
  
  #calculate r²s for model quality/site effect
  a_r2 = performance::r2_nakagawa(model_a, tolerance = 0)
  a_marg_r2 = a_r2$R2_marginal
  
  
  #linear model v
  formula_v = reformulate("v + (1 | site)", response = index) #formula for linear model
  model_v = lmer(formula_v, data = filter(offset_of_minima_q_std, a==0 & e==0))
  
  #calculate slopes for effect size
  coefficients_v = coef(summary(model_v))
  v_slope = coefficients_v["v","Estimate"]
  
  #calculate r²s for model quality/site effect
  v_r2 = performance::r2_nakagawa(model_v, tolerance = 0)
  v_marg_r2 = v_r2$R2_marginal
  
  
  #linear model e
  formula_e = reformulate("e + (1 | site)", response = index) #formula for linear model
  model_e = lmer(formula_e, data = filter(offset_of_minima_q_std, a==0 & v==0))
  
  #calculate slopes for effect size
  coefficients_e = coef(summary(model_e))
  e_slope = coefficients_e["e","Estimate"]
  
  #calculate r²s for model quality/site effect
  e_r2 = performance::r2_nakagawa(model_e, tolerance = 0)
  e_marg_r2 = e_r2$R2_marginal
  
  
  ## unique variances ##
  
  #linear model a+v
  formula_av = reformulate("a+v + (1 | site)", response = index) #formula for linear model
  model_av = lmer(formula_av, data = offset_of_minima_q_std)
  
  #calculate r²s for model quality/site effect
  av_r2 = performance::r2_nakagawa(model_av, tolerance = 0)
  av_marg_r2 = av_r2$R2_marginal
  
  
  #linear model a+e
  formula_ae = reformulate("a+e + (1 | site)", response = index) #formula for linear model
  model_ae = lmer(formula_ae, data = offset_of_minima_q_std)
  
  #calculate r²s for model quality/site effect
  ae_r2 = performance::r2_nakagawa(model_ae, tolerance = 0)
  ae_marg_r2 = ae_r2$R2_marginal
  
  
  #linear model v+e
  formula_ve = reformulate("v+e + (1 | site)", response = index) #formula for linear model
  model_ve = lmer(formula_ve, data = offset_of_minima_q_std)
  
  #calculate r²s for model quality/site effect
  ve_r2 = performance::r2_nakagawa(model_ve, tolerance = 0)
  ve_marg_r2 = ve_r2$R2_marginal
  
  
  #linear model a+v+e
  formula_ave = reformulate("a+v+e + (1 | site)", response = index) #formula for linear model
  model_ave = lmer(formula_ave, data = offset_of_minima_q_std)
  
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
  
  
  min_offset_performance[i,] = c(as.character(index), a_slope, a_marg_r2, v_slope, v_marg_r2, e_slope, e_marg_r2,
                                 a_r2_unique, v_r2_unique, e_r2_unique, ave_site_r2)
  
}


write.csv(min_offset_performance, "1-data/1-4-index-performance/1-4-3-2-quantiles_r2/offset_of_minima_quantiles_lm.csv")




### SPECIFIC OFFSET OF MINIMA


specific_offset_of_minima_q = data.frame(site = character(),
                                a = numeric(),
                                v = numeric(),
                                e = numeric(),
                                p7.5 = numeric(),
                                p6.25= numeric(),
                                p5= numeric(),
                                p3.75 = numeric(),
                                p2.5 = numeric(),
                                p1.25 = numeric(),
                                p0 = numeric())


for(filename in files){
  
  data = read_rds(paste("1-data/1-2-microclimate-simulations/1-2-1-3-microclimate-simulations_FINAL/", filename, sep=""))
  climates = data[["Data"]]
  parameters = data[["Parameters"]]
  
  p7.5 = specific_offset_of_minima(macroclimate = climates$macro, microclimate = climates$micro, percentile = 0.075)
  p6.25 = specific_offset_of_minima(macroclimate = climates$macro, microclimate = climates$micro, percentile = 0.0625)
  p5 = specific_offset_of_minima(macroclimate = climates$macro, microclimate = climates$micro, percentile = 0.05)
  p3.75 = specific_offset_of_minima(macroclimate = climates$macro, microclimate = climates$micro, percentile = 0.0375)
  p2.5 = specific_offset_of_minima(macroclimate = climates$macro, microclimate = climates$micro, percentile = 0.025)
  p1.25 = specific_offset_of_minima(macroclimate = climates$macro, microclimate = climates$micro, percentile = 0.0125)
  p0 = specific_offset_of_minima(macroclimate = climates$macro, microclimate = climates$micro, percentile = 0)
  
  
  specific_offset_of_minima_q = specific_offset_of_minima_q %>% add_row(site = parameters["site"], a = as.numeric(parameters["a"]),
                                                      v = as.numeric(parameters["v"]), e = as.numeric(parameters["e"]),
                                                      p7.5 = p7.5, p6.25 = p6.25, p5 = p5, p3.75 = p3.75,
                                                      p2.5 = p2.5, p1.25 = p1.25, p0 = p0)
} 

write.csv(specific_offset_of_minima_q, "1-data/1-4-index-performance/1-4-3-1-quantiles_FINAL/specific_offset_of_minima_quantiles.csv")


specific_offset_of_minima_q = read.csv("1-data/1-4-index-performance/1-4-3-1-quantiles_FINAL/specific_offset_of_minima_quantiles.csv")

# z standardize all columns, excluding col 1 and 2 (index and site)
specific_offset_of_minima_q_std = specific_offset_of_minima_q
specific_offset_of_minima_q_std[,-c(1:2)] = as.data.frame(lapply(specific_offset_of_minima_q[,-c(1:2)], function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)))
#specific_offset_of_minima_q_std = drop_na(specific_offset_of_minima_q_std) # remove possible NAs

#list of index names: column names without the first 5 columns (index, site, a, v, e)
index_names = colnames(specific_offset_of_minima_q_std[,-c(1:5)])

#write dataframe of lm results 
spec_min_offset_performance = data.frame(index = character(),
                                         a_slope = numeric(),
                                         a_marg_r2 = numeric(),
                                         v_slope = numeric(), 
                                         v_marg_r2 = numeric(),
                                         e_slope = numeric(),
                                         e_marg_r2 = numeric(),
                                         a_r2_unique = numeric(),
                                         v_r2_unique = numeric(),
                                         e_r2_unique = numeric(),
                                         ave_site_r2 = numeric())

for(i in 1:length(index_names)){
  
  
  index = index_names[i]
  
  ## individual models ##
  
  #linear model a
  formula_a = reformulate("a + (1 | site)", response = index) #formula for linear model
  model_a = lmer(formula_a, data = filter(specific_offset_of_minima_q_std, v==0 & e==0))
  
  #calculate slopes for effect size
  coefficients_a = coef(summary(model_a))
  a_slope = coefficients_a["a","Estimate"]
  
  #calculate r²s for model quality/site effect
  a_r2 = performance::r2_nakagawa(model_a, tolerance = 0)
  a_marg_r2 = a_r2$R2_marginal
  
  
  #linear model v
  formula_v = reformulate("v + (1 | site)", response = index) #formula for linear model
  model_v = lmer(formula_v, data = filter(specific_offset_of_minima_q_std, a==0 & e==0))
  
  #calculate slopes for effect size
  coefficients_v = coef(summary(model_v))
  v_slope = coefficients_v["v","Estimate"]
  
  #calculate r²s for model quality/site effect
  v_r2 = performance::r2_nakagawa(model_v, tolerance = 0)
  v_marg_r2 = v_r2$R2_marginal
  
  
  #linear model e
  formula_e = reformulate("e + (1 | site)", response = index) #formula for linear model
  model_e = lmer(formula_e, data = filter(specific_offset_of_minima_q_std, a==0 & v==0))
  
  #calculate slopes for effect size
  coefficients_e = coef(summary(model_e))
  e_slope = coefficients_e["e","Estimate"]
  
  #calculate r²s for model quality/site effect
  e_r2 = performance::r2_nakagawa(model_e, tolerance = 0)
  e_marg_r2 = e_r2$R2_marginal
  
  
  ## unique variances ##
  
  #linear model a+v
  formula_av = reformulate("a+v + (1 | site)", response = index) #formula for linear model
  model_av = lmer(formula_av, data = specific_offset_of_minima_q_std)
  
  #calculate r²s for model quality/site effect
  av_r2 = performance::r2_nakagawa(model_av, tolerance = 0)
  av_marg_r2 = av_r2$R2_marginal
  
  
  #linear model a+e
  formula_ae = reformulate("a+e + (1 | site)", response = index) #formula for linear model
  model_ae = lmer(formula_ae, data = specific_offset_of_minima_q_std)
  
  #calculate r²s for model quality/site effect
  ae_r2 = performance::r2_nakagawa(model_ae, tolerance = 0)
  ae_marg_r2 = ae_r2$R2_marginal
  
  
  #linear model v+e
  formula_ve = reformulate("v+e + (1 | site)", response = index) #formula for linear model
  model_ve = lmer(formula_ve, data = specific_offset_of_minima_q_std)
  
  #calculate r²s for model quality/site effect
  ve_r2 = performance::r2_nakagawa(model_ve, tolerance = 0)
  ve_marg_r2 = ve_r2$R2_marginal
  
  
  #linear model a+v+e
  formula_ave = reformulate("a+v+e + (1 | site)", response = index) #formula for linear model
  model_ave = lmer(formula_ave, data = specific_offset_of_minima_q_std)
  
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
  
  
  spec_min_offset_performance[i,] = c(as.character(index), a_slope, a_marg_r2, v_slope, v_marg_r2, e_slope, e_marg_r2,
                                 a_r2_unique, v_r2_unique, e_r2_unique, ave_site_r2)
  
}


write.csv(spec_min_offset_performance, "1-data/1-4-index-performance/1-4-3-2-quantiles_r2/specific_offset_of_minima_quantiles_lm.csv")




### AMPLITUDE RATIO

amplitude_ratio_q = data.frame(site = character(),
                                a = numeric(),
                                v = numeric(),
                                e = numeric(),
                                p7.5 = numeric(),
                                p6.25= numeric(),
                                p5= numeric(),
                                p3.75 = numeric(),
                                p2.5 = numeric(),
                                p1.25 = numeric(),
                                p0 = numeric())


for(filename in files){
  
  data = read_rds(paste("1-data/1-2-microclimate-simulations/1-2-1-3-microclimate-simulations_FINAL/", filename, sep=""))
  climates = data[["Data"]]
  parameters = data[["Parameters"]]
  
  p7.5 = amplitude_ratio(macroclimate = climates$macro, microclimate = climates$micro, percentile_min = 0.075, percentile_max = 0.925)
  p6.25 = amplitude_ratio(macroclimate = climates$macro, microclimate = climates$micro, percentile_min = 0.0625, percentile_max = 0.9375)
  p5 = amplitude_ratio(macroclimate = climates$macro, microclimate = climates$micro, percentile_min = 0.05, percentile_max = 0.95)
  p3.75 = amplitude_ratio(macroclimate = climates$macro, microclimate = climates$micro, percentile_min = 0.0375, percentile_max = 0.9625)
  p2.5 = amplitude_ratio(macroclimate = climates$macro, microclimate = climates$micro, percentile_min = 0.025, percentile_max = 0.975)
  p1.25 = amplitude_ratio(macroclimate = climates$macro, microclimate = climates$micro, percentile_min = 0.0125, percentile_max = 0.9875)
  p0 = amplitude_ratio(macroclimate = climates$macro, microclimate = climates$micro, percentile_min = 0, percentile_max = 1)
  
  
  amplitude_ratio_q = amplitude_ratio_q %>% add_row(site = parameters["site"], a = as.numeric(parameters["a"]),
                                                      v = as.numeric(parameters["v"]), e = as.numeric(parameters["e"]),
                                                      p7.5 = p7.5, p6.25 = p6.25, p5 = p5, p3.75 = p3.75,
                                                      p2.5 = p2.5, p1.25 = p1.25, p0 = p0)
} 

write.csv(amplitude_ratio_q, "1-data/1-4-index-performance/1-4-3-1-quantiles_FINAL/amplitude_ratio_quantiles.csv")


amplitude_ratio_q = read.csv("1-data/1-4-index-performance/1-4-3-1-quantiles_FINAL/amplitude_ratio_quantiles.csv")

# z standardize all columns, excluding col 1 and 2 (index and site)
amplitude_ratio_q_std = amplitude_ratio_q
amplitude_ratio_q_std[,-c(1:2)] = as.data.frame(lapply(amplitude_ratio_q[,-c(1:2)], function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)))
#amplitude_ratio_q_std = drop_na(amplitude_ratio_q_std) # remove possible NAs

#list of index names: column names without the first 5 columns (index, site, a, v, e)
index_names = colnames(amplitude_ratio_q_std[,-c(1:5)])

#write dataframe of lm results 
amplitude_ratio_performance = data.frame(index = character(),
                                         a_slope = numeric(),
                                         a_marg_r2 = numeric(),
                                         v_slope = numeric(), 
                                         v_marg_r2 = numeric(),
                                         e_slope = numeric(),
                                         e_marg_r2 = numeric(),
                                         a_r2_unique = numeric(),
                                         v_r2_unique = numeric(),
                                         e_r2_unique = numeric(),
                                         ave_site_r2 = numeric())

for(i in 1:length(index_names)){
  
  index = index_names[i]
  
  ## individual models ##
  
  #linear model a
  formula_a = reformulate("a + (1 | site)", response = index) #formula for linear model
  model_a = lmer(formula_a, data = filter(amplitude_ratio_q_std, v==0 & e==0))
  
  #calculate slopes for effect size
  coefficients_a = coef(summary(model_a))
  a_slope = coefficients_a["a","Estimate"]
  
  #calculate r²s for model quality/site effect
  a_r2 = performance::r2_nakagawa(model_a, tolerance = 0)
  a_marg_r2 = a_r2$R2_marginal
  
  
  #linear model v
  formula_v = reformulate("v + (1 | site)", response = index) #formula for linear model
  model_v = lmer(formula_v, data = filter(amplitude_ratio_q_std, a==0 & e==0))
  
  #calculate slopes for effect size
  coefficients_v = coef(summary(model_v))
  v_slope = coefficients_v["v","Estimate"]
  
  #calculate r²s for model quality/site effect
  v_r2 = performance::r2_nakagawa(model_v, tolerance = 0)
  v_marg_r2 = v_r2$R2_marginal
  
  
  #linear model e
  formula_e = reformulate("e + (1 | site)", response = index) #formula for linear model
  model_e = lmer(formula_e, data = filter(amplitude_ratio_q_std, a==0 & v==0))
  
  #calculate slopes for effect size
  coefficients_e = coef(summary(model_e))
  e_slope = coefficients_e["e","Estimate"]
  
  #calculate r²s for model quality/site effect
  e_r2 = performance::r2_nakagawa(model_e, tolerance = 0)
  e_marg_r2 = e_r2$R2_marginal
  
  
  ## unique variances ##
  
  #linear model a+v
  formula_av = reformulate("a+v + (1 | site)", response = index) #formula for linear model
  model_av = lmer(formula_av, data = amplitude_ratio_q_std)
  
  #calculate r²s for model quality/site effect
  av_r2 = performance::r2_nakagawa(model_av, tolerance = 0)
  av_marg_r2 = av_r2$R2_marginal
  
  
  #linear model a+e
  formula_ae = reformulate("a+e + (1 | site)", response = index) #formula for linear model
  model_ae = lmer(formula_ae, data = amplitude_ratio_q_std)
  
  #calculate r²s for model quality/site effect
  ae_r2 = performance::r2_nakagawa(model_ae, tolerance = 0)
  ae_marg_r2 = ae_r2$R2_marginal
  
  
  #linear model v+e
  formula_ve = reformulate("v+e + (1 | site)", response = index) #formula for linear model
  model_ve = lmer(formula_ve, data = amplitude_ratio_q_std)
  
  #calculate r²s for model quality/site effect
  ve_r2 = performance::r2_nakagawa(model_ve, tolerance = 0)
  ve_marg_r2 = ve_r2$R2_marginal
  
  
  #linear model a+v+e
  formula_ave = reformulate("a+v+e + (1 | site)", response = index) #formula for linear model
  model_ave = lmer(formula_ave, data = amplitude_ratio_q_std)
  
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
  
  
  amplitude_ratio_performance[i,] = c(as.character(index), a_slope, a_marg_r2, v_slope, v_marg_r2, e_slope, e_marg_r2,
                                 a_r2_unique, v_r2_unique, e_r2_unique, ave_site_r2)
}

write.csv(amplitude_ratio_performance, "1-data/1-4-index-performance/1-4-3-2-quantiles_r2/amplitude_ratio_quantiles_lm.csv")


### AMPLITUDE OFFSET

amplitude_offset_q = data.frame(site = character(),
                               a = numeric(),
                               v = numeric(),
                               e = numeric(),
                               p7.5 = numeric(),
                               p6.25= numeric(),
                               p5= numeric(),
                               p3.75 = numeric(),
                               p2.5 = numeric(),
                               p1.25 = numeric(),
                               p0 = numeric())


for(filename in files){
  
  data = read_rds(paste("1-data/1-2-microclimate-simulations/1-2-1-3-microclimate-simulations_FINAL/", filename, sep=""))
  climates = data[["Data"]]
  parameters = data[["Parameters"]]
  
  p7.5 = amplitude_offset(macroclimate = climates$macro, microclimate = climates$micro, percentile_min = 0.075, percentile_max = 0.925)
  p6.25 = amplitude_offset(macroclimate = climates$macro, microclimate = climates$micro, percentile_min = 0.0625, percentile_max = 0.9375)
  p5 = amplitude_offset(macroclimate = climates$macro, microclimate = climates$micro, percentile_min = 0.05, percentile_max = 0.95)
  p3.75 = amplitude_offset(macroclimate = climates$macro, microclimate = climates$micro, percentile_min = 0.0375, percentile_max = 0.9625)
  p2.5 = amplitude_offset(macroclimate = climates$macro, microclimate = climates$micro, percentile_min = 0.025, percentile_max = 0.975)
  p1.25 = amplitude_offset(macroclimate = climates$macro, microclimate = climates$micro, percentile_min = 0.0125, percentile_max = 0.9875)
  p0 = amplitude_offset(macroclimate = climates$macro, microclimate = climates$micro, percentile_min = 0, percentile_max = 1)
  
  
  amplitude_offset_q = amplitude_offset_q %>% add_row(site = parameters["site"], a = as.numeric(parameters["a"]),
                                                    v = as.numeric(parameters["v"]), e = as.numeric(parameters["e"]),
                                                    p7.5 = p7.5, p6.25 = p6.25, p5 = p5, p3.75 = p3.75,
                                                    p2.5 = p2.5, p1.25 = p1.25, p0 = p0)
} 

write.csv(amplitude_offset_q, "1-data/1-4-index-performance/1-4-3-1-quantiles_FINAL/amplitude_offset_quantiles.csv")


amplitude_offset_q = read.csv("1-data/1-4-index-performance/1-4-3-1-quantiles_FINAL/amplitude_offset_quantiles.csv")

# z standardize all columns, excluding col 1 and 2 (index and site)
amplitude_offset_q_std = amplitude_offset_q
amplitude_offset_q_std[,-c(1:2)] = as.data.frame(lapply(amplitude_offset_q[,-c(1:2)], function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)))
#amplitude_offset_q_std = drop_na(amplitude_offset_q_std) # remove possible NAs

#list of index names: column names without the first 5 columns (index, site, a, v, e)
index_names = colnames(amplitude_offset_q_std[,-c(1:5)])

#write dataframe of lm results 
amplitude_offset_performance = data.frame(index = character(),
                                          a_slope = numeric(),
                                          a_marg_r2 = numeric(),
                                          v_slope = numeric(), 
                                          v_marg_r2 = numeric(),
                                          e_slope = numeric(),
                                          e_marg_r2 = numeric(),
                                          a_r2_unique = numeric(),
                                          v_r2_unique = numeric(),
                                          e_r2_unique = numeric(),
                                          ave_site_r2 = numeric())
for(i in 1:length(index_names)){
  
  index = index_names[i]
  
  ## individual models ##
  
  #linear model a
  formula_a = reformulate("a + (1 | site)", response = index) #formula for linear model
  model_a = lmer(formula_a, data = filter(amplitude_offset_q_std, v==0 & e==0))
  
  #calculate slopes for effect size
  coefficients_a = coef(summary(model_a))
  a_slope = coefficients_a["a","Estimate"]
  
  #calculate r²s for model quality/site effect
  a_r2 = performance::r2_nakagawa(model_a, tolerance = 0)
  a_marg_r2 = a_r2$R2_marginal
  
  
  #linear model v
  formula_v = reformulate("v + (1 | site)", response = index) #formula for linear model
  model_v = lmer(formula_v, data = filter(amplitude_offset_q_std, a==0 & e==0))
  
  #calculate slopes for effect size
  coefficients_v = coef(summary(model_v))
  v_slope = coefficients_v["v","Estimate"]
  
  #calculate r²s for model quality/site effect
  v_r2 = performance::r2_nakagawa(model_v, tolerance = 0)
  v_marg_r2 = v_r2$R2_marginal
  
  
  #linear model e
  formula_e = reformulate("e + (1 | site)", response = index) #formula for linear model
  model_e = lmer(formula_e, data = filter(amplitude_offset_q_std, a==0 & v==0))
  
  #calculate slopes for effect size
  coefficients_e = coef(summary(model_e))
  e_slope = coefficients_e["e","Estimate"]
  
  #calculate r²s for model quality/site effect
  e_r2 = performance::r2_nakagawa(model_e, tolerance = 0)
  e_marg_r2 = e_r2$R2_marginal
  
  
  ## unique variances ##
  
  #linear model a+v
  formula_av = reformulate("a+v + (1 | site)", response = index) #formula for linear model
  model_av = lmer(formula_av, data = amplitude_offset_q_std)
  
  #calculate r²s for model quality/site effect
  av_r2 = performance::r2_nakagawa(model_av, tolerance = 0)
  av_marg_r2 = av_r2$R2_marginal
  
  
  #linear model a+e
  formula_ae = reformulate("a+e + (1 | site)", response = index) #formula for linear model
  model_ae = lmer(formula_ae, data = amplitude_offset_q_std)
  
  #calculate r²s for model quality/site effect
  ae_r2 = performance::r2_nakagawa(model_ae, tolerance = 0)
  ae_marg_r2 = ae_r2$R2_marginal
  
  
  #linear model v+e
  formula_ve = reformulate("v+e + (1 | site)", response = index) #formula for linear model
  model_ve = lmer(formula_ve, data = amplitude_offset_q_std)
  
  #calculate r²s for model quality/site effect
  ve_r2 = performance::r2_nakagawa(model_ve, tolerance = 0)
  ve_marg_r2 = ve_r2$R2_marginal
  
  
  #linear model a+v+e
  formula_ave = reformulate("a+v+e + (1 | site)", response = index) #formula for linear model
  model_ave = lmer(formula_ave, data = amplitude_offset_q_std)
  
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
  
  
  amplitude_offset_performance[i,] = c(as.character(index), a_slope, a_marg_r2, v_slope, v_marg_r2, e_slope, e_marg_r2,
                                      a_r2_unique, v_r2_unique, e_r2_unique, ave_site_r2)
}

write.csv(amplitude_offset_performance, "1-data/1-4-index-performance/1-4-3-2-quantiles_r2/amplitude_offset_quantiles_lm.csv")

library(tidyverse)
library(RColorBrewer)
library(gridExtra)
library(grid)

### OFFSET OF MAXIMA

max_offset_performance = read.csv("1-data/1-4-index-performance/1-4-3-2-quantiles_r2/offset_of_maxima_quantiles_lm.csv")

index_names = max_offset_performance$index

#round slope numbers for the plot
max_offset_performance$a_marg_r2 = round(as.numeric(max_offset_performance$a_marg_r2),4)
max_offset_performance$v_marg_r2 = round(as.numeric(max_offset_performance$v_marg_r2),4)
max_offset_performance$e_marg_r2 = round(as.numeric(max_offset_performance$e_marg_r2),4)
max_offset_performance$a_r2_unique = round(as.numeric(max_offset_performance$a_r2_unique),4)
max_offset_performance$v_r2_unique = round(as.numeric(max_offset_performance$v_r2_unique),4)
max_offset_performance$e_r2_unique = round(as.numeric(max_offset_performance$e_r2_unique),4)
max_offset_performance$index = factor(max_offset_performance$index, levels = index_names)

colors = c("a" = "blue", "v" = "orange", "e" = "red")

# plot of slopes ~ quantiles 
max_offset_marginal_r2 = ggplot(max_offset_performance)+
  geom_point(aes(x = index, y = a_marg_r2, col = "a"))+
  geom_line(aes(x = index, y = a_marg_r2, col="a"), group=1)+
  geom_point(aes(x = index, y = v_marg_r2, col="v"))+
  geom_line(aes(x = index, y = v_marg_r2, col="v"), group=1)+
  geom_point(aes(x = index, y = e_marg_r2, col="e"))+
  geom_line(aes(x = index, y = e_marg_r2, col="e"), group=1)+
  scale_y_continuous(limits=c(0, 1))+
  #scale_x_discrete(labels=index_names)+
  scale_color_manual(values = colors, breaks = c("a", "v", "e"))+
  labs(x = "Quantile", y = "Marginal R² for index ~ parameter", color = "Modulation\nparameter")+
  theme_bw()


# plot of slopes ~ quantiles 
max_offset_unique_r2 = ggplot(max_offset_performance)+
  geom_point(aes(x = index, y = a_r2_unique, col = "a"))+
  geom_line(aes(x = index, y = a_r2_unique, col="a"), group=1)+
  geom_point(aes(x = index, y = v_r2_unique, col="v"))+
  geom_line(aes(x = index, y = v_r2_unique, col="v"), group=1)+
  geom_point(aes(x = index, y = e_r2_unique, col="e"))+
  geom_line(aes(x = index, y = e_r2_unique, col="e"), group=1)+
  scale_y_continuous(limits=c(0, 1))+
  #scale_x_discrete(labels=index_names)+
  scale_color_manual(values = colors, breaks = c("a", "v", "e"))+
  labs(x = "Quantile", y = "Unique R² ")+
  theme_bw()+
  theme(legend.position="none")


max_offset = grid.arrange(max_offset_marginal_r2, max_offset_unique_r2, widths=c(0.55, 0.45), top=textGrob("\na) Offset of maxima\n", hjust=3.4))

ggsave("1-data/1-4-index-performance/1-4-3-2-quantiles_r2/plots/max_offset_quantiles.png", max_offset, width=3500, height=1500, unit="px")


### OFFSET OF MINIMA

min_offset_performance = read.csv("1-data/1-4-index-performance/1-4-3-2-quantiles_r2/offset_of_minima_quantiles_lm.csv")

index_names = min_offset_performance$index

#round slope numbers for the plot
min_offset_performance$a_marg_r2 = round(as.numeric(min_offset_performance$a_marg_r2),4)
min_offset_performance$v_marg_r2 = round(as.numeric(min_offset_performance$v_marg_r2),4)
min_offset_performance$e_marg_r2 = round(as.numeric(min_offset_performance$e_marg_r2),4)
min_offset_performance$a_r2_unique = round(as.numeric(min_offset_performance$a_r2_unique),4)
min_offset_performance$v_r2_unique = round(as.numeric(min_offset_performance$v_r2_unique),4)
min_offset_performance$e_r2_unique = round(as.numeric(min_offset_performance$e_r2_unique),4)
min_offset_performance$index = factor(min_offset_performance$index, levels = index_names)

colors = c("a" = "blue", "v" = "orange", "e" = "red")
# plot of slopes ~ quantiles 
min_offset_marginal_r2 = ggplot(min_offset_performance)+
  geom_point(aes(x = index, y = a_marg_r2, col = "a"))+
  geom_line(aes(x = index, y = a_marg_r2, col="a"), group=1)+
  geom_point(aes(x = index, y = v_marg_r2, col="v"))+
  geom_line(aes(x = index, y = v_marg_r2, col="v"), group=1)+
  geom_point(aes(x = index, y = e_marg_r2, col="e"))+
  geom_line(aes(x = index, y = e_marg_r2, col="e"), group=1)+
  scale_y_continuous(limits=c(0, 1))+
  #scale_x_discrete(labels=index_names)+
  scale_color_manual(values = colors, breaks = c("a", "v", "e"))+
  labs(x = "Quantile", y = "Marginal R² for index ~ parameter", color = "Modulation\nparameter")+
  theme_bw()

# plot of marginal R² ~ quantiles
min_offset_unique_r2 = ggplot(min_offset_performance)+
  geom_point(aes(x = index, y = a_r2_unique, col = "a"))+
  geom_line(aes(x = index, y = a_r2_unique, col="a"), group=1)+
  geom_point(aes(x = index, y = v_r2_unique, col="v"))+
  geom_line(aes(x = index, y = v_r2_unique, col="v"), group=1)+
  geom_point(aes(x = index, y = e_r2_unique, col="e"))+
  geom_line(aes(x = index, y = e_r2_unique, col="e"), group=1)+
  scale_y_continuous(limits=c(0, 1))+
  #scale_x_discrete(labels=index_names)+
  scale_color_manual(values = colors, breaks = c("a", "v", "e"))+
  labs(x = "Quantile", y = "Unique R² ")+
  theme_bw()+
  theme(legend.position="none")

min_offset = grid.arrange(min_offset_marginal_r2, min_offset_unique_r2, widths=c(0.55, 0.45), top=textGrob("\nb) Offset of minima\n", hjust=3.5))

ggsave("1-data/1-4-index-performance/1-4-3-2-quantiles_r2/plots/min_offset_quantiles.png", min_offset, width=3500, height=1500, unit="px")


min_max_offset = grid.arrange(max_offset, min_offset, nrow=2)
ggsave("1-data/1-4-index-performance/1-4-3-2-quantiles_r2/plots/max_min_offset.png", min_max_offset, width=3500, height=3000, unit="px")




### SPECIFIC OFFSET OF MAXIMA

spec_max_offset_performance = read.csv("1-data/1-4-index-performance/1-4-3-2-quantiles_r2/specific_offset_of_maxima_quantiles_lm.csv")

index_names = spec_max_offset_performance$index

#round slope numbers for the plot
spec_max_offset_performance$a_marg_r2 = round(as.numeric(spec_max_offset_performance$a_marg_r2),4)
spec_max_offset_performance$v_marg_r2 = round(as.numeric(spec_max_offset_performance$v_marg_r2),4)
spec_max_offset_performance$e_marg_r2 = round(as.numeric(spec_max_offset_performance$e_marg_r2),4)
spec_max_offset_performance$a_r2_unique = round(as.numeric(spec_max_offset_performance$a_r2_unique),4)
spec_max_offset_performance$v_r2_unique = round(as.numeric(spec_max_offset_performance$v_r2_unique),4)
spec_max_offset_performance$e_r2_unique = round(as.numeric(spec_max_offset_performance$e_r2_unique),4)
spec_max_offset_performance$index = factor(spec_max_offset_performance$index, levels = index_names)

colors = c("a" = "blue", "v" = "orange", "e" = "red")

# plot of slopes ~ quantiles 
spec_max_offset_marginal_r2 = ggplot(spec_max_offset_performance)+
  geom_point(aes(x = index, y = a_marg_r2, col = "a"))+
  geom_line(aes(x = index, y = a_marg_r2, col="a"), group=1)+
  geom_point(aes(x = index, y = v_marg_r2, col="v"))+
  geom_line(aes(x = index, y = v_marg_r2, col="v"), group=1)+
  geom_point(aes(x = index, y = e_marg_r2, col="e"))+
  geom_line(aes(x = index, y = e_marg_r2, col="e"), group=1)+
  scale_y_continuous(limits=c(0, 1))+
  #scale_x_discrete(labels=index_names)+
  scale_color_manual(values = colors, breaks = c("a", "v", "e"))+
  labs(x = "Quantile", y = "Marginal R² for index ~ parameter", color = "Modulation\nparameter")+
  theme_bw()


# plot of slopes ~ quantiles 
spec_max_offset_unique_r2 = ggplot(spec_max_offset_performance)+
  geom_point(aes(x = index, y = a_r2_unique, col = "a"))+
  geom_line(aes(x = index, y = a_r2_unique, col="a"), group=1)+
  geom_point(aes(x = index, y = v_r2_unique, col="v"))+
  geom_line(aes(x = index, y = v_r2_unique, col="v"), group=1)+
  geom_point(aes(x = index, y = e_r2_unique, col="e"))+
  geom_line(aes(x = index, y = e_r2_unique, col="e"), group=1)+
  scale_y_continuous(limits=c(0, 1))+
  #scale_x_discrete(labels=index_names)+
  scale_color_manual(values = colors, breaks = c("a", "v", "e"))+
  labs(x = "Quantile", y = "Unique R² ")+
  theme_bw()+
  theme(legend.position="none")

spec_max_offset = grid.arrange(spec_max_offset_marginal_r2, spec_max_offset_unique_r2, widths=c(0.55, 0.45), top=textGrob("\na) Specific offset of maxima\n", hjust=2.4) )

ggsave("1-data/1-4-index-performance/1-4-3-2-quantiles_r2/plots/spec_max_offset_quantiles.png", spec_max_offset, width=3500, height=1500, unit="px")




### SPECIFIC OFFSET OF MINIMA

spec_min_offset_performance = read.csv("1-data/1-4-index-performance/1-4-3-2-quantiles_r2/specific_offset_of_minima_quantiles_lm.csv")

index_names = spec_min_offset_performance$index

#round slope numbers for the plot
spec_min_offset_performance$a_marg_r2 = round(as.numeric(spec_min_offset_performance$a_marg_r2),4)
spec_min_offset_performance$v_marg_r2 = round(as.numeric(spec_min_offset_performance$v_marg_r2),4)
spec_min_offset_performance$e_marg_r2 = round(as.numeric(spec_min_offset_performance$e_marg_r2),4)
spec_min_offset_performance$a_r2_unique = round(as.numeric(spec_min_offset_performance$a_r2_unique),4)
spec_min_offset_performance$v_r2_unique = round(as.numeric(spec_min_offset_performance$v_r2_unique),4)
spec_min_offset_performance$e_r2_unique = round(as.numeric(spec_min_offset_performance$e_r2_unique),4)
spec_min_offset_performance$index = factor(spec_min_offset_performance$index, levels = index_names)

colors = c("a" = "blue", "v" = "orange", "e" = "red")

# plot of slopes ~ quantiles 
spec_min_offset_marginal_r2 = ggplot(spec_min_offset_performance)+
  geom_point(aes(x = index, y = a_marg_r2, col = "a"))+
  geom_line(aes(x = index, y = a_marg_r2, col="a"), group=1)+
  geom_point(aes(x = index, y = v_marg_r2, col="v"))+
  geom_line(aes(x = index, y = v_marg_r2, col="v"), group=1)+
  geom_point(aes(x = index, y = e_marg_r2, col="e"))+
  geom_line(aes(x = index, y = e_marg_r2, col="e"), group=1)+
  scale_y_continuous(limits=c(0, 1))+
  #scale_x_discrete(labels=index_names)+
  scale_color_manual(values = colors, breaks = c("a", "v", "e"))+
  labs(x = "Quantile", y = "Marginal R² for index ~ parameter", color = "Modulation\nparameter")+
  theme_bw()


# plot of slopes ~ quantiles 
spec_min_offset_unique_r2 = ggplot(spec_min_offset_performance)+
  geom_point(aes(x = index, y = a_r2_unique, col = "a"))+
  geom_line(aes(x = index, y = a_r2_unique, col="a"), group=1)+
  geom_point(aes(x = index, y = v_r2_unique, col="v"))+
  geom_line(aes(x = index, y = v_r2_unique, col="v"), group=1)+
  geom_point(aes(x = index, y = e_r2_unique, col="e"))+
  geom_line(aes(x = index, y = e_r2_unique, col="e"), group=1)+
  scale_y_continuous(limits=c(0, 1))+
  #scale_x_discrete(labels=index_names)+
  scale_color_manual(values = colors, breaks = c("a", "v", "e"))+
  labs(x = "Quantile", y = "Unique R² ")+
  theme_bw()+
  theme(legend.position="none")

spec_min_offset = grid.arrange(spec_min_offset_marginal_r2, spec_min_offset_unique_r2, widths=c(0.55, 0.45), top=textGrob("\nb) Specific offset of minima\n", hjust=2.45) )

ggsave("1-data/1-4-index-performance/1-4-3-2-quantiles_r2/plots/spec_min_offset_quantiles.png", spec_min_offset, width=3500, height=1500, unit="px")


spec_max_min = grid.arrange(spec_max_offset, spec_min_offset, nrow=2)
ggsave("1-data/1-4-index-performance/1-4-3-2-quantiles_r2/plots/spec_max_min.png", spec_max_min, width=3500, height=3000, unit="px")





### AMPLITUDE OFFSET

amplitude_offset_performance = read.csv("1-data/1-4-index-performance/1-4-3-2-quantiles_r2/amplitude_offset_quantiles_lm.csv")

index_names = amplitude_offset_performance$index

#round slope numbers for the plot
amplitude_offset_performance$a_marg_r2 = round(as.numeric(amplitude_offset_performance$a_marg_r2),4)
amplitude_offset_performance$v_marg_r2 = round(as.numeric(amplitude_offset_performance$v_marg_r2),4)
amplitude_offset_performance$e_marg_r2 = round(as.numeric(amplitude_offset_performance$e_marg_r2),4)
amplitude_offset_performance$a_r2_unique = round(as.numeric(amplitude_offset_performance$a_r2_unique),4)
amplitude_offset_performance$v_r2_unique = round(as.numeric(amplitude_offset_performance$v_r2_unique),4)
amplitude_offset_performance$e_r2_unique = round(as.numeric(amplitude_offset_performance$e_r2_unique),4)
amplitude_offset_performance$index = factor(amplitude_offset_performance$index, levels = index_names)

colors = c("a" = "blue", "v" = "orange", "e" = "red")

amplitude_offset_marginal_r2 = ggplot(amplitude_offset_performance)+
  geom_point(aes(x = index, y = a_marg_r2, col = "a"))+
  geom_line(aes(x = index, y = a_marg_r2, col="a"), group=1)+
  geom_point(aes(x = index, y = v_marg_r2, col="v"))+
  geom_line(aes(x = index, y = v_marg_r2, col="v"), group=1)+
  geom_point(aes(x = index, y = e_marg_r2, col="e"))+
  geom_line(aes(x = index, y = e_marg_r2, col="e"), group=1)+
  scale_y_continuous(limits=c(0, 1))+
  #scale_x_discrete(labels=index_names)+
  scale_color_manual(values = colors, breaks = c("a", "v", "e"))+
  labs(x = "Quantile", y = "Marginal R² for index ~ parameter", color = "Modulation\nparameter")+
  theme_bw()


# plot of slopes ~ quantiles 
amplitude_offset_unique_r2 = ggplot(amplitude_offset_performance)+
  geom_point(aes(x = index, y = a_r2_unique, col = "a"))+
  geom_line(aes(x = index, y = a_r2_unique, col="a"), group=1)+
  geom_point(aes(x = index, y = v_r2_unique, col="v"))+
  geom_line(aes(x = index, y = v_r2_unique, col="v"), group=1)+
  geom_point(aes(x = index, y = e_r2_unique, col="e"))+
  geom_line(aes(x = index, y = e_r2_unique, col="e"), group=1)+
  scale_y_continuous(limits=c(0, 1))+
  #scale_x_discrete(labels=index_names)+
  scale_color_manual(values = colors, breaks = c("a", "v", "e"))+
  labs(x = "Quantile", y = "Unique R² ")+
  theme_bw()+
  theme(legend.position="none")

amplitude_offset = grid.arrange(amplitude_offset_marginal_r2, amplitude_offset_unique_r2, widths=c(0.55, 0.45), top=textGrob("\na) Amplitude offset\n", hjust=3.53) )

ggsave("1-data/1-4-index-performance/1-4-3-2-quantiles_r2/plots/amplitude_offset_quantiles.png", amplitude_offset, width=3500, height=1500, unit="px")



### AMPLITUDE RATIO

amplitude_ratio_performance = read.csv("1-data/1-4-index-performance/1-4-3-2-quantiles_r2/amplitude_ratio_quantiles_lm.csv")

index_names = amplitude_ratio_performance$index

#round slope numbers for the plot
amplitude_ratio_performance$a_marg_r2 = round(as.numeric(amplitude_ratio_performance$a_marg_r2),4)
amplitude_ratio_performance$v_marg_r2 = round(as.numeric(amplitude_ratio_performance$v_marg_r2),4)
amplitude_ratio_performance$e_marg_r2 = round(as.numeric(amplitude_ratio_performance$e_marg_r2),4)
amplitude_ratio_performance$a_r2_unique = round(as.numeric(amplitude_ratio_performance$a_r2_unique),4)
amplitude_ratio_performance$v_r2_unique = round(as.numeric(amplitude_ratio_performance$v_r2_unique),4)
amplitude_ratio_performance$e_r2_unique = round(as.numeric(amplitude_ratio_performance$e_r2_unique),4)
amplitude_ratio_performance$index = factor(amplitude_ratio_performance$index, levels = index_names)

colors = c("a" = "blue", "v" = "orange", "e" = "red")

amplitude_ratio_marginal_r2 = ggplot(amplitude_ratio_performance)+
  geom_point(aes(x = index, y = a_marg_r2, col = "a"))+
  geom_line(aes(x = index, y = a_marg_r2, col="a"), group=1)+
  geom_point(aes(x = index, y = v_marg_r2, col="v"))+
  geom_line(aes(x = index, y = v_marg_r2, col="v"), group=1)+
  geom_point(aes(x = index, y = e_marg_r2, col="e"))+
  geom_line(aes(x = index, y = e_marg_r2, col="e"), group=1)+
  scale_y_continuous(limits=c(0, 1))+
  #scale_x_discrete(labels=index_names)+
  scale_color_manual(values = colors, breaks = c("a", "v", "e"))+
  labs(x = "Quantile", y = "Marginal R² for index ~ parameter", color = "Modulation\nparameter")+
  theme_bw()


# plot of slopes ~ quantiles 
amplitude_ratio_unique_r2 = ggplot(amplitude_ratio_performance)+
  geom_point(aes(x = index, y = a_r2_unique, col = "a"))+
  geom_line(aes(x = index, y = a_r2_unique, col="a"), group=1)+
  geom_point(aes(x = index, y = v_r2_unique, col="v"))+
  geom_line(aes(x = index, y = v_r2_unique, col="v"), group=1)+
  geom_point(aes(x = index, y = e_r2_unique, col="e"))+
  geom_line(aes(x = index, y = e_r2_unique, col="e"), group=1)+
  scale_y_continuous(limits=c(0, 1))+
  #scale_x_discrete(labels=index_names)+
  scale_color_manual(values = colors, breaks = c("a", "v", "e"))+
  labs(x = "Quantile", y = "Unique R² ")+
  theme_bw()+
  theme(legend.position="none")

amplitude_ratio = grid.arrange(amplitude_ratio_marginal_r2, amplitude_ratio_unique_r2, widths=c(0.55, 0.45), top=textGrob("\nb) Amplitude ratio\n", hjust = 3.7) )

ggsave("1-data/1-4-index-performance/1-4-3-2-quantiles_r2/plots/amplitude_ratio_quantiles.png", amplitude_ratio, width=3500, height=1500, unit="px")

amplitudes = grid.arrange(amplitude_offset, amplitude_ratio, nrow=2)
ggsave("1-data/1-4-index-performance/1-4-3-2-quantiles_r2/plots/amplitude_quantiles.png", amplitudes, width=3500, height=3000, unit="px")



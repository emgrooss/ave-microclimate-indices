rm(list=ls())

library(tidyverse)
library(shadowtext)
library(RColorBrewer)
library(ggrepel)
library(gridExtra)
library(svglite)

# table of linear model results
index_performance = read.csv("1-data/1-4-index-performance/1-4-1-4-index_performance_individual_models.csv")

#table of variance partitioning results
index_var = read.csv("1-data/1-4-index-performance/1-4-1-4-index_performance_var_partitioned.csv")

# index table only read in to get names
indices = read.csv("1-data/1-3-indices/1-3-1-3-indices_FINAL.csv")
#list of index names: column names without the first 5 columns (index, site, a, v, e)
index_names = colnames(indices[,-c(1:5)])

# lists of index names for the three categories
a_index_names = c("Mean offset" = "mean_offset","Median offset" = "median_offset",
                  "Sum of offsets" = "sum_of_offsets", "Equilibrium" = "equilibrium")

a_index_names_filtered = c("Mean offset" = "mean_offset","Median offset" = "median_offset",
                  "Sum of offsets" = "sum_of_offsets" #"Equilibrium" = "equilibrium"
                  )

v_index_names = c("Offset of SDs" = "sd_offset", "Mean daily offset of SDs" = "sd_offset_mean_daily",
                  "Amplitude offset (5%)" = "amplitude_offset.95", "Amplitude offset (2.5%)"= "amplitude_offset.975",
                  #"Amplitude offset (0%)" = "amplitude_offset1.00",
                  "Mean daily amplitude offset" = "amplitude_offset_mean_daily",
                  "Amplitude ratio (5%)" ="amplitude_ratio.95", "Amplitude ratio (2.5%)" = "amplitude_ratio.975",
                  "CV offset" = "CV_offset", "Mean daily CV offset" = "CV_offset_mean_daily",
                  "CV ratio" = "CV_ratio", "Slope" = "slope",
                  "Change ratio" = "sensitivity", "Correlation" = "correlation_micro_macro")

v_index_names_filtered = c("Offset of SDs" = "sd_offset", "Mean daily offset of SDs" = "sd_offset_mean_daily",
                  "Amplitude offset (5%)" = "amplitude_offset.95", "Amplitude offset (2.5%)"= "amplitude_offset.975",
                  #"Amplitude offset (0%)" = "amplitude_offset1.00",
                  "Mean daily amplitude offset" = "amplitude_offset_mean_daily",
                  "Amplitude ratio (5%)" ="amplitude_ratio.95", "Amplitude ratio (2.5%)" = "amplitude_ratio.975",
                  #"CV offset" = "CV_offset", "Mean daily CV offset" = "CV_offset_mean_daily",
                  #"CV ratio" = "CV_ratio",
                  "Slope" = "slope",
                  "Change ratio" = "sensitivity"
                  #"Correlation" = "correlation_micro_macro"
                  )

e_index_names = c(#"offset_of_maxima.95",
                  "Offset of maxima (97.5%)" = "offset_of_maxima.975",
                  "Offset of maxima (100%)" = "offset_of_maxima1.00",
                  #"offset_of_maxima_mean_daily.95",
                  "Mean daily offset of maxima (97.5%)" = "offset_of_maxima_mean_daily.975", 
                  "Mean daily offset of maxima (100%)" = "offset_of_maxima_mean_daily1.00",
                  "p95 of daily offset of maxima" = "p95_daily_maxima_offset",
                  # "specific_offset_of_maxima.95",
                  #"Specific offset of maxima (97.5%)" = "specific_offset_of_maxima.975",
                  #"Specific offset of maxima (100%)" = "specific_offset_of_maxima1.00",
                  
                  #"offset_of_minima.05", 
                  "Offset of minima (2.5%)" = "offset_of_minima.025",
                  "Offset of minima (0%)" = "offset_of_minima.00", #"offset_of_minima_mean_daily.05",
                  "Mean daily offset of minima (2.5%)" = "offset_of_minima_mean_daily.025", 
                  "Mean daily offset of minima (0%)" ="offset_of_minima_mean_daily.00",
                  "p5 of daily offset of minima" = "p5_daily_minima_offset"
                  #"specific_offset_of_minima.05",
                  #"Specific offset of minima (2.5%)" = "specific_offset_of_minima.025",
                  #"Specific offset of minima (0%)" = "specific_offset_of_minima.00"
                  )

e_index_names_filtered = c(#"offset_of_maxima.95", 
  "Offset of maxima (97.5%)" = "offset_of_maxima.975",
  "Offset of maxima (100%)" = "offset_of_maxima1.00", #"offset_of_maxima_mean_daily.95",
  #"Mean daily offset of maxima (97.5%)" = "offset_of_maxima_mean_daily.975", 
  #"Mean daily offset of maxima (100%)" = "offset_of_maxima_mean_daily1.00",
  #"p95 of daily offset of maxima" = "p95_daily_maxima_offset",# "specific_offset_of_maxima.95",
  #"Specific offset of maxima (97.5%)" = "specific_offset_of_maxima.975",
  #"Specific offset of maxima (100%)" = "specific_offset_of_maxima1.00",
  
  #"offset_of_minima.05", 
  "Offset of minima (2.5%)" = "offset_of_minima.025",
  "Offset of minima (0%)" = "offset_of_minima.00"#, #"offset_of_minima_mean_daily.05",
  #"Mean daily offset of minima (2.5%)" = "offset_of_minima_mean_daily.025", 
  #"Mean daily offset of minima (0%)" ="offset_of_minima_mean_daily.00",
  #"p5 of daily offset of minima" = "p5_daily_minima_offset", #"specific_offset_of_minima.05",
  #"Specific offset of minima (2.5%)" = "specific_offset_of_minima.025",
  #"Specific offset of minima (0%)" = "specific_offset_of_minima.00"
  )



all_index_names = c(a_index_names, v_index_names, e_index_names)


## COMBINED VARIANCES PLOT

marginal_r2s = index_performance %>% 
  filter(index%in%all_index_names) %>%
  rename(a = a_marg_r2, v = v_marg_r2, e = e_marg_r2) %>%
  select(index, a, v, e) %>%
  pivot_longer(cols = c(a, v, e), names_to = "indicator", values_to = "performance")

unique_r2s = index_var %>%
  filter(index%in%all_index_names) %>%
  rename(a = a_r2_unique, v = v_r2_unique, e = e_r2_unique) %>%
  select(index, a, v, e) %>%
  pivot_longer(cols = c(a, v, e), names_to = "indicator", values_to = "performance")


(combined_plot = ggplot(marginal_r2s)+
    geom_point(aes(x=indicator, y=index, size=performance),
               shape = 21, col="black", fill="grey")+
    scale_radius(range = c(1, 14), name="Proportion of\nvariance explained\nby the aspect\n(marginal/unique)")+
    geom_point(data = unique_r2s,
               aes(x=indicator, y=index, size=abs(performance), fill = indicator),
               shape = 21, col="black"
               )+
    guides(fill = "none")+
    #lines with index categories
    geom_hline(yintercept = c(10.5, 23.5, 27.5), size = .2) +
    annotate(geom="label", x=2, y=10.5, label="Extreme indices", fill="white", label.size=NA)+
    annotate(geom="label", x=2, y=23.5, label="Variability indices", fill="white", label.size=NA)+
    annotate(geom="label", x=2, y=27.5, label="Average indices", fill="white", label.size=NA)+
    scale_y_discrete(limits = rev(all_index_names), labels = names(rev(all_index_names)))+
    scale_x_discrete(limits = c("a", "v", "e"))+
    scale_fill_manual(values = c("a" = "#4C5B88", "v" = "#C9C766", "e" = "#C96666"))+
    labs(x = NULL, y = NULL)+
    theme_minimal()+
    theme(plot.title = element_text(face="bold", size = 18),
          legend.title = element_text(size = 14), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.text.x = element_text(size=16, colour = "black"), axis.text.y =element_text(size=14, colour = "black"))
          
)
ggsave("1-data/1-4-index-performance/1-4-2-3-index-performance-plots_FINAL/var_partition_PAPER.jpg", combined_plot, width = 3500, height = 3500, unit="px")

## BEST INDICES PLOT: Version with just top 3 labels

#function to get legend from plot
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#plot to get legend from
(uniquevar_all = index_var %>%
    filter(index%in%all_index_names)%>%
    mutate(indextype="all")%>%
    ggplot(aes(x=ave_marg_r2, y=indextype))+
    geom_line()+
    geom_point()+
    geom_label_repel(aes(label = names(all_index_names), fill = ave_site_r2), color = 'black', size = 3, max.overlaps = 25)+
    scale_fill_gradient2(limits = c(0, 0.18), low = "#FFF", mid ="#D55", high = "#D00", midpoint = 0.09, name = "Site effect\n(R²)")+
    scale_y_discrete(name="")+
    scale_x_continuous(limits = c(0,1))+
    theme_minimal()
)

legend = get_legend(uniquevar_all)

# a indices plot
(uniquevar_a = index_var %>%
    filter(index%in%a_index_names)%>%
    mutate(indextype="a")%>%
    ggplot(aes(x=a_r2_unique, y=indextype))+
    geom_line()+
    geom_point(aes(fill = ave_site_r2), shape=21, size=4)+
    scale_y_discrete(name="")+
    scale_x_continuous(limits = c(0,1))+
    labs(title = "\n(a) Best average modulation indices",
         x ="unique R² (a)")+
    geom_label_repel(data = subset(mutate(index_var, indextype="a"),
                                   index %in% c("mean_offset", "median_offset", "sum_of_offsets")),
                     aes(label = c("Mean offset", "Median offset", "Sum of offsets")), color = 'black', size = 3)+
    scale_fill_gradient2(limits = c(0, 0.18), low = "#FFF", mid ="#D55", high = "#D00", midpoint = 0.09, name = "Site effect\n(R²)")+
    theme_minimal()+
    theme(legend.position="none",
          axis.text.y = element_text(size=12))
)

# v indices plot
(uniquevar_v = index_var %>%
    filter(index%in%v_index_names)%>%
    mutate(indextype="v")%>%
    ggplot(aes(x=v_r2_unique, y=indextype))+
    geom_line()+
    geom_point(aes(fill = ave_site_r2), shape=21, size=4)+
    scale_y_discrete(name="")+
    scale_x_continuous(limits = c(0,1))+
    labs(title = "\n(b) Best variability modulation indices",
         x ="unique R² (v)")+
    geom_label_repel(data = subset(mutate(index_var, indextype="v"),
                                   index %in% c("amplitude_offset.95", "amplitude_ratio.95",
                                                "amplitude_offset_mean_daily", "sensitivity")),
                     aes(label = c("Amplitude offset(5%)", "Mean daily amplitude offset",
                                   "Amplitude ratio (5%)", "Change ratio")), color = 'black', size = 3)+
    scale_fill_gradient2(limits = c(0, 0.18), low = "#FFF", mid ="#D55", high = "#D00", midpoint = 0.09, name = "Site effect\n(R²)")+
    theme_minimal()+
    theme(legend.position="none",
          axis.text.y = element_text(size=12))
)

# e indices plot
(uniquevar_e = index_var %>%
    filter(index%in%e_index_names)%>%
    mutate(indextype="e")%>%
    ggplot(aes(x=e_r2_unique, y=indextype))+
    geom_line()+
    geom_point(aes(fill = ave_site_r2), shape=21, size=4)+
    scale_y_discrete(name="")+
    scale_x_continuous(limits = c(0,1))+
    labs(title = "\n(c) Best extreme modulation indices",
         x ="unique R² (e)")+
    geom_label_repel(data = subset(mutate(index_var, indextype="e"),
                                   index %in% c("offset_of_maxima1.00", "offset_of_minima.00",
                                                "offset_of_maxima.975", "offset_of_minima.025")),
                     aes(label = c("Offset of maxima (97.5%)", "Offset of maxima (100%)",
                                   "Offset of minima (2.5%)","Offset of minima (0%)")),
                     color = 'black', size = 3)+
    scale_fill_gradient2(limits = c(0, 0.18), low = "#FFF", mid ="#D55", high = "#D00", midpoint = 0.09, name = "Site effect\n(R²)")+ 
    theme_minimal()+
    theme(legend.position="none",
          axis.text.y = element_text(size=12))
)

combined_var_plots = grid.arrange(uniquevar_a, uniquevar_v, uniquevar_e, legend,
                                  nrow = 3, layout_matrix = cbind(c(1,2,3), c(4,4,4)),
                                  widths = c(0.87, 0.13))

ggsave("1-data/1-4-index-performance/1-4-2-3-index-performance-plots_FINAL/unique_r2_PAPER2.jpg", combined_var_plots,
       width = 2000, height = 2000, unit="px")


## BEST INDICES PLOTS: version with all names + site R²

#function to get legend from plot
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#plot to get legend from
(uniquevar_all = index_var %>%
  filter(index%in%all_index_names)%>%
  mutate(indextype="all")%>%
  ggplot(aes(x=ave_marg_r2, y=indextype))+
  geom_line()+
  geom_point()+
  geom_label_repel(aes(label = names(all_index_names), fill = ave_site_r2), color = 'black', size = 3, max.overlaps = 25)+
  scale_fill_gradient2(limits = c(0, 0.18), low = "#FFF", mid ="#D55", high = "#D00", midpoint = 0.09, name = "Site effect\n(R²)")+
  scale_y_discrete(name="")+
  scale_x_continuous(limits = c(0,1))+
  theme_minimal()
)

legend = get_legend(uniquevar_all)

# a indices plot
(uniquevar_a = index_var %>%
    filter(index%in%a_index_names)%>%
    mutate(indextype="a")%>%
    ggplot(aes(x=a_r2_unique, y=indextype))+
    geom_line()+
    geom_point()+
    scale_y_discrete(name="")+
    scale_x_continuous(limits = c(0,1))+
    labs(title = "\n(a) Specific response to average modulation",
         x ="unique R²")+
    geom_label_repel(data = subset(mutate(index_var, indextype="a"),
                                   index %in% c("mean_offset", "median_offset", "sum_of_offsets")),
                     aes(label = c("Mean offset", "Median offset", "Sum of offsets"), fill = ave_site_r2), color = 'black', size = 3, max.overlaps = 25)+
    scale_fill_gradient2(limits = c(0, 0.18), low = "#FFF", mid ="#D55", high = "#D00", midpoint = 0.09, name = "Site effect (R²)")+
    theme_minimal()+
    theme(legend.position="none",
          axis.text.y = element_text(size=12))
)


# v indices plot
(uniquevar_v = index_var %>%
    filter(index%in%v_index_names)%>%
    mutate(indextype="v")%>%
    ggplot(aes(x=v_r2_unique, y=indextype))+
    geom_line()+
    geom_point()+
    scale_y_discrete(name="")+
    scale_x_continuous(limits = c(0,1))+
    labs(title = "\n(b) Specific response to variability modulation",
         x ="unique R²")+
    geom_label_repel(aes(label = names(v_index_names), fill = ave_site_r2), color = 'black', size = 3, max.overlaps = 25)+
    scale_fill_gradient2(limits = c(0, 0.18), low = "#FFF", mid ="#D55", high = "#D00", midpoint = 0.09, name = "Site effect (R²)")+
    theme_minimal()+
    theme(legend.position="none",
          axis.text.y = element_text(size=12))
)

# e indices plot
(uniquevar_e = index_var %>%
    filter(index%in%e_index_names)%>%
    mutate(indextype="e")%>%
    ggplot(aes(x=e_r2_unique, y=indextype))+
    geom_line()+
    geom_point()+
    scale_y_discrete(name="")+
    scale_x_continuous(limits = c(0,1))+
    labs(title = "\n(c) Specific response to extreme modulation",
         x ="unique R²")+
    geom_label_repel(aes(label = names(e_index_names), fill = ave_site_r2), color = 'black', size = 3, max.overlaps = 25)+
    scale_fill_gradient2(limits = c(0, 0.18), low = "#FFF", mid ="#D55", high = "#D00", midpoint = 0.09, name = "Site effect (R²)")+
    theme_minimal()+
    theme(legend.position="none",
          axis.text.y = element_text(size=12))
)

combined_var_plots = grid.arrange(uniquevar_a, uniquevar_v, uniquevar_e, legend,
                              nrow = 3, layout_matrix = cbind(c(1,2,3), c(4,4,4)),
                              widths = c(0.85, 0.15))

ggsave("1-data/1-4-index-performance/1-4-2-3-index-performance-plots_FINAL/unique_r2_PAPER.jpg", combined_var_plots,
       width = 2000, height = 2600, unit="px")

  



## OLD PLOTS

#data for plots
performance = index_performance %>% 
  pivot_longer(cols = c(a_marg_r2, v_marg_r2, e_marg_r2),
                                   names_to = "indicator", values_to = "performance")

variances = index_var %>% 
  select(index, a_r2_unique, v_r2_unique, e_r2_unique) %>%
  pivot_longer(cols = c(a_r2_unique, v_r2_unique, e_r2_unique),
               names_to = "indicator", values_to = "performance")

combined = performance %>%
  select(index, a_r2_unique, v_r2_unique, e_r2_unique, ave_site_r2) %>%
  left_join(index_performance, by="index")
  

## bubble plot

(plot_all_bubble = performance %>% 
    filter(index%in%all_index_names) %>%
    ggplot(aes(x=indicator, y=index, size=abs(performance), fill=abs(performance)))+
    geom_point(shape = 21, col="black")+
    scale_radius(range = c(1, 14), name=NULL) +
    guides(size = "none")+
    #lines with index categories
    geom_hline(yintercept = c(10.5, 23.5, 27.5), size = .2) +
    annotate(geom="label", x=2, y=10.5, label="Extreme indices", fill="white", label.size=NA)+
    annotate(geom="label", x=2, y=23.5, label="Variability indices", fill="white", label.size=NA)+
    annotate(geom="label", x=2, y=27.5, label="Average indices", fill="white", label.size=NA)+
    #scales
    scale_x_discrete(position = "top") +
    #geom_shadowtext(aes(label = round(performance, 3)), size = 18/.pt)+
    #scale_fill_distiller(palette="Spectral", name = "Site effect (R²)")+
    scale_fill_gradient2(limits = c(0, 1), low = "#80E0FF", mid ="#FFFF00", high = "#FF6262", midpoint = 0.5, name = "marginal R² of\nindex ~ parameter\n")+
    scale_x_discrete(limits = c("a_marg_r2", "v_marg_r2", "e_marg_r2"),
                     labels = c("a", "v", "e"))+
    scale_y_discrete(limits = rev(all_index_names), labels = names(rev(all_index_names)))+
    labs(#title = "Indices' response to changes in\nmicroclimate modulation parameters\n",
         #x = "\nMicroclimate modulation parameter",
         x = NULL, y = NULL)+
    theme_minimal()+
    theme(plot.title = element_text(face="bold", size = 18),
          legend.title = element_text(size = 14), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.text.x = element_text(size=16, colour = "black"), axis.text.y =element_text(size=14, colour = "black"),
    )
)
ggsave("1-data/1-4-index-performance/1-4-2-3-index-performance-plots_FINAL/slopes_bubble_individual_models_PAPER.jpg", plot_all_bubble, width = 3000, height = 4000, unit="px")


(plot_var = variances %>% 
    filter(index%in%all_index_names) %>%
    ggplot(aes(x=indicator, y=index, size=round(performance,4), fill=round(performance,4)))+
    geom_point(shape = 21, col="black")+
    scale_radius(range = c(1, 14), name=NULL) +
    guides(size = "none")+
    #lines with index categories
    geom_hline(yintercept = c(12.5, 23.5, 27.5), size = .2) +
    annotate(geom="label", x=2, y=12.5, label="Extreme indices", fill="white", label.size=NA)+
    annotate(geom="label", x=2, y=23.5, label="Variability indices", fill="white", label.size=NA)+
    annotate(geom="label", x=2, y=27.5, label="Average indices", fill="white", label.size=NA)+
    #scales
    scale_x_discrete(position = "top") +
    #geom_shadowtext(aes(label = round(performance, 3)), size = 18/.pt)+
    #scale_fill_distiller(palette="Spectral", name = "Site effect (R²)")+
    scale_fill_gradient2(limits = c(0, 1), low = "#80E0FF", mid ="#FFFF00", high = "#FF6262", midpoint = 0.5, name = "Unique R²\nin index ~ a+v+e\n")+
    scale_x_discrete(limits = c("a_r2_unique", "v_r2_unique", "e_r2_unique"), labels =c("a", "v", "e"))+
    scale_y_discrete(limits = rev(all_index_names), labels = names(rev(all_index_names)))+
    labs(#title = "Indices' response to changes in\nmicroclimate modulation parameters\n",
      #x = "\nMicroclimate modulation parameter",
      x = NULL, y = NULL)+
    theme_minimal()+
    theme(plot.title = element_text(face="bold", size = 18),
          legend.title = element_text(size = 14), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.text.x = element_text(size=16, colour = "black"), axis.text.y =element_text(size=14, colour = "black"),
    )
)
ggsave("1-data/1-4-index-performance/1-4-2-3-index-performance-plots_FINAL/slopes_bubble_unique_variance_PAPER.jpg", plot_var, width = 3000, height = 400, unit="px")




#function to get legend from plot
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


# plots with max slope - other slopes

##this one is only to get the legend from
(plot_combined_all_a = combined %>%
    ggplot(aes(x=a_r2_unique, y=a_marg_r2))+
    geom_point(aes(fill=ave_site_r2), shape=21, size=10)+
    geom_label_repel(aes(label = index, fill = ave_site_r2), color = 'black', size = 4, max.overlaps = 20)+
    #geom_text(aes(label=index))+
    scale_fill_gradient2(limits = c(0, 0.01), low = "#80E0FF", mid ="#FFFF00", high = "#FF6262", midpoint=0.005, name = "Site R²")+
    #scale_fill_distiller(palette = "Spectral", name = "Site effect (R²)")+
    #scale_color_distiller(palette = "Spectral", direction = 1)+
    xlim(0,1)+
    ylim(0,1)+
    labs(title = "Average indices",
         x = "unique variance (R²) of a in model index ~ a*v*e", y = "marginal R² of a in model a~index")+
    geom_hline(yintercept = 0, linetype="dashed")+
    theme_classic()
)

legend = get_legend(plot_combined_all_a)

(plot_combined_a = combined %>%
    filter(index%in%a_index_names_filtered) %>%
    ggplot(aes(x=a_r2_unique, y=a_marg_r2))+
    geom_hline(yintercept = 1, linetype="dashed", col="grey")+
    geom_vline(xintercept = 1, linetype="dashed", col="grey")+
    geom_hline(yintercept = 0.9, linetype="dashed", col="grey")+
    geom_vline(xintercept = 0.9, linetype="dashed", col="grey")+
    #annotate("segment", x=0.85, y=1, xend=0.995, yend=1, arrow = arrow(), col="forestgreen", size=1.5)+
    #annotate("segment", x=1, y=0.85, xend=1, yend=0.99, arrow = arrow(), col="forestgreen", size=1.5)+
    #annotate(geom="label", x=1, y=1, label="Ideal\nindex", fill="white", col="forestgreen", label.size=NA)+
    annotate("segment", x=0.85, y=0.85, xend=0.985, yend=0.985, arrow = arrow(), col="firebrick", size=1.5)+
    #annotate(geom="label", x=1, y=0.925, label="Index\nquality", fill="white", col="forestgreen", label.size=NA)+
    #annotate(geom="label", x=0.925, y=1, label="Index quality", fill="white", col="forestgreen", label.size=NA)+
    annotate(geom="label", x=0.925, y=0.925, label="Index\nquality", fill="white", col="firebrick", label.size=NA)+
    scale_x_continuous(breaks = c(0.9, 1))+
    scale_y_continuous(breaks = c(0.9, 1))+
    geom_point(aes(fill=ave_site_r2), shape=21, size=10)+
    geom_label_repel(aes(label = names(a_index_names_filtered), fill = ave_site_r2), color = 'black', size = 3, max.overlaps = 25)+
    #geom_text(aes(label=index))+
    scale_fill_gradient2(limits = c(0, 0.01), low = "#80E0FF", mid ="#FFFF00", high = "#FF6262", midpoint=0.005, name = "Site R²")+
    #scale_color_distiller(palette = "Spectral", direction = 1)+
    #scale_fill_distiller(palette = "Spectral", name = "Site effect (R²)", limits=c(0,0.18))+
    #xlim(0,1)+
    #ylim(0,1)+
    #scale_y_continuous(limits = c(-0,1.1), breaks=c(0,1))+
    labs(title = "(a) Best average indices",
         x = "Response specificity (a)", y = "Response strength (a)")+
    theme_classic()+
    theme(legend.position="none")
)

(plot_combined_v = combined %>%
    filter(index%in%v_index_names_filtered) %>%
    ggplot(aes(x=v_r2_unique, y=v_marg_r2))+
    geom_hline(yintercept = 1, linetype="dashed", col="grey")+
    geom_vline(xintercept = 1, linetype="dashed", col="grey")+
    geom_hline(yintercept = 0.9, linetype="dashed", col="grey")+
    geom_vline(xintercept = 0.9, linetype="dashed", col="grey")+
    geom_hline(yintercept = 0.8, linetype="dashed", col="grey")+
    geom_vline(xintercept = 0.8, linetype="dashed", col="grey")+
    geom_vline(xintercept = 0.7, linetype="dashed", col="grey")+
    #annotate("segment", x=0.65, y=1, xend=0.99, yend=1, arrow = arrow(), col="forestgreen", size=1.5)+
    #annotate("segment", x=1, y=0.75, xend=1, yend=0.99, arrow = arrow(), col="forestgreen", size=1.5)+
    #annotate(geom="label", x=1, y=1, label="Ideal\nindex", fill="white", col="forestgreen", label.size=NA)+
    annotate("segment", x=0.65, y=0.75, xend=0.98, yend=0.98, arrow = arrow(), col="firebrick", size=1.5)+
    #annotate(geom="label", x=0.65, y=0.875, label="Index\nquality", fill="white", col="forestgreen", label.size=NA)+
    #annotate(geom="label", x=0.825, y=1, label="Index quality", fill="white", col="forestgreen", label.size=NA)+
    annotate(geom="label", x=0.82, y=0.885, label="Index\nquality", fill="white", col="firebrick", label.size=NA)+
    geom_point(aes(fill=ave_site_r2), shape=21, size=10)+
    geom_label_repel(aes(label = names(v_index_names_filtered), fill = ave_site_r2), color = 'black', size = 3, max.overlaps = 20)+
    #geom_text(aes(label=index))+
    scale_fill_gradient2(limits = c(0, 0.01), low = "#80E0FF", mid ="#FFFF00", high = "#FF6262", midpoint=0.005, name = "Site R²")+
    #scale_color_distiller(palette = "Spectral", direction = 1)+
    #scale_fill_distiller(palette = "Spectral", name = "Site effect (R²)", limits=c(0,0.18))+
    #xlim(0,1)+
    #ylim(0,1)+
    #scale_y_continuous(limits = c(-0,1.1), breaks=c(0,1))+
    labs(title = "(b) Best variability indices",
         x = "Response specificity (v)", y = "Response strength (v)")+
  theme_classic()+
  theme(legend.position="none")
)

(plot_combined_e = combined %>%
    filter(index%in%e_index_names_filtered) %>%
    ggplot(aes(x=e_r2_unique, y=e_marg_r2))+
    geom_hline(yintercept = 1, linetype="dashed", col="grey")+
    geom_vline(xintercept = 1, linetype="dashed", col="grey")+
    geom_hline(yintercept = 0.9, linetype="dashed", col="grey")+
    geom_vline(xintercept = 0.9, linetype="dashed", col="grey")+
    geom_hline(yintercept = 0.8, linetype="dashed", col="grey")+
    geom_vline(xintercept = 0.8, linetype="dashed", col="grey")+
    geom_hline(yintercept = 0.7, linetype="dashed", col="grey")+
    geom_vline(xintercept = 0.7, linetype="dashed", col="grey")+
    geom_hline(yintercept = 0.6, linetype="dashed", col="grey")+
    geom_vline(xintercept = 0.6, linetype="dashed", col="grey")+
    geom_hline(yintercept = 0.5, linetype="dashed", col="grey")+
    geom_vline(xintercept = 0.5, linetype="dashed", col="grey")+
    geom_vline(xintercept = 0.4, linetype="dashed", col="grey")+
    geom_vline(xintercept = 0.3, linetype="dashed", col="grey")+
    geom_vline(xintercept = 0.2, linetype="dashed", col="grey")+
    geom_vline(xintercept = 0.1, linetype="dashed", col="grey")+
    geom_vline(xintercept = 0, linetype="dashed", col="grey")+
    #annotate("segment", x=0, y=1, xend=0.98, yend=1, arrow = arrow(), col="forestgreen", size=1.5)+
    #annotate("segment", x=1, y=0.45, xend=1, yend=0.98, arrow = arrow(), col="forestgreen", size=1.5)+
    #annotate(geom="label", x=1, y=1, label="Ideal\nindex", fill="white", col="forestgreen", label.size=NA)+
    annotate("segment", x=0, y=0.45, xend=0.95, yend=0.95, arrow = arrow(), col="firebrick", size=1.5)+
    #annotate(geom="label", x=1, y=0.7, label="Index\nquality", fill="white", col="forestgreen", label.size=NA)+
    #annotate(geom="label", x=0.5, y=1, label="Index quality", fill="white", col="forestgreen", label.size=NA)+
    annotate(geom="label", x=0.5, y=0.71, label="Index\nquality", fill="white", col="firebrick", label.size=NA)+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    geom_point(aes(fill=ave_site_r2), shape=21, size=10)+
    geom_label_repel(aes(label = names(e_index_names_filtered), fill = ave_site_r2), color = 'black', size = 3, max.overlaps = 30)+
    #geom_text(aes(label=index))+
    scale_fill_gradient2(limits = c(0, 0.01), low = "#80E0FF", mid ="#FFFF00", high = "#FF6262", midpoint=0.005, name = "Site R²")+
    #scale_color_distiller(palette = "Spectral", direction = 1)+
    #scale_fill_distiller(palette = "Spectral", name = "Site effect (R²)", limits=c(0,0.18))+
    #xlim(0,1)+
    #ylim(0,1)+
    #scale_y_continuous(limits = c(0,1.1), breaks=c(0,1))+
    labs(title = "(c) Best extreme indices",
         x = "Response specificity (e)", y = "Response strength (e)")+
    theme_classic()+
    theme(legend.position="none")
)

combined_plots = grid.arrange(plot_combined_a, plot_combined_v, plot_combined_e, legend,
                              nrow = 3, layout_matrix = cbind(c(1,2,3), c(4,4,4)),
                              widths = c(0.85, 0.15))

ggsave("1-data/1-4-index-performance/1-4-2-3-index-performance-plots_FINAL/combined_r2_PAPER.jpg", combined_plots, width = 2300, height = 3000, unit="px")



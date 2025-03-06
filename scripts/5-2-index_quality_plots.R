rm(list=ls())

library(tidyverse)
library(shadowtext)
library(RColorBrewer)
library(ggrepel)
library(grid)
library(gridExtra)
library(svglite)

# get table of variance partitioning results
index_var = read.csv("data/4-index-performance/index_performance.csv")

# get index names
index_names = index_var$index

# lists of index names for the three categories
a_index_names = c("Mean offset" = "mean_offset",
                  "Median offset" = "median_offset",
                  "Sum of offsets" = "sum_of_offsets",
                  "Equilibrium" = "equilibrium")
# reverse vector for labeller in plot
a_index_labels = setNames(names(a_index_names), a_index_names)

v_index_names = c("Offset of SDs" = "sd_offset",
                  "Mean daily\noffset of SDs" = "sd_offset_mean_daily",
                  "Amplitude\noffset (5%)" = "amplitude_offset.95",
                  "Amplitude\noffset (2.5%)"= "amplitude_offset.975",
                  "Mean daily\namplitude offset" = "amplitude_offset_mean_daily",
                  "Amplitude\nratio (5%)" ="amplitude_ratio.95",
                  "Amplitude\nratio (2.5%)" = "amplitude_ratio.975",
                  "CV offset" = "CV_offset",
                  "Mean daily\nCV offset" = "CV_offset_mean_daily",
                  "CV ratio" = "CV_ratio",
                  "Slope" = "slope",
                  "Change ratio" = "change_ratio",
                  "Correlation" = "correlation_micro_macro")

v_index_labels = setNames(names(v_index_names), v_index_names)


e_index_names = c("Offset of\nmaxima (97.5%)" = "offset_of_maxima.975",
                  "Offset of\nmaxima (100%)" = "offset_of_maxima1.00",
                  "Mean daily offset\nof maxima (97.5%)" = "offset_of_maxima_mean_daily.975", 
                  "Mean daily offset\nof maxima (100%)" = "offset_of_maxima_mean_daily1.00",
                  "p95 of daily\noffset of maxima" = "p95_daily_maxima_offset",
                  
                  "Offset of\nminima (2.5%)" = "offset_of_minima.025",
                  "Offset of\nminima (0%)" = "offset_of_minima.00",
                  "Mean daily offset\nof minima (2.5%)" = "offset_of_minima_mean_daily.025", 
                  "Mean daily offset\nof minima (0%)" = "offset_of_minima_mean_daily.00",
                  "p5 of daily\noffset of minima" = "p5_daily_minima_offset"
                  )
e_index_labels = setNames(names(e_index_names), e_index_names)

all_index_names = c(a_index_names, v_index_names, e_index_names)
all_index_labels = setNames(names(all_index_names), all_index_names)

index_var = filter(index_var, index %in% all_index_names)
index_var$index = factor(index_var$index, levels = all_index_names)

# average modulation indices plot

# filter & format data 
index_var_a = index_var %>%
  filter(index %in% a_index_names) %>%
  rename(a = a_marg_r2, v = v_marg_r2, e = e_marg_r2, site = ave_site_r2) %>%
  pivot_longer(cols = c(a, v, e, site), names_to = "aspect", values_to = "var_part") 
index_var_a$aspect = factor(index_var_a$aspect, levels = c("a", "v", "e", "site"))

# pie chart plot
a_plot = ggplot(index_var_a, aes(x="", y=var_part, fill=aspect))+
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~index, ncol = 4,
             labeller = as_labeller(a_index_labels)) +
  scale_fill_manual(values = c("a" = "#4C5B88", "v" = "#C9C766", "e" = "#C96666", "site" = "#AAA"),
                               name = "Variance in index\nexplained by")+
  scale_size(range = c(2, 10))+
  theme_void()+
  theme(legend.box.background = element_rect(colour = "black"),
      legend.box.margin=margin(5,5,5,5))+
  labs(title= "(a) Average modulation indices\n")

ggsave("plots/varpie_a.jpg", a_plot, width=2000, height=650, unit="px")

# variability modulation indices plot

# filter & format data 
index_var_v = index_var %>%
  filter(index %in% v_index_names) %>%
  rename(a = a_marg_r2, v = v_marg_r2, e = e_marg_r2, site = ave_site_r2) %>%
  pivot_longer(cols = c(a, v, e, site), names_to = "aspect", values_to = "var_part")
index_var_v$aspect = factor(index_var_v$aspect, levels = c("a", "v", "e", "site"))

# pie chart plot
v_plot = ggplot(index_var_v, aes(x="", y=var_part, fill=aspect))+
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~index, ncol = 5,
             labeller = as_labeller(v_index_labels)) +
  scale_fill_manual(values = c("a" = "#4C5B88", "v" = "#C9C766", "e" = "#C96666", "site" = "#AAA"))+
  scale_size(range = c(2, 10))+
  theme_void()+
  labs(title= "(b) Variability modulation indices\n")+
  theme(legend.position = "none")

ggsave("plots/varpie_v.jpg", v_plot, width=2000, height=1700, unit="px")


# extreme modulation indices plot

# filter & format data 
index_var_e = index_var %>%
  filter(index %in% e_index_names) %>%
  rename(a = a_marg_r2, v = v_marg_r2, e = e_marg_r2, site = ave_site_r2) %>%
  pivot_longer(cols = c(a, v, e, site), names_to = "aspect", values_to = "var_part")
index_var_e$aspect = factor(index_var_e$aspect, levels = c("a", "v", "e", "site"))

# pie chart plot
e_plot = ggplot(index_var_e, aes(x="", y=var_part, fill=aspect))+
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~index, ncol = 5,
             labeller = as_labeller(e_index_labels)) +
  scale_fill_manual(values = c("a" = "#4C5B88", "v" = "#C9C766", "e" = "#C96666", "site" = "#AAA"))+
  scale_size(range = c(2, 10))+
  theme_void()+
  labs(title= "(c) Extreme modulation indices\n")+
  theme(legend.position = "none")

ggsave("plots/varpie_e.jpg", e_plot, width=2000, height=1100, unit="px")



# plot of indices ranked by R²

# a indices plot
(uniquevar_a = index_var %>%
    filter(index%in%a_index_names)%>%
    mutate(indextype="a")%>%
    ggplot(aes(x=a_marg_r2, y=indextype))+
    geom_line(col = "#1B2A56")+
    geom_point(shape=21, size=4, aes(fill=a_marg_r2), col = "#1B2A56")+
    scale_y_discrete(name="")+
    scale_x_continuous(limits = c(0,1))+
    labs(title = "\n(a) Best average modulation indices",
         x ="R²(a)")+
    geom_label_repel(data = subset(mutate(index_var, indextype="a"),
                                   index %in% c("mean_offset", "median_offset", "sum_of_offsets")),
                     aes(label = c("Mean offset", "Median offset", "Sum of offsets"),
                         fill = a_marg_r2), color = '#1B2A56', size = 3)+
    scale_fill_gradient2(low = "#FFF", high = "#7581A4", midpoint = 0.65, limits = c(0, 1))+
    theme_minimal()+
    theme(legend.position="none",
          axis.text.y = element_text(size=12))
)

# v indices plot
(uniquevar_v = index_var %>%
    filter(index%in%v_index_names)%>%
    mutate(indextype="v")%>%
    ggplot(aes(x=v_marg_r2, y=indextype))+
    geom_line(col = "#565404")+
    geom_point(shape=21, size=4, aes(fill = v_marg_r2), col = "#565404")+
    scale_y_discrete(name="")+
    scale_x_continuous(limits = c(0,1))+
    labs(title = "\n(b) Best variability modulation indices",
         x ="R²(v)")+
    geom_label_repel(data = subset(mutate(index_var, indextype="v"),
                                   index %in% c("amplitude_offset.95", "amplitude_ratio.95",
                                                "amplitude_offset_mean_daily", "change_ratio")),
                     aes(label = c("Amplitude offset(5%)", "Mean daily amplitude offset",
                                   "Amplitude ratio (5%)", "Change ratio"),
                         fill = v_marg_r2), color = '#565404', size = 3)+
    scale_fill_gradient2(low = "#FFF", high = "#F5F3A4", midpoint = 0.5, limits = c(0, 1))+
    theme_minimal()+
    theme(legend.position="none",
          axis.text.y = element_text(size=12))
)

# e indices plot
(uniquevar_e = index_var %>%
    filter(index%in%e_index_names)%>%
    mutate(indextype="e")%>%
    ggplot(aes(x=e_marg_r2, y=indextype))+
    geom_line(col = "#560404")+
    geom_point(shape=21, size=4, aes(fill=e_marg_r2), col = "#560404")+
    scale_y_discrete(name="")+
    scale_x_continuous(limits = c(0,1))+
    labs(title = "\n(c) Best extreme modulation indices",
         x ="R²(e)")+
    geom_label_repel(data = subset(mutate(index_var, indextype="e"),
                                   index %in% c("offset_of_maxima1.00", "offset_of_minima.00",
                                                "offset_of_maxima.975", "offset_of_minima.025")),
                     aes(label = c("Offset of maxima (97.5%)", "Offset of maxima (100%)",
                                   "Offset of minima (2.5%)","Offset of minima (0%)"),
                         fill = e_marg_r2),
                     color = '#560404', size = 3)+
    scale_fill_gradient2(low = "#FFF", high = "#A43C3C", midpoint = 0.1, limits = c(0, 1))+
    theme_minimal()+
    theme(legend.position="none",
          axis.text.y = element_text(size=12))
)

combined_var_plots = grid.arrange(uniquevar_a, uniquevar_v, uniquevar_e)

ggsave("plots/best_indices.jpg", combined_var_plots,
       width = 2000, height = 2000, unit="px")
rm(list=ls())
library(tidyverse)
library(gridExtra)
library(grid)
library(ggsignif)

indices = read.csv("data/5-example-data/mydiv_indices.csv")

mean = ggplot(indices, aes(x=myc, y=mean_offset))+
  geom_boxplot(aes(group=myc), col="#30406E", outliers = F)+
  geom_jitter(shape=21, fill="#7581A5", col="#30406E", width = 0.05)+
  geom_signif(comparisons=list(c("AMF", "EMF")), map_signif_level = TRUE, test = "t.test",
              tip_length=0, margin_top = -0.1, extend_line = -0.1)+
  labs(x = "Mycorrhiza type", y = "Mean offset")+
  theme_bw()+
  theme(plot.title = element_text(size=rel(0.9)))
  
median = ggplot(indices, aes(x=myc, y=median_offset))+
  geom_boxplot(aes(group=myc), col="#30406E", outliers = F)+
  geom_jitter(shape=21, fill="#7581A5", col="#30406E", width = 0.05)+
  geom_signif(comparisons=list(c("AMF", "EMF")), map_signif_level = TRUE, test = "t.test",
              tip_length=0, margin_top = -0.1, extend_line = -0.1)+
  labs(x = "Mycorrhiza type", y = "Median offset")+
  theme_bw()+
  theme(plot.title = element_text(size=rel(0.9)))

avgs = grid.arrange(mean, median, nrow = 1,
                    top=textGrob("\n(a) Average modulation", hjust = 1.4)
                   # bottom=textGrob("Mycorrhiza type\n")
                    )

cr = ggplot(indices, aes(x=myc, y=change_ratio))+
  geom_boxplot(aes(group=myc), col="#807E1C", outliers = F)+
  geom_jitter(shape=21, fill="#C9C766", col="#807E1C", width = 0.05)+
  geom_signif(comparisons=list(c("AMF", "EMF")), map_signif_level = TRUE, test = "t.test",
              tip_length=0, margin_top = -0.1, extend_line = -0.1)+
  labs(x = "Mycorrhiza type", y = "Change ratio")+
  theme_bw()+
  theme(plot.title = element_text(size=rel(0.9)))

amp = ggplot(indices, aes(x=myc, y=amplitude_offset))+
  geom_boxplot(aes(group=myc), col="#807E1C", outliers = F)+
  geom_jitter(shape=21, fill="#C9C766", col="#807E1C", width = 0.05)+
  geom_signif(comparisons=list(c("AMF", "EMF")), map_signif_level = TRUE, test = "t.test",
              tip_length=0, margin_top = -0.1, extend_line = -0.1)+
  labs(x = "Mycorrhiza type", y = "Amplitude offset (p5 to p95)")+
  theme_bw()+
  theme(plot.title = element_text(size=rel(0.9)))

vars = grid.arrange(cr, amp, nrow = 1,
                    top=textGrob("\n(b) Variability modulation", hjust = 1.3)
                    #bottom=textGrob("Mycorrhiza type\n")
                    )


max = ggplot(indices, aes(x=myc, y=offset_maxima))+
  geom_boxplot(aes(group=myc), col="#801C1C", outliers = F)+
  geom_jitter(shape=21, fill="#C96666", col="#801C1C", width = 0.05)+
  geom_signif(comparisons=list(c("AMF", "EMF")), map_signif_level = TRUE, test = "t.test",
              tip_length=0, margin_top = -0.1, extend_line = -0.1)+
  labs(x = "Mycorrhiza type", y = "Offset of maxima (p97.5)")+
  theme_bw()+
  theme(plot.title = element_text(size=rel(0.9)))

min = ggplot(indices, aes(x=myc, y=offset_minima))+
  geom_boxplot(aes(group=myc), col="#801C1C", outliers = F)+
  geom_jitter(shape=21, fill="#C96666", col="#801C1C", width = 0.05)+
  geom_signif(comparisons=list(c("AMF", "EMF")), map_signif_level = TRUE, test = "t.test",
              tip_length=0, margin_top = -0.1, extend_line = -0.1)+
  labs(x = "Mycorrhiza type", y = "Offset of minima (p2.5)")+
  theme_bw()+
  theme(plot.title = element_text(size=rel(0.9)))

exts = grid.arrange(max, min, nrow = 1,
                    top=textGrob("\n(c) Extreme modulation", hjust = 1.4)
                    #bottom=textGrob("Mycorrhiza type\n")
                    )

all_plots = grid.arrange(avgs, vars, exts, ncol=1)
 
ggsave("plots/mydiv_indices.jpg", all_plots, width=2000, height=2800, unit="px")




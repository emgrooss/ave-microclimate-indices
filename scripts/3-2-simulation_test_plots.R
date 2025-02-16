rm(list=ls())

library(tidyverse)
library(gridExtra)
library(grid)


#### PLOTS AND LM

simresults_a = read.csv("1-data/1-2-microclimate-simulations/1-2-2-3-simulation-test_FINAL/simresults_a.csv", row.names = 1)
simresults_v = read.csv("1-data/1-2-microclimate-simulations/1-2-2-3-simulation-test_FINAL/simresults_v.csv", row.names = 1)
simresults_e = read.csv("1-data/1-2-microclimate-simulations/1-2-2-3-simulation-test_FINAL/simresults_e.csv", row.names = 1)


#function to create label for ggplot from lm
#Source: modified from https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

lm_eqn_r2 <- function(lm){
  eq1 <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R^2)~"="~r2,
                   list(a = format(unname(coef(lm)[1]), digits = 3),
                        b = format(unname(coef(lm)[2]), digits = 3),
                        r2 = format(summary(lm)$r.squared, digits = 2)))
  as.character(as.expression(eq));
}

lm_eqn <- function(lm){
  eq <- substitute(italic(y) == a + b %.% italic(x),
                    list(a = format(round( unname(coef(lm)[1]), 1)),
                         b = format(round( unname(coef(lm)[2]), 1)) ))
  as.character(as.expression(eq));
}

lm_r2 <- function(lm){
  r2 <- substitute(~~italic(R^2)~"="~r2,
                    list(r2 = format( round(summary(lm)$r.squared, 2) )))
  as.character(as.expression(r2));
}



# linear models for A
A_a = lm(d_a ~ a, data = simresults_a)
A_v = lm(d_a ~ v, data = simresults_v)
A_e = lm(d_a ~ e, data = simresults_e)

# plots for A
a1 = ggplot(simresults_a, aes(y=d_a, x=a))+
  geom_hline(yintercept=0)+
  geom_point(alpha=.5, shape=21, col="black")+
  #geom_smooth(method = "lm", col="#FF6262", linewidth=.5)+
  geom_abline(slope = A_a$coefficients["a"], intercept = A_a$coefficients["(Intercept)"], col="#FF6262", linewidth=.5)+
  geom_text(label = lm_eqn(A_a), parse=TRUE, x=0, y=10, col="#FF6262")+
  geom_text(label = lm_r2(A_a), parse=TRUE, x=0, y=8, col="#FF6262")+
  ylim(-10,10)+
  labs(x=expression(paste(italic("a"), " (input)")), y="A (result)",
       title = expression(paste("(a) A ~ ", italic("a"))) )+
  theme_classic()

a2 = ggplot(simresults_v, aes(y=d_a, x=v))+
  geom_hline(yintercept=0)+
  geom_point(alpha=.5, shape=21, col="black")+
  #geom_smooth(method = "lm", col="#40A0DD", linewidth=.5)+
  geom_abline(slope = A_v$coefficients["v"], intercept = A_v$coefficients["(Intercept)"], col="#40A0DD", linewidth=.5)+
  geom_text(label = lm_eqn(A_v), parse=TRUE, x=0, y=0.5, col="#40A0DD")+
  geom_text(label = lm_r2(A_v), parse=TRUE, x=0, y=0.4, col="#40A0DD")+
  ylim(-0.5,0.5)+
  labs(x=expression(paste(italic("v"), " (input)")), y="A (result)",
       title = expression(paste("(b) A ~ ", italic("v"))) )+
  theme_classic()
  
a3 = ggplot(simresults_e, aes(y=d_a, x=e))+
  geom_hline(yintercept=0)+
  geom_point(alpha=.5, shape=21, col="black")+
  #geom_smooth(method = "lm", col="#40A0DD", linewidth=.5)+
  geom_abline(slope = A_e$coefficients["e"], intercept = A_e$coefficients["(Intercept)"], col="#40A0DD", linewidth=.5)+
  geom_text(label = lm_eqn(A_e), parse=TRUE, x=0, y=1, col="#40A0DD")+
  geom_text(label = lm_r2(A_e), parse=TRUE, x=0, y=0.8, col="#40A0DD")+
  ylim(-1,1)+
  labs(x=expression(paste(italic("e"), " (input)")), y="A (result)",
       title = expression(paste("(c) A ~ ", italic("e"))) )+
  theme_classic()

#a_plots = grid.arrange(a1, a2, a3, nrow=1, top=textGrob("A = mean(micro) - mean(macro)\n", gp=gpar(fontface="italic")))
a_plots = grid.arrange(a1, a2, a3, nrow=1, top=textGrob("\n Effects on average modulation (A)", gp=gpar(fontface="bold")))

ggsave("1-data/1-2-microclimate-simulations/1-2-2-3-simulation-test_FINAL/a_plots.png", a_plots, width=3500, height=1400, unit="px")


#linear models for V
V_a = lm(d_v ~ a, data = simresults_a)
V_v = lm(d_v ~ v, data = simresults_v)
V_e = lm(d_v ~ e, data = simresults_e)

# plots for V
v1 = ggplot(simresults_a, aes(y=d_v, x=a))+
  geom_hline(yintercept=0)+
  geom_point(alpha=.5, shape=21, col="black")+
  #geom_smooth(method = "lm", col="#40A0DD", linewidth=.5)+
  geom_abline(slope = V_a$coefficients["a"], intercept = V_a$coefficients["(Intercept)"], col="#40A0DD", linewidth=.5)+
  geom_text(label = lm_eqn(V_a), parse=TRUE, x=0, y=10, col="#40A0DD")+
  geom_text(label = lm_r2(V_a), parse=TRUE, x=0, y=8, col="#40A0DD")+
  ylim(-10,10)+
  labs(x=expression(paste(italic("a"), " (input)")), y="V (result)",
       title = expression(paste("(d) V ~ ", italic("a"))) )+
  theme_classic()

v2 = ggplot(simresults_v, aes(y=d_v, x=v))+
  geom_hline(yintercept=0)+
  geom_point(alpha=.5, shape=21, col="black")+
  #geom_smooth(method = "lm", col="#FF6262", linewidth=.5)+
  geom_abline(slope = V_v$coefficients["v"], intercept = V_v$coefficients["(Intercept)"], col="#FF6262", linewidth=.5)+
  geom_text(label = lm_eqn(V_v), parse=TRUE, x=0, y=0.5, col="#FF6262")+
  geom_text(label = lm_r2(V_v), parse=TRUE, x=0, y=0.4, col="#FF6262")+
  ylim(-0.5,0.5)+
  labs(x=expression(paste(italic("v"), " (input)")), y="V (result)",
       title = expression(paste("(e) V ~ ", italic("v"))) )+
  theme_classic()

v3 = ggplot(simresults_e, aes(y=d_v, x=e))+
  geom_hline(yintercept=0)+
  geom_point(alpha=.5, shape=21, col="black")+
  #geom_smooth(method = "lm", col="#40A0DD", linewidth=.5)+
  geom_abline(slope = V_e$coefficients["e"], intercept = V_e$coefficients["(Intercept)"], col="#40A0DD", linewidth=.5)+
  geom_text(label = lm_eqn(V_e), parse=TRUE, x=0, y=1, col="#40A0DD")+
  geom_text(label = lm_r2(V_e), parse=TRUE, x=0, y=0.8, col="#40A0DD")+
  ylim(-1,1)+
  labs(x=expression(paste(italic("e"), " (input)")), y="V (result)",
       title = expression(paste("(f) V ~ ", italic("e"))) )+
  theme_classic()

#v_plots = grid.arrange(v1, v2, v3, nrow=1,
 #                      top=textGrob(expression(italic(paste("V = mean monthly ", frac("sd(micro)-sd(macro)","sd(macro)"),"\n"))))
  #                     )
v_plots = grid.arrange(v1, v2, v3, nrow=1, top=textGrob("\nEffects on variability modulation (V)", gp=gpar(fontface="bold")))

ggsave("1-data/1-2-microclimate-simulations/1-2-2-3-simulation-test_FINAL/v_plots.png", v_plots, width=3500, height=1400, unit="px")


#linear models for Emax
Emax_a = lm(d_emax ~ a, data = simresults_a)
Emax_v = lm(d_emax ~ v, data = simresults_v)
Emax_e = lm(d_emax ~ e, data = simresults_e)

#plots for d_emax
emax1 = ggplot(simresults_a, aes(y=d_emax, x=a))+
  geom_hline(yintercept=0)+
  geom_point(alpha=.5, shape=21, col="black")+
  #geom_smooth(method = "lm", col="#40A0DD", linewidth=.5)+
  geom_abline(slope = Emax_a$coefficients["a"], intercept = Emax_a$coefficients["(Intercept)"], col="#40A0DD", linewidth=.5)+
  geom_text(label = lm_eqn(Emax_a), parse=TRUE, x=1.5, y=10, col="#40A0DD")+
  geom_text(label = lm_r2(Emax_a), parse=TRUE, x=1.5, y=8, col="#40A0DD")+
  ylim(-10,10)+
  labs(x=expression(paste(italic("a"), " (input)")), y=expression(paste("E"[max]," (result)")),
       title = expression(paste("(g) E"[max]," ~ ", italic("a"))))+
  theme_classic()

emax2 = ggplot(simresults_v, aes(y=d_emax, x=v))+
  geom_hline(yintercept=0)+
  geom_point(alpha=.5, shape=21, col="black")+
  #geom_smooth(method = "lm", col="#40A0DD", linewidth=.5)+
  geom_abline(slope = Emax_v$coefficients["v"], intercept = Emax_v$coefficients["(Intercept)"], col="#40A0DD", linewidth=.5)+
  geom_text(label = lm_eqn(Emax_v), parse=TRUE, x=0.25, y=0.5, col="#40A0DD")+
  geom_text(label = lm_r2(Emax_v), parse=TRUE, x=0.25, y=0.4, col="#40A0DD")+
  ylim(-0.5,0.5)+
  labs(x=expression(paste(italic("v"), " (input)")), y=expression(paste("E"[max]," (result)")),
       title = expression(paste("(h) E"[max]," ~ ", italic("v"))))+
  theme_classic()

emax3 = ggplot(simresults_e, aes(y=d_emax, x=e))+
  geom_hline(yintercept=0)+
  geom_point(alpha=.5, shape=21, col="black")+
  #geom_smooth(method = "lm", col="#FF6262", linewidth=.5)+
  geom_abline(slope = Emax_e$coefficients["e"], intercept = Emax_e$coefficients["(Intercept)"], col="#FF6262", linewidth=.5)+
  geom_text(label = lm_eqn(Emax_e), parse=TRUE, x=0, y=1, col="#FF6262")+
  geom_text(label = lm_r2(Emax_e), parse=TRUE, x=0, y=0.8, col="#FF6262")+
  ylim(-1,1)+
  labs(x=expression(paste(italic("e"), " (input)")), y=expression(paste("E"[max]," (result)")),
       title = expression(paste("(i) E"[max]," ~ ", italic("e"))))+
  theme_classic()

#emax_plots = grid.arrange(emax1, emax2, emax3, nrow=1,
 #                         top=textGrob( expression(italic(paste("for micro > p"[95],
  #                                                              ": E"[max], " = mean ", frac("micro-macro",paste("p"[95], " - macro")) ))) )
   #                       )

emax_plots = grid.arrange(emax1, emax2, emax3, nrow=1, top=textGrob( expression(atop(,bold(paste("\nEffects on maxima modulation (E"[max],")")))) ))

ggsave("1-data/1-2-microclimate-simulations/1-2-2-3-simulation-test_FINAL/emax_plots.png", emax_plots, width=3500, height=1400, unit="px")


#linear models for Emin
Emin_a = lm(d_emin ~ a, data = simresults_a)
Emin_v = lm(d_emin ~ v, data = simresults_v)
Emin_e = lm(d_emin ~ e, data = simresults_e)

#plots for Emin
emin1 = ggplot(simresults_a, aes(y=d_emin, x=a))+
  geom_hline(yintercept=0)+
  geom_point(alpha=.5, shape=21, col="black")+
  #geom_smooth(method = "lm", col="#40A0DD", linewidth=.5)+
  geom_abline(slope = Emin_a$coefficients["a"], intercept = Emin_a$coefficients["(Intercept)"], col="#40A0DD", linewidth=.5)+
  geom_text(label = lm_eqn(Emin_a), parse=TRUE, x=-2, y=10, col="#40A0DD")+
  geom_text(label = lm_r2(Emin_a), parse=TRUE, x=-2, y=8, col="#40A0DD")+
  ylim(-10,10)+
  labs(x=expression(paste(italic("a"), " (input)")), y=expression(paste("E"[min]," (result)")),
       title = expression(paste("(j) E"[min]," ~ ", italic("a"))))+
  theme_classic()

emin2 = ggplot(simresults_v, aes(y=d_emin, x=v))+
  geom_hline(yintercept=0)+
  geom_point(alpha=.5, shape=21, col="black")+
  #geom_smooth(method = "lm", col="#40A0DD", linewidth=.5)+
  geom_abline(slope = Emin_v$coefficients["v"], intercept = Emin_v$coefficients["(Intercept)"], col="#40A0DD", linewidth=.5)+
  geom_text(label = lm_eqn(Emin_v), parse=TRUE, x=0.25, y=0.5, col="#40A0DD")+
  geom_text(label = lm_r2(Emin_v), parse=TRUE, x=0.25, y=0.4, col="#40A0DD")+
  ylim(-0.5,0.5)+
  labs(x=expression(paste(italic("v"), " (input)")), y=expression(paste("E"[min]," (result)")),
       title = expression(paste("(k) E"[min]," ~ ", italic("v"))))+
  theme_classic()

emin3 = ggplot(simresults_e, aes(y=d_emin, x=e))+
  geom_hline(yintercept=0)+
  geom_point(alpha=.5, shape=21, col="black")+
  #geom_smooth(method = "lm", col="#FF6262", linewidth=.5)+
  geom_abline(slope = Emin_e$coefficients["e"], intercept = Emin_e$coefficients["(Intercept)"], col="#FF6262", linewidth=.5)+
  geom_text(label = lm_eqn(Emin_e), parse=TRUE, x=0, y=1, col="#FF6262")+
  geom_text(label = lm_r2(Emin_e), parse=TRUE, x=0, y=0.8, col="#FF6262")+
  ylim(-1,1)+
  labs(x=expression(paste(italic("e"), " (input)")), y=expression(paste("E"[min]," (result)")),
       title = expression(paste("(l) E"[min]," ~ ", italic("e"))))+
  theme_classic()

#emin_plots = grid.arrange(emin1, emin2, emin3, nrow=1,
 #                         top=textGrob( expression(italic(paste("for micro < p"[5],
  #                                                              ": E"[min], " = mean ", frac("micro-macro",paste("p"[5], " - macro")) ))) )
   #                       )

emin_plots = grid.arrange(emin1, emin2, emin3, nrow=1, top=textGrob( expression(atop(,bold(paste("Effects on minima modulation (E"[min],")")))) ))

ggsave("1-data/1-2-microclimate-simulations/1-2-2-3-simulation-test_FINAL/emin_plots.png", emin_plots, width=3000, height=1000, unit="px")


all_plots = grid.arrange(a_plots, v_plots, emax_plots, emin_plots, ncol=1)
ggsave("1-data/1-2-microclimate-simulations/1-2-2-3-simulation-test_FINAL/all_plots.png", all_plots, width=3000, height = 4000, unit="px")

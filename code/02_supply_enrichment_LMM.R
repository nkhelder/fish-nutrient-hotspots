############################################################################
# Chapter 2: Quantifying fish-derived nutrient hotspots within the reefscape 
# a) Does nutrient supply from fishes influence nutrient enrichment? 
# b) How do gradients of complexity influence hotspots? 
###########################################################################

# Modelling the relationship between estimated nutrient supply from fishes  and macroalgal nutrient content


# Load Packages: 
library(MuMIn)
library(tidyverse)
library(ggeffects)
library(lme4)


# Load input data
# 1) Intra-reef structural complexity data from photogrammetry 
predictors <- read.csv(
  # "~/UofA/Thesis/r_projects/thesis_chapters/fish_habitat_models/data/predictors.sub.csv")
  "data/input/final.predictors.csv")

# 2) Estimated nutrient supply (mg/m2/day) from all fishes per plot combined
mean_supply <- read.csv("data/processed/pooled.predicted.nutrients.v2.csv") %>% 
  dplyr::select(-'X') %>%  
  ungroup() %>% 
  group_by(site, grid) %>% 
  # calculate average and SD supply (Nitrogen + phosphorous) across visits for each plot 
  dplyr::summarize(mean.N=mean(sum.N.m2, na.rm=TRUE),
                   sd.N=sd(sum.N.m2, na.rm=TRUE),
                   mean.P=mean(sum.P.m2, na.rm=TRUE),
                   sd.P=sd(sum.P.m2, na.rm=TRUE)) %>% 
  left_join(predictors) %>% 
  ungroup () %>% 
  dplyr::filter(site != "PR") %>% 
  dplyr::filter(site != "PC") %>% 
  drop_na()


# 3) Macroalgal tissue content data measured from samples at BASL
#     Data has been modified from raw BASL format to match up with each corresponding plot. 
NDR.fp <- read.csv("data/input/NDR.fp.csv") # 151 observations of 12 vars
CR.fp <- read.csv("data/input/CR.fp.csv") # 121 obs of 12 vars


# 4) Metadata - corresponding information about each plot survey
metadata <- read.csv("data/input/metadata.csv")



# Q2: Combined data (pooled for all hotspots)
#############################################
# data prep: 

# 96 N samples at CR
CR.N <- CR.fp %>% dplyr::select(-'P', -'date') %>% 
  drop_na() 

# 25 P samples at CR
CR.P <- CR.fp %>% dplyr::select(site, fp, transect, grid, distance, P) %>% 
  drop_na()


# 110 samples at NDR
NDR.N <- NDR.fp %>% dplyr::select(-'P', -'date')  %>% 
  drop_na()
# 41 P samples at NDR
NDR.P <- NDR.fp %>% dplyr::select(site, fp, transect, grid, distance, P) %>% 
  drop_na() 

# Combine for a total N dataframe: 
96+110 # total N observations: 206
dat.N <- NDR.N %>% rbind(CR.N) %>% left_join(mean_supply)

# Combine for a total P dataframe: 
25+41 # total P observations: 66
dat.P <- NDR.P %>% rbind(CR.P) %>% left_join(mean_supply)

# Clean up workspace: 
rm(CR.fp, NDR.fp)

##########################################################################################
# Modeling the relationship between predicted fish-derived nitrogen and macroalgal enrichment
##########################################################################################
# Linear Mixed Effects Model including a quadratic term to test for non-linear relationships. 
# Plot (focal point, or 'fp') nested within reefscape as random effects to control for sampling design. 

# Units: 
# N/P are %Nitrogen and %Phosphorous
# mean.N/P are mg/m2/day

# Data includes all Nitrogen samples (N=206; Reefscapes = Carysfort and North Dry Rocks)

# Check initial distributions
# FLAG
hist(~N, data=dat.N)
hist(~C, data=dat.N)
hist(~X13C, data=dat.N)
hist(~X15N, data=dat.N)

dat.N <- dat.N %>% drop_na()

# Total model (all sampling sites combined)

N.mod <- lmer(N ~ poly(mean.N,2) + (1|fp/site), data=dat.N)

# Explore random effects structure
# N.mod2 <- lmer(N ~ mean.N + (1|fp/site), data=dat.N)
# N.mod3 <- lmer(N ~ mean.N + (1|grid/fp/site), data=dat.N)
# N.mod4 <- lmer(N ~ mean.N + (1|grid/site), data=dat.N)
# N.mod5 <- lmer(N ~ poly(mean.N, 2) + (1|grid/site), data=dat.N)
# N.mod6 <- lmer(N ~ poly(mean.N,2)+ (1|grid/site), data=dat.N)
# N.mod7 <- lmer(N ~ poly(mean.N,2) + (1|grid/site) + (1|fp), data=dat.N)

# Compare AIC scores
# AIC(N.mod, N.mod2, N.mod3, N.mod4, N.mod5, N.mod6, N.mod7)

# Select lowest AIC scores and then choose simplest model. 5, 6, and 7 are all basically equal, 
# but mod6 is the simplest. 
# summary(N.mod6)
# r.squaredGLMM(N.mod6)

# Check for assumptions of best model
plot(N.mod)
abline(0,0)


# Combined NItrogen model visualization:  
pred.mm <- ggpredict(N.mod, terms = c("mean.N [all]"))  # this gives overall predictions for the model
N.combined <- ggplot(pred.mm) + 
  geom_line(aes(x = x, y = (predicted))) +          # slope
  geom_ribbon(aes(x = x, 
                  ymin = (predicted) - std.error, 
                  ymax = (predicted) + std.error), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  #geom_point(data = dat.N,                      # adding the raw data (scaled values)
  #          aes(x = mean.N, y = N)) + 
  geom_jitter(data=dat.N, aes(x=mean.N, 
                            y=N,
                            color=site,
                            shape=site), 
              size=2, width=0.1,height=0.1,alpha=0.8) +
  labs(x=expression(N~Supply~(mg~m^-2~day^-1)), 
       y="Macroalgae %N") +
  #title = "Effect of Vertical Relief on Predicting N (g/day) from reef rish") + 
  theme_classic() +
  scale_color_grey() +
 # annotate("text", x = 0.7, y=2.3, label = "italic(R) ^ 2 == 0.31", parse=TRUE) +
  guides(shape=guide_legend(title="Reefscape"),
         color=guide_legend(title="Reefscape"))
N.combined
# ggsave("output/N.percN.LM.v3.png")


# Final model results: 
r.squaredGLMM(N.mod)
summary(N.mod)


# Clean up:
rm(N.mod2, N.mod3, N.mod4, N.mod5, N.mod6, N.mod7, pred.mm, N.combined)




##########################################################################################
# Modeling the relationship between predicted fish-derived phosphorous and macroalgal enrichment
##########################################################################################
# Total Phosphorous model (all sampling points combined):
# LM:
dat.P <- dat.P %>% drop_na()


P.mod <- lmer(P ~ mean.P + (1|grid), data=dat.P)
# W/ polynomial
P.poly <- lmer(P ~ poly(mean.P,2) + (1|grid), data=dat.P)

summary(P.mod)
summary(P.poly)

r.squaredGLMM(P.mod)
r.squaredGLMM(P.poly)


# Polynomial model is a better fit (lower AIC)
AIC(P.mod, P.poly)
anova(P.mod, P.poly) # not significant?...

# LMM: 
P.mod <- lmer(P ~ mean.P + (1|grid), data=dat.P)
P.mod.poly <- lmer(P ~ poly(mean.P,2) + (1|grid), data=dat.P)

# Poly has lower AIC score, but not significant. 
AIC(P.mod, P.mod.poly)
anova(P.mod, P.mod.poly) # not different.

r.squaredGLMM(P.poly)


# Check model: 
plot(P.mod)
abline(0,0)
qqnorm(residuals(P.mod))

# Add Reference vs. hotspot column for labeling plots: ss=sampling site
# dat.P$ss <- 'Reference'
# dat.P$ss[dat.P$fp == "1"] <- "Hotspot"
# dat.P$ss <- as.factor(dat.P$ss)


pred.P <- ggpredict(P.poly, terms = c("mean.P [all]")) 
P.combined <- ggplot(pred.P) + 
  geom_line(aes(x = x, y = (predicted))) +    
  geom_ribbon(aes(x = x, ymin = (predicted) - std.error, 
                  ymax = (predicted) + std.error), 
              fill = "lightgrey", alpha = 0.5) +
  #geom_point(data = dat.N,                   
  #          aes(x = mean.N, y = N)) + 
  # add some random noise to the data points: 
  geom_jitter(data=dat.P, aes(x=mean.P, y=P,
                                   shape=site, 
                              color=site), 
              size=2, 
              width=0.05, # noise
              height=0.05, # noise
              alpha=0.8) + 
  labs(x=expression(P~Supply~(mg~m^-2~day^-1)), 
       y="Macroalgae %P") +
  theme_classic() +
  scale_color_grey() +
  # annotate("text", x = 0.05, y=0.8, label = "italic(R) ^ 2 == 0.25", parse=TRUE) + # write R2 
  guides(shape=guide_legend(title="Reefscape"),
         colour=FALSE) # remove legend for color (i.e. hotspot vs. reference)
P.combined
ggsave("output/Combined.P.LMM.v3.png")


# One model for NDR + CR separately: separate dataframes
NDR.P <- NDR.P %>% left_join(mean_supply)
CR.P <- CR.P %>% left_join(mean_supply)


# NDR Phosphorous model: linear model is better than polynomial: 
P.mod <- lmer(P ~ mean.P + (1|grid), data=NDR.P)
P.poly <- lmer(P ~ poly(mean.P,2) + (1|grid), data=NDR.P)
summary(P.mod)
summary(P.poly)

AIC(P.mod, P.poly)
plot(P.mod)

r.squaredGLMM(P.poly)

pred.P <- ggpredict(P.poly, terms = c("mean.P [all]")) 
NDR.P.plot <- ggplot(pred.P) + 
  geom_line(aes(x = x, y = (predicted))) +    
  geom_ribbon(aes(x = x, ymin = (predicted) - std.error, 
                  ymax = (predicted) + std.error), 
              fill = "lightgrey", alpha = 0.5) +
  #geom_point(data = dat.N,                   
  #          aes(x = mean.N, y = N)) + 
  geom_jitter(data=NDR.P, aes(x=mean.P, y=P,
                              shape=site), 
              size=2, width=0.05,height=0.05,alpha=0.8) +
  scale_shape_manual(values=17)+
  labs(x=expression(P~Supply~(mg~m^-2~day^-1)), 
       y="Macroalgae %P") +
  #title = "Effect of Vertical Relief on Predicting N (g/day) from reef rish") + 
  theme_classic() +
  scale_color_grey() +
  # annotate("text", x = 0.05, y=0.75, label = "italic(R) ^ 2 == 0.28", parse=TRUE) +
  guides(shape=guide_legend(title="Site")) +
  theme(legend.position = "none")
# color=guide_legend(title="Location"))

NDR.P.plot
ggsave("output/NDR.P.LM.v2.png")


# FLAG
# C
P.cr <- lm(P ~ mean.P, data=CR.P)
P.poly <- lm(P ~ poly(mean.P,2), data=CR.P)
summary(P.cr)


AIC(P.cr, P.poly)
plot(P.cr)
r.squaredGLMM(P.cr)
fix(p.cr)
# This one is crazy. Don't do this. 
pred.P <- ggpredict(P.cr, terms = c("mean.P")) 
P.cr.plot <- ggplot(pred.P) + 
  geom_line(aes(x = x, y = (predicted))) +    
  geom_ribbon(aes(x = x, ymin = (predicted) - std.error, 
                  ymax = (predicted) + std.error), 
              fill = "lightgrey", alpha = 0.5) +
  geom_point(data = CR.P,                   
             aes(x = mean.P, y = P, shape=site)) + 
  #geom_jitter(data=p.cr, aes(x=mean.P, y=P), 
  #           size=2, width=0.1,height=0.1,alpha=0.8) +
  labs(x=expression(P~Supply~(mg~m^-2~day^-1)), 
       y="Macroalgae %P") +
  theme_classic() +
  scale_color_grey() +
  # annotate("text", x = 0.05, y=0.8, label = "italic(R) ^ 2 == 0.18", parse=TRUE) +
  guides(shape=guide_legend(title="Site")) +
  theme(legend.position = "none")
# color=guide_legend(title="Location"))
P.cr.plot
dev.off()
ggsave("P.CR.LM.v2.png")


cowplot::plot_grid(
  NDR.P.plot+ theme(legend.position="none"),
  P.cr.plot + theme(legend.position="none"),
  align='vh',
  labels=c('C', 'D'),
  # hjust=-1,
  nrow=1,
  rel_widths = c(1, 1, 1, 1))
# ggsave("Cowplot.NDR.P.CR.v2.png")

# Combine plots:
cowplot::plot_grid(
  N.combined + theme(legend.position="none"),
  P.combined + theme(legend.position="none"),
  align='vh',
  labels='AUTO',
  # hjust=-1,
  nrow=1,
  rel_widths = c(1, 1, 1, 1))



# ggsave("Cowplot.N.P.LMM.v2.png", width=8, height=4.5, units=c("in"))
####################################################

# write.csv(dat.N, "q2.csv")
# write.csv(dat.P, "q2.P.csv")


#############################################
# Appendix Figures: Modeling each reefscape separately 
#############################################
# Isolate NDR fp 1
dat.N.subset.ndr <- dat.N %>% dplyr::filter(site == "NDR")
dat.N.subset.ndr1 <- dat.N.subset.ndr %>% dplyr::filter(fp == "1") # df with just NDR 1
dat.N.subset.ndr <- dat.N.subset.ndr %>% dplyr::filter(fp == "2")
dat.N.subset <- dat.N %>% dplyr::filter(site != "NDR") %>% 
  rbind(dat.N.subset.ndr)


# 3 fps without NDR fp 1: 11.9% variance explained (site did not account for anything)
N.mod.sub <- lmer(N~ mean.N + (1|grid/site), data=dat.N.subset)
summary(N.mod.sub)
r.squaredGLMM(N.mod.sub)

plot(N.mod.sub)
abline(0,0)

# FLAG: code keeps crashing here. 
pred.sub <- ggpredict(N.mod.sub, terms = c("mean.N"))  # this gives overall predictions for the model
# Plot the predictions : this looks better overall. 
subset <- ggplot(pred.sub) + 
  geom_line(aes(x = x, y = (predicted))) +          # slope
  geom_ribbon(aes(x = x, ymin = (predicted) - std.error, ymax = (predicted) + std.error), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  #geom_point(data = dat.N,                      # adding the raw data (scaled values)
  #          aes(x = mean.N, y = N)) + 
  geom_jitter(data=dat.N.subset, aes(x=mean.N, y=N,
                                     color=as.factor(site), shape=site), 
              size=2, width=0.1,height=0.1,alpha=0.8) +
  labs(x=expression(N~Supply~(mg~m^-2~day^-1)), 
       y="Macroalgae %N") +
  #title = "Effect of Vertical Relief on Predicting N (g/day) from reef rish") + 
  theme_classic() +
  scale_color_grey() +
  # annotate("text", x = 0.2, y=2.15, label = "italic(R) ^ 2 == 0.14", parse=TRUE) +
  guides(shape=guide_legend(title="Reefscape"),
         color=guide_legend(title="Reefscape")) +
  theme(legend.position="none")
subset  

#ggsave("Subset.N.percN.LMM.v3.png")


# JUST NDR1 MODEL:
ndr1.mod <- lmer(N ~ poly(mean.N,2) + (1|grid), data=dat.N.subset.ndr1)
summary(ndr1.mod)
plot(ndr1.mod)

r.squaredGLMM(ndr1.mod)



# Get model predictions for just NDR 1 model: use [all] to grab polynomial terms
pred.ndrfp1 <- ggpredict(ndr1.mod, terms = c("mean.N [all]")) 
ndrfp1 <- ggplot(pred.ndrfp1) + 
  geom_line(aes(x = x, y = (predicted))) +          # slope
  geom_ribbon(aes(x = x, ymin = (predicted) - std.error, ymax = (predicted) + std.error), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  #geom_point(data = dat.N,                      # adding the raw data (scaled values)
  #          aes(x = mean.N, y = N)) + 
  geom_jitter(data=dat.N.subset.ndr1, aes(x=mean.N, y=N,
                                          color=as.factor(site), 
                                          shape=site), 
              size=2, width=0.4,height=0.1,alpha=0.8, shape=17) +
  labs(x=expression(N~Supply~(mg~m^-2~day^-1)), 
       y="Macroalgae %N") +
  theme_classic() +
  scale_color_grey() +
  # annotate("text", x = 1.4, y=1.85, label = "italic(R) ^ 2 == 0.49", parse=TRUE) +
  theme(legend.position="none") 

ndrfp1

# ggsave("NDR.fp1.N.percN.lm.plot.v3.png")


# Figure 3A.1: Showing NDR1 fp compared to CR1 fp
cowplot::plot_grid(ndrfp1, subset,
                   align="h",
                   labels="AUTO")
# ggsave("cowplot.ndrfp1.subset.v3.png")
#########################################








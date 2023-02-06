############################################################################
# Quantifying fish-derived nutrient hotspots within the reefscape: 
# Does intra-habitat structural complexity influence fish-derived nutrient supply? 
# By: Noelle Helder
# Date: February 3, 2023
############################################################################

# --------------------------------------
# SCRIPT OBJECTIVES: 
#  This script fits LMMs to estimated fish nutrient supply (N & P) as a function of 
#     habitat complexity (relief + VRM + VRM dev)


# OUTPUTS: 
#   - Model table: N & P ~ complexity - "output/supply.complexity.lmm.table.doc"
#   - Visual: LMM interaction terms -    ggsave("output/P.VRM.Relief.Interaction.plot.png")
#                                        ggsave("output/N.VRM.Relief.Interaction.plot.png")
# --------------------------------------


# --------------------------------------
# Packages used: 
library(tidyverse) # general
library(sjPlot) # plot effects 
library(stargazer) # model table outputs
library(lme4) # lme
library(DHARMa) # residuals 
library(ggeffects) # visualize

# Set this before using dredge. 
options(na.action = "na.fail")
# --------------------------------------


# --------------------------------------
# Load dataframe: 
# Average nutrient supply dataframe: 
# q1.df.mean <- read.csv("N.P.mean.estimates.v2.csv") %>% 
#   dplyr::select(-'X')
complexity <- read.csv(
  # "~/UofA/Thesis/r_projects/thesis_chapters/fish_habitat_models/data/predictors.sub.csv")
     # "data/input/predictors.sub.csv")
       "data/input/final.predictors.csv")

# Estimate average supply: 
mean_supply <- read.csv("data/processed/pooled.predicted.nutrients.v2.csv") %>% 
  dplyr::select(-'X') %>%  
  ungroup() %>% 
  group_by(site, grid) %>% 
  dplyr::summarize(mean.N=mean(sum.N.m2, na.rm=TRUE),
            sd.N=sd(sum.N.m2, na.rm=TRUE),
            mean.P=mean(sum.P.m2, na.rm=TRUE),
            sd.P=sd(sum.P.m2, na.rm=TRUE)) %>% 
  left_join(complexity) %>% 
  drop_na()

rm(complexity)
# --------------------------------------

# --------------------------------------
# Prep data for modelling: 
# --------------------------------------
# Drop Pickles and Pickles Control, as these are not comparable with the rest of the sites, 
# and not of particular interest. 542 obs of 13 vars. older code says 560.. lets keep looking. 
# q1.df.mean$site <- as.character(q1.df.mean$site)
mean_supply <- mean_supply %>% ungroup () %>% 
  dplyr::filter(site != "PR") %>% 
  dplyr::filter(site != "PC") 

# Rename for less typing
df <- mean_supply
str(df)
df$relief <- df$height_diff
df <- df %>% 
  filter(relief < '4')


# Scale and center continuous complexity: 
df$vrm_1cm_sc <- scale(df$vrm_1cm,center = T, scale = T)
df$vrm_dev_sc <- scale(df$vrm_dev,center = T, scale = T)
df$relief_sc <- scale(df$relief,center = T, scale = T)

# FLAG: 
# Check response distributions (estimated N and P), and w/ transformations
# hist(~mean.N, data=df, xlab="Predicted Nitrogen", w=0.25)
# hist(~log(mean.N), data=df, xlab="Predicted Nitrogen", w=0.25)
# hist(~mean.P, data=df, xlab="Predicted Phosphorous", w=0.02)
# hist(~log(mean.P), data=df, xlab="Predicted Phosphorous", w=0.05) # a bit wonky, but better. 
# --------------------------------------

# --------------------------------------
# Use Linear Mixed Effects Model to quantify relationship between 
#     estimated nitrogen from fish and intra-reef structural complexity (relief, vrm, vrm_dev)
#     Site (reefscape) is a random effect. 
# --------------------------------------

# Initial model is poorly fit 
N.lmm1 <- lmer(log(mean.N) ~ relief*vrm_1cm_sc + relief*vrm_dev_sc +
                      (1|site), REML=FALSE, 
                    data=df)
plot(simulateResiduals(N.lmm1))

# Test with polynomial terms for non linearity. 
# Significantly better fit w/ a polynomial term for relief: 
N.lmm2 <- lmer(log(mean.N) ~ poly(relief,2)*poly(vrm_1cm_sc,2) + 
                 poly(relief,2)*poly(vrm_dev_sc,2) +
                   (1|site), REML=FALSE, 
                 data=df)
plot(simulateResiduals(N.lmm2))
r.squaredGLMM(N.lmm2)

# This model explains 21% of the variation due to structural metrics; 27% of the variation 
# w/ the whole model (site effect)


# Model selection w/ DREDGE and AIC: 
results <- dredge(N.lmm2)
subset(results, delta<2)

# Refit top model to check for need for polynomial terms by removing one at a time and 
# comparing the AIC scores. 
lmm.best <- lmer(log(mean.N) ~ poly(relief,2)*poly(vrm_dev_sc,2) + 
                   poly(vrm_1cm_sc,2) +
                   (1|site), REML=FALSE, data=df)

lmm.best.comp <- lmer(log(mean.N) ~ poly(relief,2)*poly(vrm_dev_sc,2) + 
                   vrm_1cm_sc +
                   (1|site), REML=FALSE, data=df)
lmm.best.comp2 <- lmer(log(mean.N) ~ poly(relief,2)*vrm_dev_sc + 
                        poly(vrm_1cm_sc,2) +
                        (1|site), REML=FALSE, data=df)

lmm.best2 <- lmer(log(mean.N) ~ poly(relief,2) + vrm_dev_sc + 
                   (vrm_1cm_sc) +
                   (1|site), REML=FALSE, data=df)

# not different, go with simpler model that includes only relief. 
AIC(lmm.best, lmm.best.comp, lmm.best.comp2,  lmm.best2) 


lmm.best <- lmer(log(mean.N) ~ poly(relief,2)*vrm_dev_sc + 
                   (vrm_1cm_sc) +
                   (1|site), REML=FALSE, data=df)

# Check fit: 
plot(lmm.best)
r.squaredGLMM(lmm.best)

# Check residuals against each covariate: 
plot(residuals(lmm.best) ~ df$relief, data=df)
abline(0,0)

plot(residuals(lmm.best) ~ df$vrm_1cm, data=df)
abline(0,0)

plot(residuals(lmm.best) ~ df$vrm_dev, data=df)
abline(0,0)

summary(lmm.best)
# --------------------------------------


# --------------------------------------
# Phosphorous Model: 
# --------------------------------------
P.lmm <- lmer(log(mean.P) ~ relief*vrm_1cm_sc + relief*vrm_dev_sc +
                   (1|site), REML=FALSE, 
                 data=df)
plot(simulateResiduals(P.lmm))

# Significantly better fit w/ a polynomial term for relief: 
P.lmm2 <- lmer(log(mean.P) ~ poly(relief,2)*vrm_1cm_sc + poly(relief,2)*vrm_dev_sc +
                   (1|site), REML=FALSE, 
                 data=df)
P.lmm3 <- lmer(log(mean.P) ~ poly(relief,2)*poly(vrm_1cm_sc,2) + poly(relief,2)*vrm_dev_sc +
                 (1|site), REML=FALSE, 
               data=df)
P.lmm4 <- lmer(log(mean.P) ~ poly(relief,2)*vrm_1cm_sc + poly(relief,2)*poly(vrm_dev_sc,2) +
                 (1|site), REML=FALSE, 
               data=df)
P.lmm5 <- lmer(log(mean.P) ~ relief*poly(vrm_1cm_sc,2) + relief*vrm_dev_sc +
                 (1|site), REML=FALSE, 
               data=df)
P.lmm6 <- lmer(log(mean.P) ~ relief*vrm_1cm_sc + relief*poly(vrm_dev_sc,2) +
                 (1|site), REML=FALSE, 
               data=df)
P.lmm7 <- lmer(log(mean.P) ~ relief*poly(vrm_1cm_sc,2) + relief*poly(vrm_dev_sc,2) +
                 (1|site), REML=FALSE, 
               data=df)
P.lmm8 <- lmer(log(mean.P) ~ poly(relief,2)*poly(vrm_1cm_sc,2) + poly(relief,2)*poly(vrm_dev_sc,2) +
                 (1|site), REML=FALSE, 
               data=df)
AIC(P.lmm, P.lmm2, P.lmm3, P.lmm4, P.lmm5, P.lmm6, P.lmm7, P.lmm8) # better w/ quadratic term


# Model w/ polynomial term for relief is the best (P.lmm2) and simplest
plot(simulateResiduals(P.lmm2))

# This model explains 14.6% of the variation due to structural metrics; 21% of the variation 
# w/ the whole model (site effect)
r.squaredGLMM(P.lmm2) # explains 21% of the variation in P, 28% of the var overall
summary(P.lmm2)

# Model selection w/ DREDGE and AIC: 
results <- dredge(P.lmm2)
subset(results, delta<2)


P.best <- lmer(log(mean.P) ~ poly(relief,2)*vrm_dev_sc + vrm_1cm_sc +
                   (1|site), REML=FALSE, data=df)
P.drop.int <- lmer(log(mean.P) ~ poly(relief,2) + vrm_1cm_sc + vrm_dev_sc +
                 (1|site), REML=FALSE, data=df)
P.drop.vrm <- lmer(log(mean.P) ~ poly(relief,2)  +
                     (1|site), REML=FALSE, data=df)
# Keep P.best
AIC(P.best, P.drop.int, P.drop.vrm)

# Check residuals against each covariate: 
plot(residuals(P.best) ~ df$relief, data=df)
abline(0,0)

plot(residuals(P.best) ~ df$vrm_1cm, data=df)
abline(0,0)
# --------------------------------------


# --------------------------------------
# Model table for Nitrogen, Phosphorous, and structural complexity: 
# --------------------------------------
# summary(lmm.best)
# summary(P.best)

stargazer(lmm.best, P.best, 
          type="html",
          ci= TRUE, ci.separator = " - ",
          column.labels = c("N", "P"),
          digits = 2,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "",
          covariate.labels=c("Relief", "Relief(2)","VRM Dev", "VRM (1cm)", "Relief*VRM Dev",
                             "Relief(2)*VRM Dev"),
          omit.table.layout = "sn",
          out="output/supply.complexity.lmm.table.doc")
# --------------------------------------


# --------------------------------------
# Visualize model interaction terms 
# --------------------------------------
P.int <- plot_model(P.best, type = "pred", 
                    terms = c("vrm_dev_sc","relief")) +
   theme_classic() +
   scale_color_sjplot("viridis", labels=c("Low", "Medium", "High")) +
   scale_fill_sjplot("viridis") +
   ggplot2::labs(colour = "Relief") +
   labs(x="VRM Deviation", y=expression(ln~Mean~P~Supply~(g~m^-2~day^-1)))
 # ggsave("N.VRM.Relief.Interaction.plot.png")
P.int
ggsave("output/P.VRM.Relief.Interaction.plot.png")
 

N.int <- plot_model(lmm.best, type = "pred", 
                     terms = c("vrm_dev_sc","relief")) +
    theme_classic() +
    scale_color_sjplot("viridis", labels=c("Low", "Medium", "High")) +
    scale_fill_sjplot("viridis") +
    ggplot2::labs(colour = "Relief") +
    labs(x="VRM Deviation", y=expression(ln~Mean~P~Supply~(g~m^-2~day^-1)))N.int

ggsave("output/N.VRM.Relief.Interaction.plot.png")
# --------------------------------------
 

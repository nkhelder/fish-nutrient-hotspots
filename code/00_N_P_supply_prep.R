############################################################################
# Quantifying fish-derived nutrient hotspots within the reefscape 
# By: Noelle Helder
# Date: February 3, 2023
############################################################################

# Objectives: clean original fish data and create all processed datasets used in the analysis. 
# 00: Clean fish data and prep for analysis
# 01: Estimate and export nutrient supply per plot from bioenergetics models from Burkepile et al. 2013
# 02: Export Carysfort summary for visualization: biomass, N supply, P supply, and vertical relief
# 03: Export Figure 3.2: Carysfort Reef subset showing relief, biomass, N supply, and P supply 



# OUTPUTS: 
# Final estimated nutrient supply from fishes per plot:  write.csv(df, "pooled.predicted.nutrients.v2.csv")
# Caysfort summary:   write.csv(test, "data/processed/carysfort_means.csv") 
# Carysfort summary vis:  ggsave("output/carysfort.png")


# Packages:
library(tidyverse) # data management and manipulation 
library(viridis) # colors


# Load data inputs: 
#####################
# fish observational survey data:
fish <- read_csv(
  # "~/UofA/Thesis/r_projects/thesis_chapters/fish_hotspots_ch2/data/clean/fish_data_clean_v3.csv")
  "data/input/fish_data_clean_v3.csv")

# list of species information from Fish Base - used to estimate biomass: 
species <- read_csv(
  # "~/UofA/Thesis/r_projects/thesis_chapters/fish_hotspots_ch2/data/clean/species_list_clean_v1.csv")
  "data/input/species_list_clean_v1.csv") %>% 
  dplyr::select(-'...1')

# metedata information for each plot/visit:
metadata <- read_csv(
  # "~/UofA/Thesis/r_projects/thesis_chapters/fish_habitat_models/data/metadata.csv") 
  "data/input/metadata.csv") %>% 
  dplyr::select(-'...1')

# habitat data for each plot estimated from photogrammetry: 
predictors <- read.csv(
  # "~/UofA/Thesis/r_projects/thesis_chapters/fish_habitat_models/data/predictors.sub.csv")
   "data/input/final.predictors.csv")


##########################################################################################
# 00: Clean fish data and prep for further analysis
##########################################################################################
# Label grids 1-9 as 01, 02...09. 
temp <- fish
temp$grid[temp$grid %in% "1"] <- "01"
temp$grid[temp$grid %in% "2"] <- "02"
temp$grid[temp$grid %in% "3"] <- "03"
temp$grid[temp$grid %in% "4"] <- "04"
temp$grid[temp$grid %in% "5"] <- "05"
temp$grid[temp$grid %in% "6"] <- "06"
temp$grid[temp$grid %in% "7"] <- "07"
temp$grid[temp$grid %in% "8"] <- "08"
temp$grid[temp$grid %in% "9"] <- "09"
fish <- temp

# Confirm grids look good: values of 01 - 100
table(fish$grid)


# Fix data entry issues to match with species list
# is.na(fish)
# NAs to correct for: grouper sp., pompano sp., Porgy sp. 
fish[fish == "Grouper sp."] <- "Black Grouper" #unknown grouper? use data for black grouper
fish[fish == "Pompano sp."] <- "Florida Pompano" #unknown species coded as Pompano. 
fish[fish == "Porgy sp."] <- "Saucereye Porgy" #unknown porgy? use data for Saucereye porgy
fish[fish == "Chub sp."] <- "Brassy chub" #unknown chub? use data for brassy chub. 
fish[fish == "Slippery dick"] <- "Slippery Dick" #rename as needed to align with species list.


# Exclude species that are not part of analysis but were observed during surveys. 
# Includes pelagic species, sharks/rays, max body sizes <15 cm. 
filter_fish <- fish %>% 
  dplyr::filter(common_name != "Reef shark") %>% 
  dplyr::filter(common_name != "Nurse Shark") %>% 
  dplyr::filter(common_name != "Tarpon") %>% 
  dplyr::filter(common_name != "Southern Stingray") %>% 
  dplyr::filter(common_name != "Yellow Stingray") %>% 
  dplyr::filter(common_name != "Permit") %>% 
  dplyr::filter(common_name != "Florida Pompano") %>% 
  dplyr::filter(common_name != "Brown Chromis") %>% 
  dplyr::filter(common_name != "Blue Chromis") %>% 
  dplyr::filter(common_name != "Yellowtail Damselfish") %>% 
  dplyr::filter(common_name != "Yellowtail Hamlet") %>% 
  dplyr::filter(common_name != "Blue Hamlet") %>% 
  dplyr:: filter(common_name != "Black Hamlet") %>% 
  dplyr::filter(common_name != "Barred Hamlet") %>% 
  dplyr::filter(common_name != "Butter Hamlet") %>% 
  dplyr::filter(common_name != "Mackerel Scad") %>% # only seen once
  dplyr::filter(common_name != "Blue spotted cornetfish") %>%  # only seen once + not included in sp. matrix
  dplyr::filter(common_name != "Yellow Jack")


fish <- filter_fish %>% #3 2624 observations of 17 vars
  left_join(metadata) 
rm(filter_fish)

# Check common names for spelling, weird occurrences, etc. 
# 82 unique species observed that meet survey criteria. 
unique(fish$common_name) 
length(unique(fish$common_name)) 

# Link species name with species info from FishBase spreadsheet 
dat <- merge(fish, species, by.x = "common_name", 
             by.y = "common_name", all.x = TRUE, all.y = FALSE)


# 23 families observed. 
length(unique(dat$family))

# Fix incorrect a and b values for Queen Parrotfish (as found during herbivory analysis)
dat$a <- ifelse(dat$common_name %in% 'Queen Parrotfish', 0.0144,dat$a)
dat$b <- ifelse(dat$common_name %in% 'Queen Parrotfish', 3.05, dat$b)

# Remove all observations less than 15 cm from the analysis 
dat <- dat %>% 
  dplyr::filter(total_length >= 15)

# Make transit a binary category to drop or include as needed later on.
dat$transit[dat$transit == 'transit'] <- "1"
dat$transit[is.na(dat$transit)] <- 0

# FLAG: Calculate fish mass (g), mean = 10.27 for an individual
dat$biomass <-(dat$a*(dat$total_length^dat$b))
summary(dat$biomass) # largest fish are barracuda (~800 g/m2) and black grouper (~500)



# Cleaned individual fish biomass dataframe for all sites, grids, visits
df <- dat %>% dplyr::select(site, grid, visit, family, common_name, species_name, transit,
                            biomass, x, y, outplant)

levels(as.factor(df$family))



##########################################################################################
# 01: Estimate and export nutrient supply per plot from bioenergetics models from Burkepile et al. 2013
##########################################################################################
### Nitrogen Formulas from Burkepile et. al. for fish families
nutrients <- df # df with individual fish biomass estimates
nutrients$ID <- seq.int(nrow(nutrients))

#ACANTHURIDAE: 
acanthuridae <- nutrients %>% 
  dplyr::filter(family == "Acanthuridae") #
acanthuridae$nitrogen <- (0.00019)*(acanthuridae$biomass) + 0.0011
acanthuridae$phosphorous <- (0.000012)*(acanthuridae$biomass)

#LABRIDAE: wrasses (193)
labrid <- nutrients %>% 
  dplyr::filter(family == "Labridae")
labrid$nitrogen <- (0.00032)*(labrid$biomass) + 0.22
labrid$phosphorous <- (0.000052)*(labrid$biomass) + 0.0016

#HAEMULIDAE: 
haemulidae <- nutrients %>% 
  filter(family == "Haemulidae")
haemulidae$nitrogen <- (0.00027)*(haemulidae$biomass) + 0.011
haemulidae$phosphorous <- (0.000029)*(haemulidae$biomass) + 0.00013

#HOLOCENTRIDAE
holocentridae <- nutrients %>% 
  filter(family == "Holocentridae")
holocentridae$nitrogen <- (0.00029)*(holocentridae$biomass) + 0.011
holocentridae$phosphorous <- (0.000047)*(holocentridae$biomass) 

#kyphosidae:
kyphosidae <- nutrients %>% 
  filter(family == "Kyphosidae")
kyphosidae$nitrogen <- (0.000087)*(kyphosidae$biomass) + 0.0031
kyphosidae$phosphorous <- (0.0000058)*(kyphosidae$biomass) 

#LUTJANIDAE: 
lutjanidae <- nutrients %>% 
  filter(family == "Lutjanidae")
lutjanidae$nitrogen <- (0.00032)*(lutjanidae$biomass) + 0.022
lutjanidae$phosphorous <- (0.000097)*(lutjanidae$biomass) + 0.0061

#MULLIDAE: 
mullidae <- nutrients %>% 
  filter(family=="Mullidae")
mullidae$nitrogen <- (0.00035)*mullidae$biomass + 0.028
mullidae$phosphorous <- (0.000039)*mullidae$biomass + 0.0021

#SCARIDAE: 
scaridae <- nutrients %>%  
  filter(family == "Scaridae")
scaridae$nitrogen <- (0.00011)*(scaridae$biomass)
scaridae$phosphorous <- (0.0000072)*(scaridae$biomass)

#SERRANIDAE: 
serranidae <- nutrients %>%  
  filter(family == "Serranidae")
serranidae$nitrogen <- (0.00016)*(serranidae$biomass) + 0.014
serranidae$phosphorous <- (0.000058)*(serranidae$biomass) + 0.0062


# Combine into a single dataset that represents the g of nitrogen and phosphorous 
# excreted per day by each individual fish observed on our sureys: 
nutrient_predict <- acanthuridae %>% 
  rbind(labrid) %>% 
  rbind(haemulidae) %>% 
  rbind(holocentridae) %>% 
  rbind(kyphosidae) %>% 
  rbind(lutjanidae) %>% 
  rbind(mullidae) %>% 
  rbind(serranidae) %>% 
  rbind(scaridae) #total dataset with expected NITROGEN excretion. 


# While this isn't every family that we observed, this accounts for 96% of all fish surveyed
# Check this by dividing length of df (fish observations) by length of nutrient_predict (individuals in families with
# bioenergetics models)
30782/32034 

# Angelfish (Pomacanthidae) are probably the largest group not captured in this analysis.  
table(as.factor(df$family))


# Sum the nutrient supply for each individual fish in each plot ('grid') per visit
site.predict <- nutrient_predict %>% group_by(site, grid, visit) %>% 
  dplyr::summarize(sum.nitrogen=sum(nitrogen), 
                   sum.phosphorous=sum(phosphorous))

# Estimate  N and P supply per m2 per day for each plot and convert to mg
site.predict$sum.N.m2 <- (site.predict$sum.nitrogen/25)*1000 # convert to mg
site.predict$sum.P.m2 <- (site.predict$sum.phosphorous/25)*1000 # convert to mg

# ~max = 0.9 g N/m2/day = 900 mg (!) m2 day
# ~mean = 0.017 = 17 mg N/m2/day
summary(site.predict$sum.N.m2)

##### EXTRA
# For fun: check nutrient excretion at the reefscape (2500 m2) level
# Site level: 
# site.area.predict <- nutrient_predict %>% dplyr::group_by(site, visit) %>% 
# dplyr::summarize(sum.site.nitrogen=sum(nitrogen), 
#      sum.site.phosphorous=sum(phosphorous))
# site.area.predict$sum.N.m2 <- (site.area.predict$sum.site.nitrogen/2500)*1000 # convert to mg
# site.area.predict$sum.P.m2 <- (site.area.predict$sum.site.phosphorous/2500)*1000 # convert to mg

# max = 0.04 g N/m2/day = 40 mg per m2 per day at the site level.
# mean = 0.013 g N/m2/day = 13 mg per m2 per day at the site level
# This is actually much lower than the 35 mg per m2 per day at the site level estimated
# in Burkepiles 2013 paper. Suggests that these hotspots really are different from the 
# surrounding areas. 
# summary(site.area.predict$sum.N.m2)
##### END EXTRA

# Add in missing 0s: total of 4308 observations should be included
site.predict.df <- site.predict %>% ungroup() %>% 
  complete(nesting(site, grid), visit,fill=list(sum.N.m2=0))
site.predict.df$sum.P.m2[is.na(site.predict.df$sum.P.m2)] <- 0
site.predict.df$uniqueid <- paste(site.predict.df$site, site.predict.df$visit,  sep="_") # add unique ID col  

# drop surveys that do not exist: 
df <- site.predict.df %>% # df should have 4308 observations total
  dplyr::filter(uniqueid != "PC_5") %>% # PC only had 4 visits (drop 5 & 6)
  dplyr::filter(uniqueid != "PC_6") %>% 
  dplyr::filter(uniqueid != "PR_6") %>% # PR only had 5 visits
  dplyr:: filter(uniqueid != "GC_6") %>%    # GC only had 5 visits; total = 25,848 (=4308*6 groups)
  dplyr::select(-'uniqueid')


# Final estimated nutrient supply from fishes: 
write.csv(df, "data/processed/pooled.predicted.nutrients.v2.csv")

##########################################################################################
# 02: Export Carysfort summary for visualization: biomass, N supply, P supply, and vertical relief
##########################################################################################
# Create the means dataset that I will use for visualization later on:
# calculate mean biomass for CR and NDR: 
mean.biomass <- df %>% group_by(site, grid, visit) %>% 
  summarize(sum.biomass=sum(biomass/25)) %>% 
  ungroup() %>% 
  group_by(site, grid) %>% 
  summarize(mean.biomass=mean(sum.biomass))

mean.biomass.cr <- mean.biomass %>% filter(site == "CR")
mean.biomass.ndr <- mean.biomass %>% filter(site == "NDR")

# Estimate average supply: 
supply.load <- read.csv("data/processed/pooled.predicted.nutrients.v2.csv") %>% dplyr::select(-'X')
supply <- supply.load

supply.mean <- supply %>%  group_by(site, grid) %>% 
  summarize(mean.N=mean(sum.N.m2, na.rm=TRUE),
            # sd.N=sd(sum.N.m2, na.rm=TRUE),
            mean.P=mean(sum.P.m2, na.rm=TRUE))
            # sd.P=sd(sum.P.m2, na.rm=TRUE)) %>% 
  # left_join(predictors) %>% 
  # drop_na()


mean.supply.cr <- supply.mean %>% filter(site == "CR")
mean.supply.ndr <- supply.mean %>% filter(site == "NDR")
# write.csv(supply.mean, "N.P.mean.estimates.v2.csv")


cr.relief <- predictors %>% 
  filter(site == "CR") %>% 
  dplyr::select(site, grid, height_diff)
ndr.relief <- predictors %>% 
  filter(site == "NDR") %>% 
  dplyr::select(site, grid, height_diff)

mean.biomass.cr$grid <- as.factor(mean.biomass.cr$grid)
mean.supply.cr$grid <- as.factor(mean.supply.cr$grid)


cr.meta <- metadata %>% filter(site == "CR") %>% 
  filter(visit == '1')
cr.relief$grid <- as.factor(cr.relief$grid)
temp <- left_join(mean.biomass.cr, mean.supply.cr)
temp <- left_join(temp, cr.relief)

names(temp)
test <- left_join(temp, cr.meta) %>% select(c("site", "grid", "mean.biomass",
                                              "mean.N", "mean.P", "height_diff", "x", "y"))

# Export the dataset used in manuscript figure: carysfort averages to show within-reef variation 
# write.csv(test, "data/processed/carysfort_means.csv")

##########################################################################################
# 03: Export Figure 3.2 - Carysfort Reef subset showing relief, biomass, N supply, and P supply 
##########################################################################################
# Load data: dataframe that includes relief, mean biomass, and mean supply per plot
# carysfort <- read.csv("data/processed/carysfort_means.csv")


# Filter out grids in ortho image for the paper: 
carysfort$grid <- as.numeric(carysfort$grid)
carysfort <- carysfort %>% filter(grid > 20)      


# Relief
cr.rel <- ggplot(carysfort, 
                 aes(x=x, 
                     y=y)) + 
  geom_raster(aes(fill = height_diff)) +
  guides(colour = none) + #remove  legend
  scale_fill_gradientn(colors=grey(seq(0,1,l=20)),
                       name ="Relief (m)") +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),)+
  xlab("50m") +   
  ylab("50m") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
cr.rel
# ggsave("CR.site.vis.relief.png")



# CR: N
CR.N <- ggplot(carysfort, 
               aes(x=x, 
                   y=y)) + 
  geom_raster(aes(fill = mean.N)) +
  guides(colour = FALSE) + #remove  legend
  scale_fill_distiller(palette="GnBu",
                       name =expression(atop("N Supply", paste((mg~m^-2~day^-1))))) +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),)+
  xlab("50m") +   
  ylab("50m") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
CR.N
# ggsave("CR.mean.N.visual.png")

# CR: P
CR.P <- ggplot(carysfort, 
               aes(x=x, 
                   y=y)) + 
  geom_raster(aes(fill = mean.P)) +
  guides(colour = FALSE) + #remove  legend
  scale_fill_distiller(palette="BuPu",
                       name =expression(atop("P Supply", paste((mg~m^-2~day^-1))))) +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),)+
  xlab("50m") +   
  ylab("50m") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
CR.P
# ggsave("CR.mean.P.visual.png")

# Fish biomass: 
CR.bio <- ggplot(carysfort, 
                 aes(x=x, 
                     y=y)) + 
  geom_raster(aes(fill = mean.biomass)) +
  guides(colour = FALSE) + #remove  legend
  scale_fill_distiller(palette="Oranges", 
                       name =expression(atop("Biomass", paste((g~m^-2))))) +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
CR.bio



cowplot::plot_grid(cr.rel + theme(legend.justification = c(0,1)), # align all of the legends 
                   CR.N + theme(legend.justification = c(0,1)), 
                   CR.bio + theme(legend.justification = c(0,1)), 
                   CR.P + theme(legend.justification = c(0,1)), 
                   align='v')
ggsave("output/carysfort.png")
######################################
# # Relief
# ndr.rel <- ggplot(NDR.site.vis.df, 
#                   aes(x=x, 
#                       y=y)) + 
#   geom_raster(aes(fill = height_diff)) +
#   guides(colour = FALSE) + #remove  legend
#   scale_fill_gradientn(colors=grey(seq(0,1,l=20)),
#                        name ="Relief (m)") +
#   theme(axis.ticks = element_blank(), 
#         axis.text.x = element_blank(), 
#         axis.text.y = element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),)+
#   xlab("50m") +   
#   ylab("50m") +
#   
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank())
# ndr.rel
# # ggsave("CR.site.vis.relief.png")
# 
# 
# 
# # CR: N
# CR.N <- ggplot(carysfort, 
#                aes(x=x, 
#                    y=y)) + 
#   geom_raster(aes(fill = mean.N)) +
#   guides(colour = FALSE) + #remove  legend
#   scale_fill_distiller(palette="GnBu",
#                        name =expression(atop("N Supply", paste((g~m^-2~day^-1))))) +
#   theme(axis.ticks = element_blank(), 
#         axis.text.x = element_blank(), axis.text.y = element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),)+
#   xlab("50m") +   
#   ylab("50m") +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank())
# CR.N
# # ggsave("CR.mean.N.visual.png")
# 
# # CR: P
# CR.P <- ggplot(carysfort, 
#                aes(x=x, 
#                    y=y)) + 
#   geom_raster(aes(fill = mean.P)) +
#   guides(colour = FALSE) + #remove  legend
#   scale_fill_distiller(palette="BuPu",
#                        name =expression(atop("P Supply", paste((g~m^-2~day^-1))))) +
#   theme(axis.ticks = element_blank(), 
#         axis.text.x = element_blank(), axis.text.y = element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),)+
#   xlab("50m") +   
#   ylab("50m") +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank())
# CR.P
# # ggsave("CR.mean.P.visual.png")
# 
# # Fish biomass: 
# CR.bio <- ggplot(carysfort, 
#                  aes(x=x, 
#                      y=y)) + 
#   geom_raster(aes(fill = mean.biomass)) +
#   guides(colour = FALSE) + #remove  legend
#   scale_fill_distiller(palette="Oranges", 
#                        name =expression(atop("Biomass", paste((g~m^-2))))) +
#   theme(axis.ticks = element_blank(), 
#         axis.text.x = element_blank(), 
#         axis.text.y = element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),)+
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank())
# CR.bio
# 
# 
# 
# cowplot::plot_grid(cr.rel + theme(legend.justification = c(0,1)), # align all of the legends 
#                    CR.N + theme(legend.justification = c(0,1)), 
#                    CR.bio + theme(legend.justification = c(0,1)), 
#                    CR.P + theme(legend.justification = c(0,1)), 
#                    align='v')
# ggsave("cr.test.png")
# ######################################
# 

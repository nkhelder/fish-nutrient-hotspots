############################################################################
# Chapter 2: Quantifying fish-derived nutrient hotspots within the reefscape 
###########################################################################

# This script is used to create the final figure exploring variation 
# within a hotspot. 
library(tidyverse)
library(PNWColors)



# Relief SD was calculated in the original R script from Burns et al. Here I just filtered
# the spreadsheet to just Relief and its SD, and just for NDR and CR. 
relief <- read.csv("data/input/hotspot.relief.df.csv") %>% 
  dplyr::filter(fac == "1") %>% # keep only 1 cm res estimates
  dplyr::select(-'fac') %>% 
  dplyr::select(-'height_diff') # drop extra col

# Macroalgal nutrient supply per grid.  
N.fp <- read.csv("data/processed/N.fp.test.csv")

# Nutrient supply from fishes. 
supply.load <- read.csv("data/processed/pooled.predicted.nutrients.v2.csv") %>% 
  dplyr::select(-'X')

# refactor for joins
N.fp$site <- as.factor(N.fp$site)
relief$site <- as.factor(relief$site)
N.fp$grid <- as.factor(N.fp$grid)
relief$grid <- as.factor(relief$grid)

# add SD to dataframe
# test <- N.fp %>% left_join(relief)
N.fp <- N.fp %>% left_join(relief)
# filt <- test %>% filter(fp == "1")
# plot(N~sd_relief, data=filt)




# Hotspot dataframes used in this visual: 
# North Dry Rocks (NDR) 
ndr.hotspot <- N.fp %>% filter(site == "NDR") %>% 
  filter(fp == "1") # hypothesized hotspot sampling location 

# Carysfort Reef (CR)
cr.hotspot <- N.fp %>% filter(site == "CR") %>% 
  filter(fp == "1") # hypothesized hotspot sampling location

ndr.hotspot$var <- (ndr.hotspot$sd_relief)^2
cr.hotspot$var <- (cr.hotspot$sd_relief)^2


hotspot.pal <- pnw_palette(6, name = "Bay", type = "continuous")
# get color codes
# view(hotspot.pal)

# NDR Hotspot (FP 1)

# 1) Transect 1
#   a) Vertical relief by distance from hotspot 
ndr.1.relief <- ggplot(subset(ndr.hotspot, trans.cat == "1"),
       aes(x=dist.cat, y=height_diff)) +
  geom_point(color='#00496F') +
  geom_errorbar(aes(ymin=height_diff-var/2, ymax=height_diff+var/2), width=.2,
                position=position_dodge(.9))  +
  facet_wrap(~trans.cat) +
  ylim(1, 3.5) +
  ylab("Relief (m)") +
  xlab("") +
  theme_classic() +
  geom_line()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
ndr.1.relief

#   b) %N (macroalgal samples) by distance from hotspot 
ndr.1.percN <- ggplot(subset(ndr.hotspot, trans.cat == "1"),
                       aes(x=factor(dist.cat), 
                           y=N)) +
  geom_boxplot(fill='#00496F') +
  theme_classic() +
  labs(x="",
       y="Macroalgal %N") +
  scale_y_continuous(breaks = seq(0.5, 2, by=0.25), limits=c(0.5,2))
ndr.1.percN

# 2) Transect 2:
#   a) Vertical relief by distance from hotspot 
ndr.2.relief <- ggplot(subset(ndr.hotspot, trans.cat == "2"),
                       aes(x=dist.cat, y=height_diff)) +
  geom_point(color='#0C7996') +
  geom_errorbar(aes(ymin=height_diff-var/2, ymax=height_diff+var/2), width=.2,
                position=position_dodge(.9))  +
  facet_wrap(~trans.cat) +
  geom_line() +
  ylim(1, 3.5) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )  +
  ylab("Relief (m)") +
  xlab("") 
ndr.2.relief

#   b) %N (macroalgal samples) by distance from hotspot
ndr.2.percN <- ggplot(subset(ndr.hotspot, trans.cat == "2"),
                      aes(x=factor(dist.cat), 
                          y=N)) +
  geom_boxplot(fill='#0C7996') +
  #geom_errorbar(aes(ymin=height_diff-var, ymax=height_diff+var), width=.2,
  #             position=position_dodge(.9))  +
  theme_classic() +
  labs(x="",
       y="Macroalgal %N") +
  scale_y_continuous(breaks = seq(0.5, 2, by=0.25), limits=c(0.5,2))
ndr.2.percN


# 3) Transect 3:
#   a) Vertical relief by distance from hotspot 
ndr.3.relief <- ggplot(subset(ndr.hotspot, trans.cat == "3"),
                       aes(x=dist.cat, y=height_diff)) +
  geom_point(color='#94B669') +
  geom_errorbar(aes(ymin=height_diff-var/2, ymax=height_diff+var/2), width=.2,
                position=position_dodge(.9))  +
  facet_wrap(~trans.cat) +
  theme_classic() +
  geom_line() +
  ylim(1, 3.5)+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) +
  ylab("Relief (m)") +
  xlab("")
ndr.3.relief

#   b) %N (macroalgal samples) by distance from hotspot 
ndr.3.percN <- ggplot(subset(ndr.hotspot, trans.cat == "3"),
                      aes(x=factor(dist.cat), 
                          y=N)) +
  geom_boxplot(fill='#94B669') +
  #geom_errorbar(aes(ymin=height_diff-var, ymax=height_diff+var), width=.2,
  #             position=position_dodge(.9))  +
  theme_classic() +
  labs(x="",
       y="Macroalgal %N") +
  scale_y_continuous(breaks = seq(0.5, 2, by=0.25), limits=c(0.5,2))
ndr.3.percN



#########################################
# pdf("q3.pdf")
# cowplot::plot_grid(ndr.1.relief, ndr.1.percN)
# cowplot::plot_grid(ndr.2.relief, ndr.2.percN)
# cowplot::plot_grid(ndr.3.relief, ndr.3.percN)

# cowplot::plot_grid(cr.1.relief, cr.1.percN)
# cowplot::plot_grid(cr.2.relief, cr.2.percN)
# cowplot::plot_grid(cr.3.relief, cr.3.percN)
# dev.off()


#########################
# Use the q1.df.all to plot estimated nitrogen supply, but first select only the plots of interest


# 2023 update: not sure what q1.df.all refers to. Maybe this? Let's check: success!
# This dataset is the sum of all individual fish contributions to N and P excretion for each plot
# within each reefscape. We don't use the ndr.hotspot dataframe because it has average values
# for supply, but we want to use Individual values. 

supply <- supply.load

q1.df.all <- supply

NDR.1 <- q1.df.all %>% filter(site == "NDR")

F1.1.df <- NDR.1 %>% filter(grid == "63" | grid == "64" | grid == "65" | grid == "71") %>% 
  dplyr::select(site, grid, visit, sum.N.m2)
F1.2.df <- NDR.1 %>% filter(grid == "63" |grid == "68" | grid == "67" | grid == "73")%>% 
  dplyr::select(site, grid, visit, sum.N.m2)
F1.3.df <- NDR.1 %>% filter(grid == "63" |grid == "57" | grid == "51" | grid == "45")%>% 
  dplyr::select(site, grid, visit, sum.N.m2)


NDR.1b <- F1.1.df %>% rbind(F1.2.df) %>% rbind(F1.3.df)

F1.3.df$grid <- factor(F1.3.df$grid, 
                       levels=c("63", "57", "51", "45")) # specify factor level order

supply.labs <- c("0", "5", "10", "15")
# NDR 1 supply: 
ndr.1.supply <- ggplot(F1.1.df,
                     aes(x=factor(grid), 
                         y=sum.N.m2)) +
  geom_boxplot(fill='#00496F') +
  theme_classic() +
  labs(x="",
       y=expression(atop("N Supply", paste("(mg ",m^{-2}~day^{-1},")")))) + 
  scale_x_discrete(labels=supply.labs)
  # scale_y_continuous(breaks = seq(0.5, 2, by=0.25), limits=c(0.5,2))
ndr.1.supply


# NDR 2 supply: 
ndr.2.supply <- ggplot(F1.2.df,
                       aes(x=factor(grid), 
                           y=sum.N.m2)) +
  geom_boxplot(fill='#0C7996') +
  theme_classic() +
  labs(x="",
       y=expression(atop("N Supply", paste("(mg ",m^{-2}~day^{-1},")")))) +
  scale_x_discrete(labels=supply.labs)
# scale_y_continuous(breaks = seq(0.5, 2, by=0.25), limits=c(0.5,2))
ndr.2.supply


# NDR 3 supply: 
ndr.3.supply <- ggplot(F1.3.df,
                       aes(x=factor(grid), 
                           y=sum.N.m2)) +
  geom_boxplot(fill='#94B669') +
  theme_classic() +
  labs(x="",
       y=expression(atop("N Supply", paste("(mg ",m^{-2}~day^{-1},")")))) +
      #  y=expression(N~Supply~(mg~m^-2~day^-1))) +
  scale_x_discrete(labels=supply.labs)
# scale_y_continuous(breaks = seq(0.5, 2, by=0.25), limits=c(0.5,2))
ndr.3.supply

cowplot::plot_grid(ndr.1.relief, ndr.1.supply, ndr.1.percN,
                   ndr.2.relief, ndr.2.supply, ndr.2.percN,
                   ndr.3.relief, ndr.3.supply, ndr.3.percN,
                   align = "v",
                   ncol=3)
ggsave("q3.ndr.png")


# Carysfort: 
CR.1 <- q1.df.all %>% filter(site == "CR")
# CR focal points: FP 1=== F3 (grid 60) and FP 2 == F4 (grid 42)
# For each CR transect (A, C, and E)
F3.B.df <- CR.1 %>% filter(grid == "60" | grid == "50" | grid == "49" | grid == "48")
F3.C.df <- CR.1 %>% filter(grid == "60" |grid == "70" | grid == "80" | grid == "90")
F3.E.df <- CR.1 %>% filter(grid == "60" |grid == "69" | grid == "68" | grid == "67")

# For plotting, I need to specify the order of the grid cells to be increasing distances. Here, 
# I only had to do it for E, because the rest were in numerical order already, which is the
# standard plotting order.
F3.B.df$grid <- factor(F3.B.df$grid, levels=c("60", "49", "48", "38")) # specify factor level order
F3.C.df$grid <- factor(F3.C.df$grid, levels=c("60", "70", "80", "90"))
F3.E.df$grid <- factor(F3.E.df$grid, levels=c("60", "69", "68", "67"))


# CR 1 supply: 
cr.1.supply <- ggplot(F3.B.df,
                       aes(x=factor(grid), 
                           y=sum.N.m2)) +
  geom_boxplot(fill='#EDB829') +
  theme_classic() +
  labs(x="",
       y=expression(atop("N Supply", paste("(mg ",m^{-2}~day^{-1},")")))) + 
  scale_x_discrete(labels=supply.labs) 
# scale_y_continuous(breaks = seq(0.5, 2, by=0.25), limits=c(0.5,2))
cr.1.supply

cr.1.supply.scaled <- ggplot(F3.B.df,
                      aes(x=factor(grid), 
                          y=sum.N.m2)) +
  geom_boxplot(fill='#EDB829') +
  theme_classic() +
  labs(x="",
       y=expression(atop("N Supply", paste("(mg ",m^{-2}~day^{-1},")")))) + 
  scale_x_discrete(labels=supply.labs) +
  ylim(0,600)
# scale_y_continuous(breaks = seq(0.5, 2, by=0.25), limits=c(0.5,2))
cr.1.supply.scaled

# CR 2: 
cr.2.supply <- ggplot(F3.C.df,
                      aes(x=factor(grid), 
                          y=sum.N.m2)) +
  geom_boxplot(fill='#E97C07') +
  theme_classic() +
  labs(x="",
       y=expression(atop("N Supply", paste("(mg ",m^{-2}~day^{-1},")")))) + 
  scale_x_discrete(labels=supply.labs) 
# scale_y_continuous(breaks = seq(0.5, 2, by=0.25), limits=c(0.5,2))
cr.2.supply

cr.2.supply.scaled <- ggplot(F3.C.df,
                      aes(x=factor(grid), 
                          y=sum.N.m2)) +
  geom_boxplot(fill='#E97C07') +
  theme_classic() +
  labs(x="",
       y=expression(atop("N Supply", paste("(mg ",m^{-2}~day^{-1},")")))) + 
  scale_x_discrete(labels=supply.labs) +
  ylim(0,600)
# scale_y_continuous(breaks = seq(0.5, 2, by=0.25), limits=c(0.5,2))
cr.2.supply.scaled

F3.E.df <- read.csv("F3.E.df.csv")
cr.3.supply <- ggplot(F3.E.df,
                      aes(x=factor(grid), 
                          y=sum.N.m2)) +
  geom_boxplot(fill='#DD4124') +
  theme_classic() +
  labs(x="",
       y=expression(atop("N Supply", paste("(mg ",m^{-2}~day^{-1},")")))) + 
  scale_x_discrete(labels=supply.labs) 
# scale_y_continuous(breaks = seq(0.5, 2, by=0.25), limits=c(0.5,2))
cr.3.supply

cr.3.supply.scaled <- ggplot(F3.E.df,
                      aes(x=factor(grid), 
                          y=sum.N.m2)) +
  geom_boxplot(fill='#DD4124') +
  theme_classic() +
  labs(x="",
       y=expression(atop("N Supply", paste("(mg ",m^{-2}~day^{-1},")")))) + 
  scale_x_discrete(labels=supply.labs) +
  ylim(0,600)
# scale_y_continuous(breaks = seq(0.5, 2, by=0.25), limits=c(0.5,2))
cr.3.supply.scaled

view(hotspot.pal)

cowplot::plot_grid(ndr.1.relief, ndr.1.supply, ndr.1.percN,
                   ndr.2.relief, ndr.2.supply, ndr.2.percN,
                   ndr.3.relief, ndr.3.supply, ndr.3.percN,
                   align = "v",
                   labels='AUTO',
                   label_y = 1.03,
                   ncol=3)
ggsave("q3.ndr.v2.png")


cowplot::plot_grid(cr.3.relief, cr.3.supply,  cr.3.percN,
                   cr.1.relief, cr.1.supply,  cr.1.percN,
                   cr.2.relief, cr.2.supply,  cr.2.percN,
                   align = "v", 
                   label_y = 1.03,
                   labels=c("J", "K", "L", "M", "N", "O", "P", "Q", "R"),
                   ncol=3)
ggsave("q3.cr.3.png")

# Order by transect similarity: 
cowplot::plot_grid(ndr.1.relief, ndr.1.supply, ndr.1.percN, ndr1.cerv, ndr1.palm,
                   #cr.3.relief, cr.3.supply,  cr.3.percN,
                   #ndr.3.relief, ndr.3.supply, ndr.3.percN,
                   align = "v",
                   labels='AUTO',
                   label_y = 1.03,
                   ncol=3)
# first NDR transect with both cerv and palm
ggsave("ndr.crf.visA.png")

# third NDR transect with just cerv
cowplot::plot_grid(
                   ndr.3.relief, ndr.3.supply, ndr.3.percN,ndr3.cerv,
                   align = "v",
                   labels= c("F", "G", "H", "I"),
                   label_y = 1.03,
                   ncol=3)
ggsave("ndr.crf.3.cerv.png")


cowplot::plot_grid(ndr1.cerv, ndr1.palm,
                   #cr.3.relief, cr.3.supply,  cr.3.percN,
                   #ndr.3.relief, ndr.3.supply, ndr.3.percN,
                   align = "v",
                   labels=c("D", "E"),
                   label_y = 1.03,
                   ncol=2)

ggsave("ndr.crf.visB.png")



#cowplot::plot_grid(cr.2.relief, cr.2.supply,  cr.2.percN,
 #                 cr.1.relief, cr.1.supply,  cr.1.percN,
  #                ndr.2.relief, ndr.2.supply, ndr.2.percN,
   #               align = "v",
    #              labels=c("J", "K", "L", "M", "N", "O", "P", "Q", "R"),
     #             
      #            label_y = 1.03,
       #            ncol=3)
# ggsave("q3.bottom.png")




cowplot::plot_grid( cr.3.relief, cr.3.supply,  cr.3.percN,
                   # cr.2.relief, cr.2.supply,  cr.2.percN,
                   cr.1.relief, cr.1.supply,  cr.1.percN,
                   # ndr.2.relief, ndr.2.supply, ndr.2.percN,
                  
                   align = "v",
                   labels=c("G", "H", "I", "J", "K", "L"),
                   
                   label_y = 1.03,
                   ncol=3)
ggsave("q3.cr.png")


# NDR visual for Amelia + Steph

cowplot::plot_grid(ndr.1.relief, ndr.1.supply, ndr.1.percN, 
                   align = "v",
                   labels='AUTO',
                   labels=c("A", "B", "C", "D", "E"),
                   label_y = 1.03,
                   ncol=3)

ggsave("q3.ndr.png")


# #########################################
# CR Hotspot
#########################################
# 1) T1: relief
cr.1.relief <- ggplot(subset(cr.hotspot, trans.cat == "1"),
                      aes(x=dist.cat, y=height_diff)) +
  geom_point(color='#EDB829') +
  geom_errorbar(aes(ymin=height_diff-var, ymax=height_diff+var), width=.2,
                position=position_dodge(.9))  +
  facet_wrap(~trans.cat) +
  theme_classic() +
  ylim(1, 3.5)+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()) +
  ylab("Relief (m)") +
  xlab("") +
  geom_line()
cr.1.relief
fix(cr.hotspot)

# 2) T1: %N
cr.1.percN <- ggplot(subset(cr.hotspot, trans.cat == "1"),
                     aes(x=factor(dist.cat), 
                         y=N)) +
  geom_boxplot(fill='#EDB829') +
  #geom_errorbar(aes(ymin=height_diff-var, ymax=height_diff+var), width=.2,
  #             position=position_dodge(.9))  +
  theme_classic() +
  labs(x="",
       y="Macroalgal %N") +
  scale_y_continuous(breaks = seq(0.5, 2, by=0.25), limits=c(0.5,2))
cr.1.percN


# 3) T2: relief
cr.2.relief <- ggplot(subset(cr.hotspot, trans.cat == "2"),
                      aes(x=dist.cat, y=height_diff)) +
  geom_point(color='#E97C07') +
  geom_errorbar(aes(ymin=height_diff-var, ymax=height_diff+var), width=.2,
                position=position_dodge(.9))  +
  facet_wrap(~trans.cat) +
  theme_classic() +
  ylim(1, 3.5)+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()) +
  ylab("Relief (m)") +
  xlab("") +
  geom_line()
cr.2.relief

# 4) T3: %N
cr.2.percN <- ggplot(subset(cr.hotspot, trans.cat == "2"),
                     aes(x=factor(dist.cat), 
                         y=N)) +
  geom_boxplot(fill='#E97C07') +
  #geom_errorbar(aes(ymin=height_diff-var, ymax=height_diff+var), width=.2,
  #             position=position_dodge(.9))  +
  theme_classic() +
  labs(x="",
       y="Macroalgal %N") +
  # ylim(0.5,2) 
  scale_y_continuous(breaks = seq(0.5, 2, by=0.25), limits=c(0.5,2))
cr.2.percN

# 5) T3: relief
cr.3.relief <- ggplot(subset(cr.hotspot, trans.cat == "3"),
                      aes(x=dist.cat, y=height_diff)) +
  geom_point(color='#DD4124') +
  geom_errorbar(aes(ymin=height_diff-var, ymax=height_diff+var), width=.2,
                position=position_dodge(.9))  +
  facet_wrap(~trans.cat) +
  theme_classic() +
  ylim(1, 3.5)+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()) +
  ylab("Relief (m)") +
  xlab("")  +
  geom_line()
cr.3.relief


# 4) T3: %N
cr.3.percN <- ggplot(subset(cr.hotspot, trans.cat == "3"),
                     aes(x=factor(dist.cat), 
                         y=N)) +
  geom_boxplot(fill='#DD4124') +
  #geom_errorbar(aes(ymin=height_diff-var, ymax=height_diff+var), width=.2,
  #             position=position_dodge(.9))  +
  theme_classic() +
  labs(x="",
       y="Macroalgal %N") +
  scale_y_continuous(breaks = seq(0.5, 2, by=0.25), limits=c(0.5,2))
cr.3.percN


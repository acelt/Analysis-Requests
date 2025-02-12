setwd('\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\WY')
library(ggplot2)
library(spsurvey)

# Analysis of Kemmerer Field Office, Land Use Plan (LUP) AIM stratified random sample design (2017-2020)
# for the LUP sample design 194 plots were sampled, no rejections ## I doubled checked in the SDD and also didnt find rejections - this seems pretty odd to me
# design used BPS data as strata

# First get indicator estimates from the LUP design without using the 2019 PHMA design
# Data inputs needed
# the plot data and its corresponding sample design strata information 
plots <- read.csv('KFO_SD-Terr_5yr-design_random_plots_only.csv', header=T) ## How are you designating strata? based on sampled location in relation to design strata or based on matching to a set of criteria based on field data?
plots <- plots[1:194,]


# the total area, in hectares, of each strata that comprises the sample frame, from BLM GIS data
strata.area <- read.csv('KFO_SD_frame_strata_area.csv', header=T)
# NA is comprised of BPS categories water, Barren-Rock/Sand/Clay, and Sparsely Vegetated; remove NA as they 
# were not mentioned in the MDW ## its kinda odd that theres NAs in your strata - although this is helpful as an estimate of Non-target acres
colnames(strata.area) <- c("Stratum", "Hectares")
strata.area <- strata.area[-c(5), ]


# t(t(table(plots$Stratum))) # number of plots per stratum ## you may want to compare these to the proportion of points/strata within the original design 
## Basin Big sage - 50
## Conifer - 15
## Cottonwood - 15
## Mountain bigsage - 25
## Other - 15
## Aspen - 15
## Shrubland - 15
## Wyoming bigsage - 50 Base points

## it looks like there is a fair amount of difference between sampled points and design strata - this would probably get picked up with the weights 
# but is worth noting at least that there may be proortionally higher representation of wyoming big sage compare to basin big


# calculate the plot weights within each strata
plots$weight[plots$Stratum == "BasinBigSagebrush"] <- 
  1 / (nrow(subset(plots, Stratum == "BasinBigSagebrush"))/ strata.area$Hectares[1])

plots$weight[plots$Stratum == "Conifer"] <- 
  1 / (nrow(subset(plots, Stratum == "Conifer"))/ strata.area$Hectares[2])

plots$weight[plots$Stratum == "Cottonwood"] <- 
  1 / (nrow(subset(plots, Stratum == "Cottonwood"))/ strata.area$Hectares[3])

plots$weight[plots$Stratum == "MountainSagebrush"] <- 
  1 / (nrow(subset(plots, Stratum == "MountainSagebrush"))/ strata.area$Hectares[4])

plots$weight[plots$Stratum == "Other"] <- 
  1 / (nrow(subset(plots, Stratum == "Other"))/ strata.area$Hectares[5])

plots$weight[plots$Stratum == "QuakingAspen"] <- 
  1 / (nrow(subset(plots, Stratum == "QuakingAspen"))/ strata.area$Hectares[6])

plots$weight[plots$Stratum == "Shrubland"] <- 
  1 / (nrow(subset(plots, Stratum == "Shrubland"))/ strata.area$Hectares[7])

plots$weight[plots$Stratum == "WyomingBigSagebrush"] <- 
  1 / (nrow(subset(plots, Stratum == "WyomingBigSagebrush"))/ strata.area$Hectares[8])

## checking out weights
summary(plots$weight)
## range is 23.24-3862.45

# set up arguments for spsurvey categorical analysis
myvars <- c("BareSoilCategory") # vars argument

# population level categorical estimate of percent area meeting vs. not meeting
baresoil.results <- cat_analysis(plots, vars = myvars, weight = "weight", vartype = "HT", conf = 95) 

## For what its wirth we've been calculating goodmans multinomial confidence intervals instead of the default ones used by spsurvey 
## although thats mostly relevant for analysis with >2 categories


# extract results needed from the categorial analysis to plot
baresoil.results <- baresoil.results[1:2,c(4,6,9,10)] 

ggplot(baresoil.results) +
  geom_bar( aes(x=Category, y=Estimate.P), stat="identity",  alpha=0.5) +
  geom_errorbar( aes(x=Category, ymin=LCB95Pct.P, ymax=UCB95Pct.P), width=0.4, colour="black", alpha=0.9, size=0.9) +
  ggtitle("Percent of KFO surface area meeting bare soil benchmarks, w/ 95% CI") + 
  ylab("Percent area") + xlab("")


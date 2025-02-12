# Test out different variables and variable weights for use in the WCCT
# we could use a set of refernce and non-reference sites or sites previously listed as meeting land health standards 
# as a train and test dataset

# Can then potentially use random forest, mlr, or, glmm to select variables

# Setup
library(corrplot)
library(tidyverse)
library(GGally)

# set paths
output_path <- "\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\C drive back up\\2023\\National Report\\Restoration priorities HQ\\"

# Import data
data <- read.csv(paste0(output_path, "/","benchmarked_points_9272024.csv"))

# test correlation of variables
# make correlation matrix
# pare data down
# may want to do this with both raw indicators values and benchmarked values

# start with benchmarked/departure scores
cor_data_bm <- data[,c("Native_score_dep",
                       "InvasiveAG_score_dep",
                       "Invasive_score_dep",
                       "SoilStabilityScore_dep",
                       "CanopyGapScore101_200_dep",
                       "CanopyGapScore200plus_dep",
                       "BareSoilScore_dep")]
# look at variance and covariance
var <- var(cor_data_bm, na.rm = TRUE)

# default is Pearson's correlation coefficient
cov <- cov(cor_data_bm, use = "complete.obs")
cor <- cor(cor_data_bm, use = "complete.obs")

# using spearmans rank
cov_sr <- cov(cor_data_bm, use = "complete.obs", method = "spearman")
cor_sr <- cor(cor_data_bm, use = "complete.obs", method = "spearman")

# lets plot these
corrplot(cor_sr, methods = "ellipse")
corrplot(cor, methods = "ellipse")# ellipse gives thin lines for high correlation and circles for smaller correlations
# perhaps unsurprisingly the invasive/native scores are highly correlated

ggplot(cor_data_bm, aes(x = Native_score_dep,
                        y = InvasiveAG_score_dep))+
  geom_point()
# should probably only keep one of them, everything else looks generally OK

ggpairs(cor_data_bm)

?corrplot::corrplot()


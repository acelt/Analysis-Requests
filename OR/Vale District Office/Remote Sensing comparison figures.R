data <- read.csv("\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\OR\\CLAA_analysis_comparison.csv")

library(tidyverse)
dodge1 <- position_dodge(width = 0.5)

claa_data <- data[data$ReportingUnit == "CLAA",]
sm_data <- data[data$ReportingUnit == "Spring Mountain",]
mm_data <- data[data$ReportingUnit == "Mahogany Mountain",]

ggplot(claa_data, aes(y = Estimated.Area, x = Rating, col = Source))+
  geom_point(position = dodge1)+
  facet_wrap(.~Indicator)+
  theme_bw(base_size = 14)+
  geom_errorbar(aes(ymin = Lower.confidence.bound.of.acres..80...Goodman.multinomial., ymax = Upper.confidence.bound.of.acres..80...Goodman.multinomial.), width = 0.2, position = dodge1)+
  ggtitle("CLAA Group 2")+
  labs(y = "Estimated Area (Hectares)")
ggsave()

ggplot(sm_data, aes(y = Estimated.Area, x = Rating, col = Source))+
  geom_point(position = dodge1)+
  facet_wrap(.~Indicator)+
  theme_bw(base_size = 14)+
  geom_errorbar(aes(ymin = Lower.confidence.bound.of.acres..80...Goodman.multinomial., ymax = Upper.confidence.bound.of.acres..80...Goodman.multinomial.), width = 0.2, position = dodge1)+
  ggtitle("Spring Mountain")+
  labs(y = "Estimated Area (Hectares)")

ggplot(mm_data, aes(y = Estimated.Area, x = Rating, col = Source))+
  geom_point(position = dodge1)+
  facet_wrap(.~Indicator)+
  theme_bw(base_size = 14)+
  geom_errorbar(aes(ymin = Lower.confidence.bound.of.acres..80...Goodman.multinomial., ymax = Upper.confidence.bound.of.acres..80...Goodman.multinomial.), width = 0.2, position = dodge1)+
  ggtitle("Mahogany Mountain")+
  labs(y = "Estimated Area (Hectares)")

# plotting raster distributions against AIM data

library(raster) # i should probably be using the terra package instead...
library(terra)
library(rgdal)
library(sf)
library(arcgisbinding)
arc.check_product() 

points <- sf::st_read(dsn = "\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\OR\\Remote sensing benchmarks.gdb","Extract_AIM_LMF1")

rcmap_baresoil <- raster("\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\OR\\Rasters\\Mean_bare_ground.tif")
hist(rcmap_baresoil, maxpixels=ncell(rcmap_baresoil))

rcmap_annualherb <- raster("\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\OR\\Rasters\\Mean_annual_herb.tif")
hist(rcmap_annualherb, maxpixels=ncell(rcmap_annualherb))
rcmap_totalfoliar <- raster("\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\OR\\Rasters\\Mean_total_foliar.tif")
hist(rcmap_totalfoliar, maxpixels=ncell(rcmap_totalfoliar))
rcmap_perann <- raster("\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\OR\\Rasters\\Mean_perennial_annual_ratio.tif")
hist(rcmap_perann, maxpixels=ncell(rcmap_perann))

rap_baresoil <- raster("\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\OR\\RAP\\extract_RAP_Means_RAP2017_2021_BG_BGR_tif.tif")
hist(rap_baresoil, maxpixels=ncell(rap_baresoil))

rap_annualherb <- araster("\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\OR\\RAP\\extract_RAP_Means_RAP2017_2021_AFG_AFG_tif.tif")
rap_totalfoliar <- raster("\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\OR\\RAP\\extract_RAP_Means_total_foliar_RAP_tif.tif")
rap_perann <- raster("\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\OR\\RAP\\extract_RAP_Means_perennial_annual_ratio_RAP_tif.tif")

# Annual Herb
ggplot()+
  theme_bw(base_size = 14)+
  geom_bar(data = points, aes(x = AH_AnnGrassCover, y = (..count..)/sum(..count..)), fill = NA, col = "black")+
  geom_bar(data = as.data.frame(values(rcmap_annualherb)), aes(x = `values(rcmap_annualherb)`, y = (..count..)/sum(..count..)), fill = NA, col = "orange")+
  geom_bar(data = as.data.frame(values(rap_annualherb)), aes(x = `values(rap_annualherb)`, y = (..count..)/sum(..count..)), fill = NA, col = "purple")+
  ggtitle("RCMAP and RAP Histograms - Annual Herbaceous")+
  labs(y = "Frequency", x = "Annual Grass Cover")+
  coord_cartesian(xlim = c(0,100))

# bare soil
ggplot()+
  theme_bw(base_size = 14)+
  geom_bar(data = points, aes(x = BareSoilCover, y = (..count..)/sum(..count..)), bins = 50, fill = NA, col = "black")+
  geom_bar(data = as.data.frame(values(rcmap_baresoil)), aes(x = `values(rcmap_baresoil)`, y = (..count..)/sum(..count..)), bins = 50, fill = NA, col = "orange")+
  geom_bar(data = as.data.frame(values(rap_baresoil)), aes(x = `values(rap_baresoil)`, y = (..count..)/sum(..count..)), bins = 50, fill = NA, col = "purple")+
  ggtitle("RCMAP and RAP Histograms - Bare Ground")+
  labs(y = "Frequency", x = "Bare Ground Cover")+
  coord_cartesian(xlim = c(0,100))

# Total Foliar
ggplot()+
  theme_bw(base_size = 14)+
  geom_bar(data = points, aes(x = TotalFoliarCover, y = (..count..)/sum(..count..)), bins = 50, fill = NA, col = "black")+
  geom_bar(data = as.data.frame(values(rcmap_totalfoliar)), aes(x = `values(rcmap_totalfoliar)`, y = (..count..)/sum(..count..)), fill = NA, col = "orange")+
  geom_bar(data = as.data.frame(values(rap_totalfoliar)), aes(x = `values(rap_totalfoliar)`, y = (..count..)/sum(..count..)), fill = NA, col = "purple")+
  ggtitle("RCMAP and RAP Histograms - Total Foliar")+
  labs(y = "Frequency", x = "Total Foliar Cover")+
  coord_cartesian(xlim = c(0,100))



# perennial:annual ratio\
rcmap_pa_df <-  as.data.frame(values(rcmap_perann))
rcmap_pa_df <- rcmap_pa_df[!is.na(rcmap_pa_df$`values(rcmap_perann)`),]
rcmap_pa_df <- as.data.frame(rcmap_pa_df)

rap_pa_df <-  as.data.frame(values(rap_perann))
rap_pa_df <- rap_pa_df[!is.na(rap_pa_df$`values(rap_perann)`),]
rap_pa_df <- as.data.frame(rap_pa_df)

ggplot()+
  theme_bw(base_size = 14)+
  geom_bar(data = points[!is.na(points$PerennialAnnualGrassRatio),], aes(x = PerennialAnnualGrassRatio, y = (..count..)/sum(..count..)), bins = 50, fill = NA, col = "black")+
  geom_bar(data = rcmap_pa_df, aes(x = rcmap_pa_df, y = (..count..)/sum(..count..)), bins = 50, fill = NA, col = "orange")+
  geom_bar(data = rap_pa_df, aes(x = rap_pa_df, y = (..count..)/sum(..count..)), bins = 50, fill = NA, col = "purple")+
  ggtitle("RCMAP and RAP Histograms - Perennial:Annual Ratio")+
  labs(y = "Frequency", x = "Perennial:Annual Ratio") +
  coord_cartesian(xlim = c(0,25))


# plotting/ table for rmse
rmse <- function(data, column_predicted, column_actual){
  # predicted values - these should just be the AIM data
  rmse <-  sqrt(mean((data[[column_actual]] - data[[column_predicted]])^2, na.rm = TRUE))
  return(rmse)
}

aim_vars <- c("BareSoilCover", "AH_AnnGrassCover","PerennialAnnualGrassRatio","TotalFoliarCover")
rcmap_vars <- c("RCMAP_Mean_bare_ground","RCMAP_Mean_annual_herb","RCMAP_Mean_perennial_annual_ratio","RCMAP_Mean_total_foliar")
rap_vars <- c("RAP2017_2021_BG_BGR","RAP2017_2021_AFG_AFG","perennial_annual_ratio_RAP","total_foliar_RAP")

for(i in seq(1:4)){
  rmse_rcmap <- rmse(points, rcmap_vars[i], aim_vars[i])
  print(paste0("RMSE values for ",rcmap_vars[i]))
  print(rmse_rcmap)
  
  rmse_rap <- rmse(points, rap_vars[i], aim_vars[i])
  print(paste0("RMSE values for ",rap_vars[i]))
  print(rmse_rap)
}

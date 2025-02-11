# load packages
library(tidyverse)
library(RODBC)
library(MVA)
library(mclust)
library(lattice)
library(sf)

# grab terradat data from sde
conn <- RODBC::odbcConnect("AIMPub")

tblPlots <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMPub.ilmocAIMPubDBO.tblPlots;')
soil_pit <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMPub.ilmocAIMPubDBO.tblSoilPits;')
soil_horizon <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMPub.ilmocAIMPubDBO.tblSoilPitHorizons;')
TerrADatSpecies <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMPub.ilmocAIMPubDBO.TerrADatSpeciesIndicators;')
SpeciesList <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMPub.ilmocAIMPubDBO.tblStateSpecies;')

# We'll just grab the already subset AIM and LMF points within the relevant MLRA from the gdb
points_path <-  'C:\\Users\\alaurencetraynor\\Documents\\2021\\Analysis\\ORWA\\Benchmark Groups\\MyProject3\\MyProject3.gdb'
points <- st_read(points_path, "AIM_LMF_PairwiseIntersect")
points <-  st_drop_geometry(points) # just interested in the table values

# join to tblplots, soil pit and first horizon of soil pit
# will need to first filter soilpit horizons table to only the layers that start with 0 (aka top layer)
soil_horizon <- soil_horizon[soil_horizon$HorizonDepthUpper == 0,]

temp1 <- merge(points,
               soil_horizon,
               by = "PrimaryKey")
temp2 <- merge(temp1,
               soil_pit,
               by = "PrimaryKey")
all_data <- merge(temp2,
                  tblPlots,
                  by = "PrimaryKey")
rm(temp1)
rm(temp2)

# select relevant columns
# for this analysis we want to focus on the static variables including soil pit and plot characterization info
data_selection <- all_data[,c("PrimaryKey","FH_RockCover","Texture","RockFragments.x","ESD_PctClay","SoilDepthLower","DepthMeasure.y","AvgPrecip","AvgPrecipUOM", "Slope","Aspect","LandscapeType", "Elevation.y", "ElevationType.y","BareSoilCover", "TotalFoliarCover","GapCover_200_plus","AH_NonNoxPerenForbCover", "AH_NonNoxAnnForbCover","AH_NonNoxPerenGrassCover","AH_NonNoxShrubCover","AH_NonNoxSubShrubCover","AH_NonNoxSucculentCover","AH_NonNoxTreeCover",'AH_PreferredForbCover', "AH_TallPerenGrassCover","AH_ShortPerenGrassCover","AH_TotalLitterCover","NumSpp_NonNoxPlant")]

# need to make sure depth is all in same units
data_selection$SoilDepthLower[data_selection$DepthMeasure.y == "in"] <- data_selection$SoilDepthLower[data_selection$DepthMeasure.y == "in"]* 2.54

# same with precip
data_selection[data_selection$AvgPrecipUOM == "in" & !is.na(data_selection$AvgPrecipUOM),]$AvgPrecip <- data_selection[data_selection$AvgPrecipUOM == "in" & !is.na(data_selection$AvgPrecipUOM),]$AvgPrecip*25.4

# inspect data and look for holes
summary(data_selection)

# slope has a negative value - lets remove the minus sign
data_selection$Slope[data_selection$Slope == -45] <- 45
data_selection$Slope[data_selection$Slope == -7] <- 7

# theres some high slope values too
# replace with NA
data_selection$Slope[data_selection$Slope > 90] <- NA

# may want to alter this to Northness or Eastness?
data_selection$Aspect

# elevation
# I think elevation type specifies ft versus meters - lets convert those with type 2 back to meters
data_selection$Elevation.y[data_selection$ElevationType.y == 2 & !is.na(data_selection$ElevationType.y)] <- data_selection$Elevation.y[data_selection$ElevationType.y == 2 & !is.na(data_selection$ElevationType.y)]/3.281

# theres still 3 values over 5000 which cannot be meters...
data_selection$Elevation.y[data_selection$Elevation.y > 5000 & !is.na(data_selection$Elevation.y)] <- data_selection$Elevation.y[data_selection$Elevation.y > 5000& !is.na(data_selection$Elevation.y)]/3.281

# theres also 3 values with zeros...drop them
data_selection$Elevation.y[data_selection$Elevation.y < 1 & !is.na(data_selection$Elevation.y)] <-  NA

hist(data_selection$Elevation.y)

# Explroation
hist(data_selection$SoilDepthLower)

# rock cover
hist(data_selection$FH_RockCover)

hist(data_selection$RockFragments.x)
hist(data_selection$ESD_PctClay)

# make characters factors
data_selection$Texture <- as.factor(data_selection$Texture)
summary(data_selection$Texture)

data_selection$LandscapeType <- as.factor(data_selection$LandscapeType)

summary(data_selection$LandscapeType)

# remove missing values/NAs # drop those metadata cols
complete_data <- data_selection[complete.cases(data_selection),c("PrimaryKey","FH_RockCover","Texture","RockFragments.x","ESD_PctClay","SoilDepthLower","AvgPrecip", "Slope","Aspect","LandscapeType", "Elevation.y","BareSoilCover", "TotalFoliarCover","GapCover_200_plus","AH_NonNoxPerenForbCover", "AH_NonNoxAnnForbCover","AH_NonNoxPerenGrassCover","AH_NonNoxShrubCover","AH_NonNoxSubShrubCover","AH_NonNoxSucculentCover","AH_NonNoxTreeCover",'AH_PreferredForbCover', "AH_TallPerenGrassCover","AH_ShortPerenGrassCover","AH_TotalLitterCover","NumSpp_NonNoxPlant")]

by(complete_data, complete_data$LandscapeType, summary)

complete_data$ESD_PctClay

ggplot(complete_data, aes(x = Slope, y = Elevation.y))+
  geom_point()

# ordination/cluster analysis/PCA
# remove categorical variables 
pca_data <- complete_data[,c("PrimaryKey","FH_RockCover","RockFragments.x","ESD_PctClay","SoilDepthLower","AvgPrecip", "Slope", "Elevation.y","BareSoilCover", "TotalFoliarCover","GapCover_200_plus","AH_NonNoxPerenForbCover", "AH_NonNoxAnnForbCover","AH_NonNoxPerenGrassCover","AH_NonNoxShrubCover","AH_NonNoxSubShrubCover","AH_NonNoxSucculentCover","AH_NonNoxTreeCover",'AH_PreferredForbCover', "AH_TallPerenGrassCover","AH_ShortPerenGrassCover","AH_TotalLitterCover","NumSpp_NonNoxPlant")]

length(unique(pca_data$PrimaryKey))
# there's non unique PKs here...
pca_data <- distinct(pca_data)

pca_data[pca_data$PrimaryKey == "19071514172896832019-09-01",]
pca_data[pca_data$PrimaryKey == "19072807184736622019-09-01",]

# remove these weird duplicates
pca_data <- pca_data[-c(803:805,837),]

row.names(pca_data) <- pca_data$PrimaryKey

# now drop PK col
pca_data <- pca_data[,-1]

# OK now run pca
# make sure everything is numeric
#pca_data$Aspect <- as.numeric(pca_data$Aspect)
# this introduced some NAs, we need to drop them
#pca_data <- pca_data[!is.na(pca_data$Aspect),]

pca <- prcomp(pca_data, center = TRUE, scale = TRUE)

# plot\
plot(pca$x[,1],pca$x[,2])

# calc variance
pca.var <- pca$sdev^2

# calc percentage of variation explained by each pc
pca.var.per <- pca.var/sum(pca.var)*100
barplot(pca.var.per)

ggdata <- data.frame(PK = row.names(pca$x),X = pca$x[,1], Y = pca$x[,2])

loading_scores_pc1 <- abs(pca$rotation[,1])
loading_scores_pc1

# Keeping it simple and focus on elevation?
clust <- kmeans(pca_data, centers = 2)

clust_num <- as.data.frame(clust$cluster)
clust_num$PrimaryKey <- row.names(clust_num)

ggdata <- merge(ggdata,
                clust_num,
                by.x = "PK",
                by.y = "PrimaryKey")

ggplot(ggdata, aes(x = X, y = Y, col = as.factor(clust_num$`clust$cluster`)))+
  geom_point(alpha = 0.5)+
  theme_bw()+
  labs(x = paste0("PC1 - ", round(pca.var.per[1],1),"%"),
       y = paste0("PC2 - ", round(pca.var.per[2],1),"%"))

# merge kmeans data with 
complete_data_grouped <- merge(complete_data,
                clust_num,
                by.x = "PrimaryKey",
                by.y = "PrimaryKey")

by(complete_data_grouped, complete_data_grouped$`clust$cluster`, summary)

# What if there arent big differences?

# Let add precip to this based on prism

# Grab the spatial points data

# make bounding box

# pull prism precip data from api baed on bb
# I think we might want recent weather + 30 year normals
# we'll use rgee for this

#install.packages("rgee")
#install.packages("remotes")
library(remotes)
#install_github("r-spatial/rgee")
rgee::ee_install()

# set python path
rgee::ee_install_set_pyenv(
  py_path = "C:\\Program Files\\ArcGIS\\Pro\\bin\\Python\\envs\\arcgispro-py3",
  py_env = NULL
)

rgee::ee_check()

# SETUP
library(tidyverse)
library(arcgis)
library(arcgisbinding)
arc.check_product()
library(sf)
library(tmap)
library(terra)

sf_use_s2(FALSE)

# SET PATHS
sndo_gdb <- "\\\\blm.doi.net\\dfs\\loc\\egis\\GISUsers\\alaurencetraynor.BLM\\My Documents\\ArcGIS\\Projects\\SNDO REPORT\\SNDO REPORT.gdb"
output_path <- "C:\\Users\\alaurencetraynor\\Documents\\NV\\SNDO Report\\outputs"

# IMPORT DATA
# Points
terradat <- st_read(dsn = sndo_gdb,
                    layer = "ilmocAIMdev_Terradat")

lmf <- st_read(dsn = sndo_gdb,
               layer = "SNDO_LMF")

# Design
tdat_designpoints <-  st_read(dsn = sndo_gdb,
                              layer = "ilmocAIMdev_SNDO_DesignPoints")

tdat_designpolys <- st_read(dsn = sndo_gdb,
                            layer = "ilmocAIMdev_SNDO_DesignPolys")

tdat_notsampled <- st_read(dsn = sndo_gdb,
                           layer = "ilmocAIMdev_SNDO_NotSampled")

lmf_designpoints <-  st_read(dsn = sndo_gdb,
                              layer = "SNDO_LMF_LMFDesignPoints")

lmf_tpolys <- st_read(dsn = sndo_gdb,
                            layer = "SNDO_LMF_LMFThiessenPolygons")

lmf_segments <- st_read(dsn = sndo_gdb,
                      layer = "SNDO_LMF_LMFSegmentPolygons")

lmf_notsampled <- st_read(dsn = sndo_gdb,
                           layer = "SNDO_LMFNotSampled")

# Reporting Unit
ru <- st_read(dsn = sndo_gdb,
              layer = "SNDOBoundary")

aea_crs = 'PROJCS["USA_Contiguous_Albers_Equal_Area_Conic",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Albers"],PARAMETER["False_Easting",0],PARAMETER["False_Northing",0],PARAMETER["central_meridian",-96],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["latitude_of_origin",37.5],UNIT["Meter",1]]'

## transform everything to AEA 
object_list <- lapply(X = list(terradat=terradat,lmf=lmf,tdat_designpoints=tdat_designpoints, tdat_designpolys=tdat_designpolys,
                               tdat_notsampled=tdat_notsampled,lmf_designpoints=lmf_designpoints, lmf_tpolys=lmf_tpolys,lmf_segments=lmf_segments,lmf_notsampled=lmf_notsampled,ru=ru),
                      FUN = function(X){
                        st_transform(X,crs = aea_crs)})

# quick map
blm_basemap_url <- "https://gis.blm.gov/arcgis/rest/services/lands/BLM_Natl_SMA_Cached_BLM_Only/MapServer/2"
blm_basemap <- arc.open(blm_basemap_url)

# filter to NM
blm_sma <- arc.select(blm_basemap,
                      where_clause = "ADMIN_ST = 'NV'")

blm_sma <- arc.data2sf(blm_sma)

# transform to AEA
blm_sma <- st_transform(blm_sma, aea_crs)

# dropping z value cis thats gonna cause issues
blm_sma <- st_zm(blm_sma, drop = TRUE)
ru <- st_zm(object_list[["ru"]], drop = TRUE)

# bb for map
bb <-  st_bbox(ru)

# make some bbox magic
bbox_new <- bb # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.3 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.3 * yrange) # ymax - top

# fix invalid polys
ru_valid <- ru %>% 
  st_buffer(0.5) %>% # make a buffer of half a meter around all parts (to avoid slivers)
  st_union() %>% # unite to a geometry object
  st_sf()

sma_valid <- blm_sma %>% 
  st_sf()

# basemap
tmap_mode("plot")
basemap <- tm_shape(ru_valid, bbox = bbox_new) +
  tm_borders()+
  tm_shape(sma_valid, bbox = bbox_new)+
  tm_polygons(col = "#fbea65")+
  tm_shape(ru_valid) +
  tm_borders(col = "black",
               lwd =1.5)+
  tm_legend()+
  tm_scale_bar(position =  c("LEFT", "BOTTOM"))+
  tm_shape(object_list$terradat)+
  tm_bubbles(col = "darkgreen",
             size =0.5)+
  tm_shape(object_list$lmf)+
  tm_bubbles(col = "green",
             size =0.5)

#basemap

# JOIN SAMPLED POINTS TO DESIGN INFO
design_points_df <- st_drop_geometry(object_list$tdat_designpoints)
design_polys_df <- st_drop_geometry(object_list$tdat_designpolys)

aim_design <- merge(x = object_list$terradat,
                    y = design_points_df,
                    by = c("DesignPointKey","DesignPolygonID"),
                    all.x = TRUE,
                    all.y = FALSE)

# How many actually have design info?
# percent
paste0(length(aim_design$DesignName[!is.na(aim_design$DesignName)])/nrow(aim_design)*100, "%")
summary(as.factor(aim_design$DesignName))

nrow(aim_design[aim_design$DesignName=="Targeted",]) #495 are targeted

# subset points to random ones
aim_design_random <- aim_design[aim_design$DesignPolygonID != "Targeted" & !is.na(aim_design$DesignPolygonID), ]

aim_design <- aim_design %>% 
  group_by(DesignPolygonID) %>% 
  sort_by(aim_design$DateVisited) %>% 
  mutate(ActualStratumOrder = row_number()) %>% 
  ungroup()

# lets look at point implementation order
summary(aim_design_random$WithinStratumOrder) #thats alot of NAs - fixed in Pro

# order within year 
aim_design$Year <- format(aim_design$DateVisited, "%Y")

aim_design <- aim_design %>% 
  group_by(Year) %>% 
  mutate(OrderDiff = abs(WithinStratumOrder-ActualStratumOrder)) %>% 
  ungroup()
           
# order within entire design?

# compare to random GRTS design to assess spatial balance
# first get all blm land in reporting unit
sample_frame <- object_list$tdat_designpolys[tdat_designpolys$DesignPolygonID == "NV_SouthernNevadaDO_2021_LUP",]

#draw simple unstratified GRTS design 
library(spsurvey)
# Seed number
seed_number <- 420

# taking code from nelsons og script
sample_frame_spdf <- methods::as(sf::st_as_sf(sf::st_zm(sample_frame)), "Spatial")

# this fails so hopefully we dont need it...
#sample_frame_spdf <- aim.analysis::repair_geometry(sample_frame,
#                                                   verbose = TRUE)
# count of existing points (minus revisits)
point_count <-  nrow(aim_design) - nrow(aim_design[aim_design$PrimaryKey != aim_design$Visit1PrimaryKey,])

design <- sample.design::allocate_panels(polygons = sample_frame,
                                         stratum_field = "DesignPolygonID",
                                         panel_names = "Nonrevisit",
                                         panel_sample_size = point_count,
                                         oversample_proportion = 0,
                                         oversample_min = 0)

template_points_spdf <- sample.design::grts_aim(design_object = design,
                                                frame = sample_frame,
                                                stratum_var = "DesignPolygonID",
                                                seed_number = seed_number)

# now calc mean nearest neighbour for both mock design and sampled points

# first remove revisits
aim_design_norevisits <- aim_design[aim_design$PrimaryKey ==aim_design$Visit1PrimaryKey,]

# define function
mean_nn <- function(sf){
  # first find the nn and calc distance
  nn_sampled <- nngeo::st_nn(x = sf,
                             y = sf,
                             k = 2,
                             returnDist = TRUE) # trying 2 so it doesnt just calc distance between itself
  
  # Since the projection is AEA, units should be meters
    # remove zeros
  nn_dist_sampled <- lapply(X = nn_sampled["dist"], FUN = function(X){
    Y <- as.data.frame(X)[2,]
    return(Y)
  })
  
  nn_dist_sampled <- unlist(nn_dist_sampled)
  
  # calc mean
  mean <- mean(nn_dist_sampled)
  sd <- sd(nn_dist_sampled)
  
  summary <- data.frame(Mean=mean, StdDev=sd)
  return(list(summary,nn_dist_sampled))# ill return the whole lsit too for a t test
}

summary_nn_sampled <- mean_nn(aim_design_norevisits)[[1]]
summary_nn_test <- mean_nn(template_points_spdf)[[1]]
number_sampled <- mean_nn(aim_design_norevisits)[[2]]
number_test <- mean_nn(template_points_spdf)[[2]]

summary_nn_sampled$Type <- "Sampled"
summary_nn_test$Type <- "Test"

# I going to combine them to plot
nn_summary <-  rbind(summary_nn_sampled, summary_nn_test) %>% 
  ggplot(aes(x = Mean, y = Type, fill = Type))+
  geom_col()+
  theme_bw()+
  geom_errorbar(aes(xmin = Mean-StdDev, xmax = Mean +StdDev), width = 0.5)
  
nn_summary
# compare the 2
ttest <- t.test(x = number_sampled, y = number_test)
# there is a signifcant difference in mean nearest neighbour distance -  the sampled points have a significantly lower mean distance than the GRTS test design
# the sampled also has a larger variance 
# these are a symptoms of spatial clustering

# merge AIM and LMF for simplicity
aim_lmf <- bind_rows(object_list$terradat, object_list$lmf)

# POINT SUMMARY
# Count by year
ggplot(data = aim_lmf, aes(x = format(aim_lmf$DateVisited, "%Y")))+
  geom_bar()+
  labs(x = "Year", y = "Count of AIM/LMF Plots")+
  theme_bw()

# Count by season/month
ggplot(data = aim_lmf, aes(x = format(aim_lmf$DateVisited, "%Y"), fill = format(aim_lmf$DateVisited, "%m")))+
  geom_bar()+
  labs(x = "Year", y = "Count of AIM/LMF Plots", fill = "Month")+
  theme_bw()+
  scale_fill_brewer(palette = "Paired")

ggplot(data = aim_lmf, aes(x = format(aim_lmf$DateVisited, "%m"), fill = format(aim_lmf$DateVisited, "%m")))+
  geom_bar()+
  labs(x = "Month", y = "Count of AIM/LMF Plots", fill = "Month")+
  theme_bw()+
  scale_fill_brewer(palette = "Paired")

# Look at point implementation - order and locations

# Climate projection rasters
path <- "C:\\Users\\alaurencetraynor\\Documents\\NV\\SNDO Report\\Climate"

# actual evapotranspiration
aet <- rast(x = paste0(path, "/TerraClimate2C_aet.nc"))
plot(aet)

#
def <- rast(x = paste0(path, "/TerraClimate2C_def.nc"))
pet <- rast(x = paste0(path, "/TerraClimate2C_pet.nc"))
ppt <- rast(x = paste0(path, "/TerraClimate2C_ppt.nc"))
q <- rast(x = paste0(path, "/TerraClimate2C_q.nc"))
soil <- rast(x = paste0(path, "/TerraClimate2C_soil.nc"))
swe <- rast(x = paste0(path, "/TerraClimate2C_swe.nc"))
tmax <- rast(x = paste0(path, "/TerraClimate2C_tmax.nc"))
tmin <- rast(x = paste0(path, "/TerraClimate2C_tmin.nc"))

# clip to sndo
for(rast in list(aet,def,pet,ppt,q,soil,swe,tmax,tmin)){
  
  rast_cip <- st_intersection(rast, sn)
}

### TEST OUT NELSON TPOLY analysis here

### TEST OUT AH's FUZZY CLUSTERING HERE to look at plant communities

### PULL IN DRGS and see how they can help

### grouping
# DRGS, ESDs, BPS, ecoregions, MLRA, cluster analysis

# Look at RMPs

# Guiding questions
# 1, whats available
# 2. what can we do
# 3. field office needs

# desert tortoise layer

# las vegas rmp will be renewed

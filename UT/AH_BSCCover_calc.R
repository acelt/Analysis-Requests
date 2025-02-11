library(RODBC)
library(tidyverse)
#devtools::install_github('Landscape-Data-Commons/terradactyl')
library(terradactyl)
library(sf)
library(arcgisbinding)
arc.check_product() 
library(scales)
library(arcgis)
############################ Caluclating AH Biological Soil Crust Cover ################################################
# Set paths
wd <- "C:\\Users\\alaurencetraynor\\Documents\\UT"
gdb <- paste0(wd, "/BENM_R_AIM.gdb")
aoi <- "HUC12_Intersect"
aim_lmf_url <-  "https://gis.blm.doi.net/arcgis/rest/services/vegetation/BLM_Natl_AIM_TerrADatAndLMF/MapServer/0"
tbl_lpiheader_url <- "https://gis.blm.doi.net/arcgis/rest/services/vegetation/BLM_Natl_AIM_TerrADatAndLMF/MapServer/30"
tbl_lpidetail_url <- "https://gis.blm.doi.net/arcgis/rest/services/vegetation/BLM_Natl_AIM_TerrADatAndLMF/MapServer/29"
lmf_lpi_url <-  'https://gis.blm.doi.net/arcgis/rest/services/vegetation/BLM_Natl_AIM_TerrADatAndLMF/MapServer/15'

# Import terrestrial data
# We can pull from the feature services to make this easy
# I saved this as csv from Arc since RODBC aint working right now
aim_lmf <- arcgisbinding::arc.open(aim_lmf_url)
terradat <-  arc.select(aim_lmf)
terradat_sf <- arc.data2sf(terradat)

# read in aoi and clip terradat to boundary
aoi_fc <- st_read(dsn = gdb,
                  layer = aoi)
aoi_fc_t <- st_transform(aoi_fc, crs = st_crs(terradat_sf))

terradat_clip <- st_intersection(terradat_sf, aoi_fc)

# subset to aim and lmf for calcs
lmf <- terradat_clip[terradat_clip$ProjectName == "LMF",]
aim <- terradat_clip[terradat_clip$ProjectName != "LMF",]

# grab lpi header info - terradactyl function wants this
tbllpiheader <- arc.open(tbl_lpiheader_url)

# reformat pk list to feed into where clause
pk_string <- paste(aim$PrimaryKey, collapse = "', '")

query <- paste0("PrimaryKey in ('", pk_string, "')")

# only need these two fields
tbllpiheader <- arc.select(tbllpiheader, where_clause = query)
tbllpiheader <- tbllpiheader[,c("LineKey", "RecKey")]

# grab LPI detail from terradat
terradat_lpi <- arc.open(tbl_lpidetail_url)
terradat_lpi <- arc.select(terradat_lpi, where_clause = query)

# Make tall
tdat_tall <- tidyr::pivot_longer(data = terradat_lpi,
                                 cols = c(TopCanopy:SoilSurface),
                                 names_to = "layer",
                                 values_to = "code")

# merge detail with line info for terradactyl
tdat_tall <- merge(x = tdat_tall,
                   y = tbllpiheader,
                   by = "RecKey",
                   all.x = T,
                   all.y = F)

tdat_tall <- tdat_tall[,c("PrimaryKey", "layer", "code", "PointNbr", "LineKey")]

# Calc field for soil crust
tdat_tall$bsc <- "NotBSC"
tdat_tall[tdat_tall$code %in% c("M", "LC","CY"), "bsc"] <- "BSC" 

# calc AH cover
aim_cover_bsc <- terradactyl::pct_cover(lpi_tall = tdat_tall,
                                        tall = F,
                                        hit = "any",
                                        by_line = F,
                                        bsc)
# Now repeat everything for LMF raw data
# grab LPI detail from lmf
lmf_lpi <- arc.open(lmf_lpi_url)

# reformat pk list to feed into where clause
pk_string <- paste(lmf$PrimaryKey, collapse = "', '")

query <- paste0("PrimaryKey in ('", pk_string, "')")

lmf_lpi <- arc.select(lmf_lpi, where_clause = query)

lmf_tall <- terradactyl::gather_lpi_lmf(PINTERCEPT = lmf_lpi)

lmf_tall$bsc <- "NotBSC"
lmf_tall[lmf_tall$code %in% c("M", "LC","CY"), "bsc"] <- "BSC" # do these make sense? including VL for now

lmf_cover_bsc <- terradactyl::pct_cover(lpi_tall = lmf_tall,
                                        tall = F,
                                        hit = "any",
                                        by_line = F,
                                        bsc)

#Combine AIM and LMF
aim_lmf_bsc <- rbind(lmf_cover_bsc, aim_cover_bsc)

# Adding indicator for just presence of BSC
aim_lmf_bsc$BSC_present <- "FALSE"
aim_lmf_bsc[aim_lmf_bsc$BSC>0,"BSC_present"] <- "TRUE"

# I think we can drop the notbsc cover field
aim_lmf_bsc <- aim_lmf_bsc[,c(1,2,4)]

write.csv(aim_lmf_bsc, paste0(wd,"/BENNM_AH_BSC_cover.csv"))

.libPaths(c("T:\\ProjectsNational\\AIM\\AIMDataTools\\R\\library", .libPaths()))
library(tidyverse)
#devtools::install_github('Landscape-Data-Commons/terradactyl')
library(terradactyl)
library(sf)
library(arcgis)

############################ Caluclating AH Cover ################################################

# Set token for AGOL API
token <- auth_binding()
set_arc_token(token)

# Import terrestrial data
ca_lpi <- arc_open(url = "https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/BLM_CA_Terrestrial_AIM_2024_Plot_Service/FeatureServer/13",
                   token = token)

ca_lpi_df <- arc_select(ca_gap)

ca_lpi_header <- arc_select(arc_open(url = "https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/BLM_CA_Terrestrial_AIM_2024_Plot_Service/FeatureServer/3",
                          token = token))

state_species <- arc_open(url = "https://gis.blm.doi.net/arcgis/rest/services/vegetation/BLM_Natl_AIM_TerrADatAndLMF/FeatureServer/51",
                          token = token)

state_speceis_df <- arc_select(state_species, where = "SpeciesState = 'CA'")



# grab LPI detail from terradat
terradat_lpi <- RODBC::sqlQuery(con1, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TBLLPIDETAIL;')

# grab lpi header info - terradactyl function wants this
tbllpiheader <- RODBC::sqlQuery(con1, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TBLLPIHEADER;')

# only need these two fields
tbllpiheader <- tbllpiheader[,c("LineKey", "RecKey")]

# Make tall - can also use terradactyl::gather_lpi
tdat_tall <- tidyr::pivot_longer(data = terradat_lpi,
                                 cols = c(TopCanopy:SoilSurface),
                                 names_to = "layer",
                                 values_to = "code")

terradactyl::gather_lpi()

# merge detail with line info for terradactyl
tdat_tall <- merge(x = tdat_tall,
                   y = tbllpiheader,
                   by = "RecKey",
                   all.x = T,
                   all.y = F)

tdat_tall <- tdat_tall[,c("PrimaryKey", "layer", "code", "PointNbr", "LineKey")]

# Calc field for soil crust
tdat_tall$bsc <- "NotBSC"
tdat_tall[tdat_tall$code %in% c("M", "LC","CY","VL"), "bsc"] <- "BSC" # do these make sense? including VL for now but may want to remove

# calc AH cover
aim_cover_bsc <- terradactyl::pct_cover(lpi_tall = tdat_tall,
                                        tall = F,
                                        hit = "any",
                                        by_line = F,
                                        bsc)

# Now repeat everything for LMF raw data
# grab LPI detail from lmf
lmf_lpi <- RODBC::sqlQuery(con1, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.PINTERCEPT;')

lmf_tall <- terradactyl::gather_lpi_lmf(PINTERCEPT = lmf_lpi)

lmf_tall$bsc <- "NotBSC"
lmf_tall[lmf_tall$code %in% c("M", "LC","CY", "VL"), "bsc"] <- "BSC" # do these make sense? including VL for now

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

# check geometry in the output, arcgidbinding is doing some odd things
arc.write(path = '\\\\blm.doi.net\\dfs\\loc\\egis\\GISUsers\\alaurencetraynor.BLM\\My Documents\\ArcGIS\\Projects\\BENM\\BENM.gdb\\terradat_bsc',
          data = aim_lmf_bsc,
          shape_info=shape_info,
          overwrite = TRUE)

write.csv(aim_lmf_bsc, "C:\\Users\\alaurencetraynor\\Documents\\bsc_cover.csv")

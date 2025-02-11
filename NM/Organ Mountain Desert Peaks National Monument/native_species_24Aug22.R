# Patrick Alexander, 30 Jun 2022
# This script uses my list of all exotic species in the western US and the 'SpeciesIndicators' table to calculate
# the proportion of the plants listed for each plot that are native, and the proportion of any hit foliar cover
# from native species. It then adds these values to the 'TerrADat' table as three new columns, 
# nativeProportion_speciesRichness, nativeProportion_foliarCover, and nativeFoliar.

library(tidyverse)
#devtools::install_github('Landscape-Data-Commons/terradactyl')
library(terradactyl)

#### creating species list group any hit cover function

## Making this into a function
cover_calc <-  function(directory, aim_plots, primarykey_field, species_list, group_name){
  
  # set directory
  setwd(directory)
  
  # Import list of plots from spreadsheets
  ost_aim <- read.csv(aim_plots)
  
  plotlist <- ost_aim[[primarykey_field]]
  
  # Add all of Terradat (this is the combined AIM and LMF layer)
  # although will need to split them later for calcs since LMF is wierd
  #Create connection
  conn <- RODBC::odbcConnect(dsn = "AIMPub", rows_at_time = 1)
  
  TerrADat <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMPubDBO.TerrestrialIndicators;')
  
  SpeciesIndicators <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMPubDBO.TerrestrialSpecies;')
  
  # subset
  TerrADat <- TerrADat[TerrADat$PrimaryKey %in% plotlist,]
  SpeciesIndicators <- SpeciesIndicators[SpeciesIndicators$PrimaryKey %in% plotlist,]
  
  # Create an "exotic" field in SpeciesIndicators. I'm using the "namecode" field so that it should match any 
  # name in SpeciesIndicators whether it is the accepted name or not.
  
  terradat_lpi <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TBLLPIDETAIL;')
  
  terradat_lpi_ost <- terradat_lpi[terradat_lpi$PrimaryKey %in% plotlist,]
  
  # grab lpi header info - terradactyl function wants this
  tbllpiheader <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TBLLPIHEADER;')
  
  tbllpiheader <- tbllpiheader[,c("LineKey", "RecKey")]
  
  tbllpiheader_ost <-tbllpiheader[tbllpiheader$RecKey %in% terradat_lpi_ost$RecKey,]
  
  # Make tall
  tdat_tall <- tidyr::pivot_longer(data = terradat_lpi_ost,
                                   cols = c(TopCanopy:SoilSurface),
                                   names_to = "layer",
                                   values_to = "code")
  
  tdat_tall <- merge(x = tdat_tall,
                     y = tbllpiheader_ost,
                     by = "RecKey",
                     all.x = T,
                     all.y = F)
  
  tdat_tall <- tdat_tall[,c("PrimaryKey", "layer", "code", "PointNbr", "LineKey")]
  
  tdat_tall[[group_name]]<- paste0("Not",group_name)
  
 
  tdat_tall[tdat_tall$code %in% species_list, group_name] <- group_name 
  
  aim_cover <- terradactyl::pct_cover(lpi_tall = tdat_tall,
                                             tall = F,
                                             hit = "any",
                                             by_line = F,
                                      .data[[group_name]])
  # rename field to be more helpful
  colnames(aim_cover)[2] <- paste0("AH_", group_name, "Cover")
  
   # join this to terradat for comparison
  TerrADat <- merge(x = TerrADat,
                    y = aim_cover,
                    by = "PrimaryKey")
  
  # remove weird spatial fields
  TerrADat <- apply(TerrADat,2,as.character)
  
  # And I guess we'll write that to csv.
  write.csv(TerrADat,paste0("TerrADat_", group_name, ".csv"))
  odbcCloseAll()
}

####################################################################################
# Import 'nominaTaxa'; from CSV for the national list...
nominaTaxa <- read.csv('C:\\Users\\alaurencetraynor\\Documents\\Old Spanish Trail\\nominaTaxa_20May22s.csv')
# Convert nulls to "FALSE" for the "exotic" field.
nominaTaxa <- nominaTaxa %>% mutate(exotic = ifelse(exotic =="","FALSE",exotic))

native_list <- nominaTaxa[nominaTaxa$exotic == FALSE ,"namCode"]

cover_calc(directory = "C:\\Users\\alaurencetraynor\\Documents\\OMDPNM",
           aim_plots = "OMDPNM_PLOTS.csv",
           primarykey_field = "PrimaryKey",
           species_list = native_list,
           group_name = "Native")

# now for non-native

exotic_list <- nominaTaxa[nominaTaxa$exotic == TRUE ,"namCode"]

cover_calc(directory = "C:\\Users\\alaurencetraynor\\Documents\\OMDPNM",
           aim_plots = "OMDPNM_PLOTS.csv",
           primarykey_field = "PrimaryKey",
           species_list = exotic_list,
           group_name = "Non-Native")

# invasive cover
nominaTaxa <- read.csv('C:\\Users\\alaurencetraynor\\Documents\\OMDPNM\\nominaTaxa_NewMexico_23Aug22.csv')

# Convert nulls to "FALSE" for the "INVASIVE" field.

nominaTaxa <- nominaTaxa %>% mutate(invasive = ifelse(invasive =="","FALSE",invasive))

# In my case, working from local csv copies of combined AIM + LMF data... replace with your TerrADat / 
# SpeciesIndicators import method of choice.

invasive_list <- nominaTaxa[nominaTaxa$invasive == TRUE ,"namCode"]

cover_calc(directory = "C:\\Users\\alaurencetraynor\\Documents\\OMDPNM",
           aim_plots = "OMDPNM_PLOTS.csv",
           primarykey_field = "PrimaryKey",
           species_list = invasive_list,
           group_name = "Invasive")

# invasive perennial grass cover
invasive_pg_list <- nominaTaxa[nominaTaxa$invasive == TRUE & nominaTaxa$Duration == "Perennial" & nominaTaxa$GrowthHabitSub == "Graminoid","namCode"]

cover_calc(directory = "C:\\Users\\alaurencetraynor\\Documents\\OMDPNM",
           aim_plots = "OMDPNM_PLOTS.csv",
           primarykey_field = "PrimaryKey",
           species_list = invasive_pg_list,
           group_name = "InvasivePerenGrass")


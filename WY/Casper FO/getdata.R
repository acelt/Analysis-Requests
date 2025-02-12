library(arcgis)
library(arcgisbinding)
library(tidyverse)

arc.check_product()

data <- arc.open("https://gis.blm.doi.net/arcgis/rest/services/vegetation/BLM_Natl_AIM_TerrADatAndLMF/MapServer/0")

data_df <- arc.select(data)

data_cafo <- data_df |> 
  mutate(year = format(as.POSIXct(DateVisited), "%Y")) |> 
  filter(year == 2024,
         State == "WY",
         grepl("Casper", ProjectName)|grepl("CAFO", PlotID)) |> 
  select(PlotID,
         DateVisited,
         BareSoilCover,
         TotalFoliarCover,
         SoilStability_All,
         AH_PerenForbCover,
         AH_PerenGrassCover,
         AH_AnnGrassCover,
         AH_SagebrushCover,
         Hgt_Herbaceous_Avg,
         Hgt_Sagebrush_Avg,
         SagebrushShape_All_Predominant,
         Spp_Sagebrush,
         ProjectName,
         PhotoLink,
         EcologicalSiteId)

species_data <- arc.open("https://gis.blm.doi.net/arcgis/rest/services/vegetation/BLM_Natl_AIM_TerrADatAndLMF/MapServer/1")

species_data_df <- arc.select(species_data, where_clause = "SpeciesState = 'WY'") |> 
  filter(PlotID %in% data_cafo$PlotID)

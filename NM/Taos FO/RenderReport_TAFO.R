### Thank you for your interest in running the Ecological Site Summary Tool!
### Define all of the arguments below. 
### Make sure each command is run in order. 
### Your report may take several minutes to run, depending on your network connection
### The output will be in your WorkingFolder.
### Open the html output in Google Chrome or Mozilla Firefox. 

########################################### FUNCTIONS ############################################
Combine_AIM_LMF_alt <- function(TerrADat_Path, EDIT_List_Path, groups, group_name, Internal, use_EDIT = TRUE){
  
  if(!Internal){
    TerrADat <- sf::st_read(dsn = TerrADat_Path , layer = "TerrADat")
    LMF <- sf::st_read(dsn = TerrADat_Path , layer = "LMF")
    TerrADat <- as.data.frame(TerrADat)
    TerrADat <- dplyr::select(TerrADat, -Shape)
    LMF <- as.data.frame(LMF)
    LMF <- dplyr::select(LMF, -Shape)}
  
  if(Internal){
    TerrADat <- TerrADat
    LMF <- LMF
  }
  
  #This method only works for ecosites in EDIT currently - there a re a number of older ecosites in teh LMF that arent present - these will be replaced by NAs using the following code
  #Added a check above to ask if this is what you want - otherwise just add R/F
  
  #Merge the dataframe with the full EcologicalSiteId and dropped R/F Id with the LMF
  if(use_EDIT){
    EDIT <- read.csv(file = paste0(EDIT_List_Path, "EDIT_public_ecological_site_list.csv"))
    
    #Read in full csv of ecological site ids from EDIT
    
    EDIT[["EcoSiteId_Stripped"]] <- gsub(EDIT[["new_es_symbol"]],
                                         pattern = "^[RF]", replacement = "")
    
    #Check to see if unique
    ecosite_lut <- unique(EDIT[,c("new_es_symbol" , "EcoSiteId_Stripped")])
    
    #Pull out the repeat ids (fortunatley there are only 15)
    
    trouble_ids <- names(table(ecosite_lut[["EcoSiteId_Stripped"]]))[table(ecosite_lut[["EcoSiteId_Stripped"]]) > 1]
    
    #Drop the repeat ids
    '%notin%' <- Negate('%in%')
    
    ecosite_lut_drop_bad <- ecosite_lut[ecosite_lut$EcoSiteId_Stripped %notin% trouble_ids,]
    
    #merge the lut with LMF table
    LMF_EcoSite <- merge(x = LMF , y = ecosite_lut_drop_bad, by.x = "EcologicalSiteId", by.y = "EcoSiteId_Stripped",  all.x = TRUE, all.y = FALSE)
    
    #Drop the EcologicalSiteId value that we added earlier
    LMF_EcoSite$EcologicalSiteId <- LMF_EcoSite$new_es_symbol
  } else {LMF_EcoSite <- LMF}
  
  #Read in csv of ecological site ids / PKs
  #Merge LUT with terradat and rename ecosite column
  
  TDat_grouped <- merge(x = TerrADat,
                        y = groups[,c("PrimaryKey", group_name)],
                        by = "PrimaryKey",
                        all.x = TRUE)
  
  TDat_grouped$EcologicalSiteId <- TDat_grouped[[group_name]]
  
  # Characterize to prevent rbind factor issues
  TDat_grouped$EcologicalSiteId <- as.character(TDat_grouped$EcologicalSiteId)
  LMF_EcoSite$EcologicalSiteId <- as.character(LMF_EcoSite$EcologicalSiteId)
  
  # r bind function to add NAs to mismatched columns
  rbind.all.columns <- function(x, y) {
    
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    
    x[, c(as.character(y.diff))] <- NA
    
    y[, c(as.character(x.diff))] <- NA
    
    return(rbind(x, y))
  }
  
  TDat_LMF <- rbind.all.columns(TDat_grouped, LMF_EcoSite)
  
  # add factor levels back in
  TDat_LMF$EcologicalSiteId <- as.factor(TDat_LMF$EcologicalSiteId)
  
  #Pull out year for future
  TDat_LMF$DateVisited <- sub("^(\\d{4}).*$", "\\1", TDat_LMF$DateVisited)
  TDat_LMF <- TDat_LMF %>% dplyr::rename(Year = DateVisited)
  
  
  return(TDat_LMF)
  
}

AttributePlots_alt <- function(TDat_LMF, 
                               shapefile_name, shapefile_path, 
                               attribute_title, attribute_name){
  if(missing(attribute_name)){
    #if attribute_name is included, will subset data to attribute 
    #(i.e. 1 or multiple allotments)
    attribute_name <- NA
  }
  
  # Match coordinate reference systems for intersection
  projection <- sf::st_crs("+proj=longlat +datum=NAD83")
  
  # Join by PrimaryKey to get GPS coordinates for species indicators entries
  # TDat_LMF is output of Combine_AIM_LMF
  coordinates <- TDat_LMF %>% dplyr::select(PrimaryKey, Latitude_NAD83, Longitude_NAD83)
  
  # Convert Terradat data into "simple feature" object class for spatial reference. Not removing any NA or entries without coordinates.
  TDat_LMF_Spatial <- sf::st_as_sf(TDat_LMF, coords = c("Longitude_NAD83", "Latitude_NAD83"), na.fail = FALSE, remove = FALSE, crs = projection)
  
  ##-----------------------------------
  # Load in shapefiles and intersect with data (include AND exclude data in sf)
  ##-----------------------------------
  
  # Read in shapefiles
  
  shapefile  <-  sf::st_read(dsn = shapefile_path, layer = shapefile_name)
  shapefile <- sf::st_transform(shapefile, crs = projection)
  #Simplify shapefile to just the attributes we want and filter by attribute name
  shapefile <- shapefile %>% dplyr::select(all_of(attribute_title))
  
  # Intersect shapefile with plots to get attributed
  TDat_LMF_Attributed <- sf::st_intersection(TDat_LMF_Spatial, sf::st_make_valid(shapefile))
  
  output <- subset(TDat_LMF_Attributed, TDat_LMF_Attributed[[attribute_title]] == attribute_name) 
  
  return(output)
  
}
# making a few changes to the make map function
MakeMap_alt <- function(EcologicalSiteId, TDat_LMF){
  
  ##Caption to use in your tables and plots
  Caption <- paste0("Cover Summaries for " , attribute_name)
  
  #Clean up
  TDat_LMF <- TDat_LMF %>%
    dplyr::arrange(EcologicalSiteId) %>%
    dplyr::filter(Latitude_NAD83 > 0)
  
  # List of ecological sites for legend
  EcoSiteList <- unique(TDat_LMF$EcologicalSiteId)
  
  #Set color palettes
  # removed palette for ecosite name as Im not using that
  Pal_EcoSite <- leaflet::colorFactor(palette = 'Accent' , domain = TDat_LMF$EcologicalSiteId)
  Pal_Date <- leaflet::colorFactor(palette = 'YlOrRd' , domain = TDat_LMF$Time_Period)
  Time_Period <- TDat_LMF$Time_Period
  
  Map <- leaflet::leaflet(height = 650 , width = 650)
  
  #Convert vector to string to use in caption
  EcoSiteCaption <- toString(EcologicalSiteId)
  
  if(Groups) {
    # Reading in Group polygon
    projection <- sf::st_crs("+proj=longlat +datum=NAD83")
    poly <- sf::st_read(dsn = shapefile_path, layer = shapefile_name, quiet = TRUE)
    poly <- sf::st_transform(poly, crs = projection)
    poly <- methods::as(poly, "Spatial")
    
    # Select only the features of interest
    poly <- poly[poly[[attribute_title]]== attribute_name,]
    
    # Making Map with polygon
    Map <- leaflet::addTiles(Map) %>% leaflet::addCircleMarkers(lng = ~Longitude_NAD83 , lat = ~Latitude_NAD83 ,
                                                                popup = paste("Time period: " , TDat_LMF$Time_Period,
                                                                              sep = "<br>") ,
                                                                color = ~Pal_Date(Time_Period) ,
                                                                fillOpacity = .5 , group = "Time_Period",
                                                                radius = ~ifelse(Time_Period == "2011-2015", 3, 5),
                                                                data = TDat_LMF) %>%
      leaflet::addCircleMarkers(lng = ~Longitude_NAD83 , lat = ~Latitude_NAD83 ,
                                fillOpacity = 0.5 ,
                                popup = paste("Ecological Site Id: " , TDat_LMF$EcologicalSiteId ,
                                              sep = "<br>") ,
                                color = ~Pal_EcoSite(EcologicalSiteId) , group = "EcologicalSiteId" ,
                                data = TDat_LMF) %>%
      leaflet::addLayersControl(overlayGroups = c("EcologicalSiteId", "Time_Period") ,
                                options = leaflet::layersControlOptions(collapsed = TRUE)) %>%
      leaflet::addLegend(pal = Pal_Date , values = TDat_LMF$Time_Period , opacity = 1 , group = "Time_Period") %>%
      leaflet::addLegend(pal = Pal_EcoSite , values = TDat_LMF$EcologicalSiteId , opacity = 1 , group = "EcologicalSiteId") %>%
      addPolygons(data = poly, fillColor = "transparent",
                  color = "black")
    
    return(Map)
    
  } else{
    
    # Making Map without polygon
    Map <- leaflet::addTiles(Map) %>% leaflet::addCircleMarkers(lng = ~Longitude_NAD83 , lat = ~Latitude_NAD83 , radius = 3 ,
                                                                popup = paste("Ecological Site: " , TDat_LMF$es_name,
                                                                              "Ecolgical Site Id: " , TDat_LMF$EcologicalSiteId,
                                                                              sep = "<br>") ,
                                                                color = ~Pal_Date(Time_Period) ,
                                                                fillOpacity = .5 , group = Time_Period ,
                                                                data = TDat_LMF) %>%
      leaflet::addCircleMarkers(lng = ~Longitude_NAD83 , lat = ~Latitude_NAD83 ,
                                radius = 3,
                                fillOpacity = 0.5 ,
                                popup = paste("Ecological Site: " ,TDat_LMF$es_name ,
                                              "Ecological Site Id: " , TDat_LMF$EcologicalSiteId ,
                                              sep = "<br>") ,
                                color = ~Pal_EcoSite(EcologicalSiteId) , group = EcologicalSiteId ,
                                data = TDat_LMF) %>%
      leaflet::addLayersControl(overlayGroups = c(EcologicalSiteId , Time_Period) ,
                                options = leaflet::layersControlOptions(collapsed = TRUE)) %>%
      leaflet::addLegend(pal = Pal_Date , values = TDat_LMF$Time_Period , opacity = 1 , group = Time_Period) %>%
      leaflet::addLegend(pal = Pal_EcoSite , values = EcologicalSiteId , opacity = 1 , group = EcologicalSiteId)
    
    return(Map)
  }
  
}
SummaryTables_WithAttributes_alt <- function(EcoSitePlots, Species_plots_ecosite, 
                                             SummaryVar, SummarizeBy, Attributed_Pks){
  
  #Prep
  SpeciesList <- SpeciesList %>% dplyr::select(Species, ScientificName, CommonName,
                                               Family, SpeciesState,
                                               SynonymOf, UpdatedSpeciesCode) %>% 
    dplyr::mutate(link = paste("https://plants.sc.egov.usda.gov/core/profile?symbol=", Species, sep = ""))
  
  #Merge with species list so we can hover for scientific name
  Species_plots_ecosite_attributed <- sp::merge(Species_plots_ecosite, Attributed_Pks, by = "PrimaryKey", all = TRUE) %>% 
    unique() 
  
  # Added in attribute title here instead of allotment name
  Species_plots_ecosite_attributed <- sp::merge(Species_plots_ecosite_attributed , SpeciesList , by = c("Species" , "SpeciesState")) %>% 
    dplyr::select(Species, ScientificName, Family, SpeciesState,
                  SynonymOf, UpdatedSpeciesCode, CommonName, PrimaryKey, 
                  PlotID,  AH_SpeciesCover, 
                  AH_SpeciesCover_n, Hgt_Species_Avg, 
                  Hgt_Species_Avg_n, GrowthHabit, GrowthHabitSub, Duration, 
                  Noxious, SG_Group, link, !!attribute_title, Time_Period, EcologicalSiteId) %>%
    dplyr::mutate_if(is.numeric, round , digits = 2) 
  
  # For some reason there are plots in here with no data. Filtering them out. 
  
  Species_plots_ecosite_attributed <- Species_plots_ecosite_attributed %>% filter(!is.na(Species),!is.na(GrowthHabit))
  
  # the input tdat_lmf attributed already has atribute heres, no need to merge
  EcoSitePlots_Attributed <- EcoSitePlots %>% filter(!is.na(TotalFoliarCover),)
  
  EcoSitePlots_Attributed <- sf::st_set_geometry(EcoSitePlots_Attributed, NULL)
  
  #Get Noxious versus Non in Standard Format
  
  Species_plots_ecosite_attributed$Noxious <- gsub("YES" , "Yes", Species_plots_ecosite_attributed$Noxious)
  Species_plots_ecosite_attributed$Noxious <- gsub("NO", "No", Species_plots_ecosite_attributed$Noxious)
  
  # Prep species richness for trace species
  
  #Prep
  
  #Detected in richness only on plot
  RichnessPresent <- Species_plots_ecosite_attributed %>% filter(is.na(AH_SpeciesCover))
  #Detected in LPI on plot
  LPI_Present <- Species_plots_ecosite_attributed %>% filter(AH_SpeciesCover > 0.000000)
  #Removes duplicates
  LPI_Present_String <- unique(LPI_Present$Species)
  #Removes values from richness that also occurred in LPI
  RichnessSpecies_Only <- RichnessPresent[!(RichnessPresent[["Species"]] %in% LPI_Present_String),]
  #Removes duplicates
  TraceCover_List <- unique(RichnessSpecies_Only$Species)
  #Get into dataframe (just select state species that were trace)
  TraceSpeciesCover <- Species_plots_ecosite_attributed[Species_plots_ecosite_attributed[["Species"]] %in% TraceCover_List,]
  
  TraceCover_Table_SpList <- TraceSpeciesCover %>%
    dplyr::select(Species, ScientificName , Family , GrowthHabit ,
                  GrowthHabitSub , Duration, Noxious , SG_Group ,
                  SynonymOf , CommonName ,
                  UpdatedSpeciesCode, link, EcologicalSiteId, Time_Period) %>% unique() %>% filter(!is.na(Species))
  
  if(SummaryVar == "Species" & SummarizeBy == "Plot"){
    #hyperlink species
    Species_plots_ecosite_attributed$Species <- paste0("<a href='",Species_plots_ecosite_attributed$link,"'>",Species_plots_ecosite_attributed$Species,"</a>")
    
    table <- Species_plots_ecosite_attributed %>% select(-link) %>% filter(!is.na(AH_SpeciesCover)) %>% 
      DT::datatable(escape = FALSE, extensions = 'Buttons', filter = "top" , 
                    options = list(scrollX = TRUE,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(extend = 'collection', buttons = c('csv', 'excel'),
                                               text = 'Download Table'))) , 
                    caption = (paste("Percent Cover by Species by Plot within " , attribute_name)) , 
                    rownames = FALSE)
  }
  
  if(SummaryVar == "Species" & SummarizeBy == "EcologicalSite"){
    #For summarizing across all plots
    # well add in time periodd here too
    Species_cover_summary <- Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover)) %>% 
      mutate(Tally = 1) %>%
      group_by(Species , GrowthHabit , GrowthHabitSub , 
               Duration , Noxious , ScientificName , 
               CommonName , SG_Group, EcologicalSiteId, Time_Period) %>% 
      summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
                StandardDeviation = sd(AH_SpeciesCover),
                MinCover = min(AH_SpeciesCover) ,
                MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
      mutate_if(is.numeric, round , digits = 2) %>%
      dplyr::select(Species, ScientificName, 
                    AveragePercentCover, StandardDeviation,
                    MinCover, MaxCover, n, GrowthHabit, 
                    GrowthHabitSub, Duration, 
                    Noxious, CommonName, SG_Group, EcologicalSiteId, Time_Period)
    
    #hyperlink species
    Species_cover_summary$Species <- paste0("<a href='",Species_cover_summary$link,"'>", Species_cover_summary$Species,"</a>")
    
    table <- Species_cover_summary %>% DT::datatable(escape = FALSE, 
                                                     extensions = 'Buttons', 
                                                     filter = "top" , 
                                                     options = list(scrollX = TRUE ,
                                                                    dom = 'Bfrtip',
                                                                    buttons =
                                                                      list(list(
                                                                        extend = 'collection',
                                                                        buttons = c('csv', 'excel'),
                                                                        text = 'Download Table'))) , 
                                                     caption = (paste("Average Percent Cover Values Across" , attribute_name)) , 
                                                     rownames = FALSE)
  }
  
  
  if(SummaryVar== "GrowthHabitSub" & SummarizeBy == "Plot"){
    
    table <-  Species_plots_ecosite_attributed %>% 
      group_by(PrimaryKey , PlotID , GrowthHabitSub , Duration) %>% 
      filter(!is.na(AH_SpeciesCover)) %>% 
      summarize(PercentCover = sum(AH_SpeciesCover)) %>%
      mutate_if(is.numeric, round , digits = 2) %>% 
      filter(!is.na(GrowthHabitSub)) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" ,  
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) ,
                    caption = (paste("Percent Cover by Structure and Functional Group by Plot  within " , 
                                     attribute_name)), 
                    rownames = FALSE)
  }
  
  if(SummaryVar == "GrowthHabitSub" & SummarizeBy == "EcologicalSite"){
    #removes trace species
    table <- Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover)) %>% 
      mutate(Tally = 1) %>%
      group_by(GrowthHabitSub, Duration, Tally, EcologicalSiteId, Time_Period) %>%
      summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
                StandardDeviation = sd(AH_SpeciesCover),
                MinCover = min(AH_SpeciesCover) ,
                MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
      mutate_if(is.numeric, round , digits = 2) %>%
      select(-Tally) %>%
      filter(!is.na(GrowthHabitSub)) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" ,  
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) ,
                    caption = (paste("Percent Cover by Structure and Functional Group in " , 
                                     attribute_name)), 
                    rownames = FALSE)
    
  }
  
  if(SummaryVar== "Noxious" & SummarizeBy == "Plot"){
    
    table <- EcoSitePlots_Attributed %>% dplyr::select(PlotID, PrimaryKey, AH_NoxCover, AH_NonNoxCover, EcologicalSiteId, Time_Period) %>%
      filter(!is.na(AH_NonNoxCover)) %>%
      rename(NonNoxious = AH_NonNoxCover, Noxious = AH_NoxCover) %>%
      dplyr::mutate_if(is.numeric, round , digits = 2) %>% 
      DT::datatable(extensions = 'Buttons', filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Percent Cover Noxious Versus Non by Plot within " , 
                                     attribute_name)) , 
                    rownames = FALSE)
  }
  
  if(SummaryVar== "Noxious" & SummarizeBy == "EcologicalSite"){
    
    prep <-  EcoSitePlots_Attributed %>% dplyr::select(PlotID, PrimaryKey, AH_NoxCover, AH_NonNoxCover, EcologicalSiteId, Time_Period) %>% 
      filter(!is.na(AH_NonNoxCover)) %>%
      dplyr::rename(NonNoxious = AH_NonNoxCover, Noxious = AH_NoxCover) %>%
      gather(key = "Noxious", value = Percent,
             NonNoxious:Noxious) %>%
      dplyr::mutate(Tally = 1) 
    
    prep$Noxious <- gsub("NonNoxious" , "No", prep$Noxious)
    prep$Noxious <- gsub("Noxious", "Yes", prep$Noxious)
    
    table <-   prep %>% group_by(Noxious, EcologicalSiteId, Time_Period) %>% 
      summarize(AveragePercentCover = mean(Percent) ,
                StandardDeviation = sd(Percent),
                MinCover = min(Percent) ,
                MaxCover = max(Percent) , n = sum(Tally)) %>%
      mutate_if(is.numeric, round , digits = 2) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Percent Cover Noxious Versus Non in " , 
                                     attribute_name)) , 
                    rownames = FALSE)
    
  }
  
  if(SummaryVar == "Woody" & SummarizeBy == "Plot"){
    
    table <-  Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover)) %>%
      group_by(GrowthHabit , PrimaryKey , PlotID) %>%
      summarize(PercentCover = sum(AH_SpeciesCover)) %>%
      mutate_if(is.numeric, round , digits = 2) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Percent Cover Woody vs. Non by Plot within: " , attribute_name)) , 
                    rownames = FALSE)
    
  }
  
  if(SummaryVar == "Woody" & SummarizeBy == "EcologicalSite"){
    
    table <- Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover)) %>%
      mutate(Tally = 1) %>%
      group_by(GrowthHabit , Tally, EcologicalSiteId, Time_Period) %>%
      summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
                StandardDeviation = sd(AH_SpeciesCover),
                MinCover = min(AH_SpeciesCover) ,
                MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
      subset(AveragePercentCover > 0.0000) %>%
      mutate_if(is.numeric, round , digits = 2) %>% 
      dplyr::select(-Tally) %>%
      filter(!is.na(GrowthHabit)) %>% 
      DT::datatable(extensions = 'Buttons', 
                    filter = "top" , options = list(scrollX = TRUE ,
                                                    dom = 'Bfrtip',
                                                    buttons =
                                                      list(list(
                                                        extend = 'collection',
                                                        buttons = c('csv', 'excel'),
                                                        text = 'Download Table'))) , 
                    caption = (paste("Percent Cover Woody vs. Non in: " , 
                                     attribute_name)) , 
                    rownames = FALSE)
    
  }
  
  
  if(SummaryVar == "SageGrouseGroup" & SummarizeBy == "Plot"){
    table <-     Species_plots_ecosite_attributed %>% filter(!is.na(SG_Group)) %>% 
      filter(!is.na(AH_SpeciesCover)) %>%
      group_by(SG_Group, PrimaryKey , PlotID) %>%
      summarize(PercentCover = sum(AH_SpeciesCover)) %>%
      mutate_if(is.numeric, round , digits = 2) %>% 
      DT::datatable(extensions = 'Buttons', 
                    filter = "top" ,  options = list(scrollX = TRUE ,
                                                     dom = 'Bfrtip',
                                                     buttons =
                                                       list(list(
                                                         extend = 'collection',
                                                         buttons = c('csv', 'excel'),
                                                         text = 'Download Table'))) , 
                    caption = (paste("Percent Cover by Sage-Grouse Group by Plot within: " , 
                                     attribute_name)), 
                    rownames = FALSE)
  }
  
  if(SummaryVar == "SageGrouseGroup" & SummarizeBy == "EcologicalSite"){
    
    table <-  Species_plots_ecosite_attributed %>% filter(!is.na(SG_Group)) %>% 
      filter(!is.na(AH_SpeciesCover)) %>% mutate(Tally = 1) %>%
      group_by(SG_Group, Tally, EcologicalSiteId, Time_Period) %>%
      summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
                StandardDeviation = sd(AH_SpeciesCover),
                MinCover = min(AH_SpeciesCover) ,
                MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
      mutate_if(is.numeric, round , digits = 2) %>%
      dplyr::select(-Tally) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" ,  
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) ,
                    caption = (paste("Percent Cover by Sage-Grouse Group in " , 
                                     attribute_name)) , 
                    rownames = FALSE)
    
  }
  
  if(SummaryVar == "PreferredForb" & SummarizeBy == "Plot"){
    
    table <- Species_plots_ecosite_attributed %>% 
      mutate(PreferredForb = (SG_Group == "PreferredForb")) %>% 
      subset(PreferredForb == TRUE) %>% 
      subset(AH_SpeciesCover > 0.0000) %>%
      filter(!is.na(AH_SpeciesCover)) %>%
      group_by(Species, PrimaryKey , PlotID) %>%
      summarize(PercentCover = sum(AH_SpeciesCover)) %>%
      mutate_if(is.numeric, round , digits = 2) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" ,  
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Percent Cover by Preferred Forb By Plot within " , 
                                     attribute_name)) , 
                    rownames = FALSE)
    
    
  }
  
  if(SummaryVar == "PreferredForb" & SummarizeBy == "EcologicalSite"){
    
    table <- Species_plots_ecosite_attributed %>% 
      mutate(PreferredForb = (SG_Group == "PreferredForb")) %>% 
      subset(PreferredForb == TRUE) %>% 
      subset(AH_SpeciesCover > 0.0000) %>%
      filter(!is.na(AH_SpeciesCover)) %>%
      filter(!is.na(AH_SpeciesCover)) %>%
      mutate(Tally = 1) %>%
      group_by(Species, Tally, EcologicalSiteId, Time_Period) %>%
      summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
                StandardDeviation = sd(AH_SpeciesCover),
                MinCover = min(AH_SpeciesCover) ,
                MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
      mutate_if(is.numeric, round , digits = 2) %>% dplyr::select(-Tally) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" ,  
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Percent Cover by Preferred Forb in " , 
                                     attribute_name)) , 
                    rownames = FALSE)
    
  }
  
  if(SummaryVar == "TraceSpecies" & SummarizeBy == "Plot"){
    
    RichnessPresent <-  RichnessPresent %>% 
      dplyr::select(Species, ScientificName , GrowthHabit ,
                    GrowthHabitSub , Duration, Noxious , SG_Group, 
                    PrimaryKey, PlotID, link, EcologicalSiteId, Time_Period) %>% filter(!is.na(Species))
    
    RichnessPresent$Species <- paste0("<a href='",RichnessPresent$link,"'>", RichnessPresent$Species,"</a>")
    
    table <- RichnessPresent %>% select(-link) %>%  
      DT::datatable(escape = FALSE, extensions = 'Buttons', 
                    filter = "top" , options = list(scrollX = TRUE ,
                                                    dom = 'Bfrtip',
                                                    buttons =
                                                      list(list(
                                                        extend = 'collection',
                                                        buttons = c('csv', 'excel'),
                                                        text = 'Download Table'))) , 
                    caption = (paste("Trace Species by Plot within " , 
                                     attribute_name)) , 
                    rownames = FALSE)
    
  }
  
  if(SummaryVar == "TraceSpecies" & SummarizeBy == "EcologicalSite"){
    
    
    TraceCover_Table_SpList$Species <- paste0("<a href='",TraceCover_Table_SpList$link,"'>", TraceCover_Table_SpList$Species,"</a>")
    
    table <- TraceCover_Table_SpList %>% arrange(Species) %>% 
      select(-link) %>% 
      DT::datatable(escape = FALSE, 
                    extensions = 'Buttons', 
                    filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Trace species in " , 
                                     attribute_name)) , 
                    rownames = FALSE)
    
    
  }
  
  if(SummaryVar == "GroundCover" & SummarizeBy == "Plot"){
    
    table <- EcoSitePlots_Attributed %>% dplyr::select(PlotID, PrimaryKey, BareSoilCover , 
                                                       TotalFoliarCover , FH_TotalLitterCover , 
                                                       FH_RockCover, EcologicalSiteId, Time_Period) %>% 
      gather(key = Indicator , value = Percent, 
             BareSoilCover:FH_RockCover) %>%
      filter(!is.na(Percent)) %>% mutate(Tally = 1) %>% 
      group_by(PlotID, PrimaryKey, Indicator) %>% 
      mutate_if(is.numeric, round , digits = 2) %>% select(-Tally) %>% 
      rename(PercentCover = Percent) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Percent cover by plot within " , 
                                     attribute_name)) , 
                    rownames = FALSE)
  }
  
  if(SummaryVar == "GroundCover" & SummarizeBy == "EcologicalSite"){
    
    table <- EcoSitePlots_Attributed %>% dplyr::select(PlotID, PrimaryKey, BareSoilCover , 
                                                       TotalFoliarCover , FH_TotalLitterCover , 
                                                       FH_RockCover, EcologicalSiteId, Time_Period) %>%
      gather(key = Indicator , value = Percent, 
             BareSoilCover:FH_RockCover) %>% 
      filter(!is.na(Percent)) %>% mutate(Tally = 1) %>%
      group_by(Indicator, EcologicalSiteId, Time_Period) %>%
      summarize(AveragePercentCover = mean(Percent) ,
                Standard_Deviation = sd(Percent) ,
                Low = min(Percent) ,
                High = max(Percent), n = sum(Tally)) %>% 
      mutate_if(is.numeric, round , digits = 2) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Average percent cover in " , 
                                     attribute_name)), 
                    rownames = FALSE)
  }
  
  if(SummaryVar == "Gap" & SummarizeBy == "Plot"){
    
    table  <- EcoSitePlots_Attributed %>% dplyr::select(PlotID , PrimaryKey , 
                                                        GapCover_25_50 , GapCover_51_100 , 
                                                        GapCover_101_200 , GapCover_200_plus , 
                                                        GapCover_25_plus, EcologicalSiteId, Time_Period) %>% 
      gather(key = Gap_Class_cm , 
             value = Percent , GapCover_25_50:GapCover_25_plus) %>%
      filter(!is.na(Percent)) %>% 
      mutate_if(is.numeric , round, digits = 2) %>% 
      group_by(PlotID , PrimaryKey) %>%  
      mutate_if(is.numeric, round , digits = 2) %>% 
      rename(Percent_Cover = Percent) %>% 
      DT::datatable(extensions = 'Buttons', filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Percent cover by canopy gap class by plot within " , 
                                     attribute_name)) , 
                    rownames = FALSE)
  }
  
  if(SummaryVar == "Gap" & SummarizeBy == "EcologicalSite"){
    
    table <- EcoSitePlots_Attributed %>% dplyr::select(PlotID , PrimaryKey , 
                                                       GapCover_25_50 , GapCover_51_100 , 
                                                       GapCover_101_200 , GapCover_200_plus , 
                                                       GapCover_25_plus, EcologicalSiteId, Time_Period) %>% 
      gather(key = Gap_Class_cm , 
             value = Percent , GapCover_25_50:GapCover_25_plus) %>%
      filter(!is.na(Percent)) %>%
      mutate_if(is.numeric , round, digits = 2) %>%
      group_by(Gap_Class_cm, EcologicalSiteId, Time_Period) %>%
      summarize(AveragePercentCover = mean(Percent) ,
                StandardDeviation = sd(Percent),
                MinPercentCover = min(Percent) ,
                MaxPercentCover = max(Percent)) %>%
      mutate_if(is.numeric, round , digits = 2) %>% 
      DT::datatable(extensions = 'Buttons', 
                    filter = "top" , options = list(scrollX = TRUE ,
                                                    dom = 'Bfrtip',
                                                    buttons =
                                                      list(list(
                                                        extend = 'collection',
                                                        buttons = c('csv', 'excel'),
                                                        text = 'Download Table'))) , 
                    caption = (paste("Percent cover by canopy gap class in: " , 
                                     attribute_name)) , 
                    rownames = FALSE)
    
  }
  
  if(SummaryVar == "SoilStability" & SummarizeBy == "Plot"){
    
    table <-  EcoSitePlots_Attributed %>% dplyr::select(PlotID , PrimaryKey , 
                                                        SoilStability_All , 
                                                        SoilStability_Protected , 
                                                        SoilStability_Unprotected, EcologicalSiteId, Time_Period) %>%
      gather(key = Veg , value = Rating , 
             SoilStability_All:SoilStability_Unprotected) %>%
      filter(!is.na(Rating)) %>% 
      mutate_if(is.numeric, round, digits = 2)  %>% 
      group_by(PrimaryKey , PlotID) %>% 
      mutate_if(is.numeric, round , digits = 2) %>%
      DT::datatable(extensions = 'Buttons', 
                    filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Soil stability ratings by plot in: " , 
                                     attribute_name)) , 
                    rownames = FALSE)
    
  }
  
  if(SummaryVar == "SoilStability" & SummarizeBy == "EcologicalSite"){
    
    table <-  EcoSitePlots_Attributed %>% dplyr::select(PlotID , PrimaryKey , 
                                                        SoilStability_All , 
                                                        SoilStability_Protected , 
                                                        SoilStability_Unprotected, EcologicalSiteId, Time_Period) %>%
      gather(key = Veg , value = Rating , 
             SoilStability_All:SoilStability_Unprotected) %>%
      filter(!is.na(Rating)) %>% 
      mutate_if(is.numeric, round, digits = 2)  %>% 
      group_by(Veg, EcologicalSiteId, Time_Period) %>% 
      summarize(AverageSoilStability = mean(Rating , na.rm = TRUE) ,
                StandardDeviation = sd(Rating , na.rm = TRUE) ,
                MinSoilStability = min(Rating , na.rm = TRUE) ,
                MaxSoilStability = max(Rating, na.rm = TRUE)) %>%
      mutate_if(is.numeric, round , digits = 2) %>%
      DT::datatable(extensions = 'Buttons', filter = "top" , 
                    options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                     list(list(
                                       extend = 'collection',
                                       buttons = c('csv', 'excel'),
                                       text = 'Download Table'))) , 
                    caption = (paste("Average soil stability ratings in: " , 
                                     attribute_name)) , 
                    rownames = FALSE)
    
  }
  
  return(table)
  
}
# Attribute TDAT_LMF for comparison of two time periods
attribute_time <- function(TDAT_LMF, time_period1, time_period2){
  t1 <- cbind(Year = time_period1, Time_Period = rep("2011-2015", 5))
  t2 <- cbind(Year = time_period2,  Time_Period = rep("2016-2020", 5))
  time_period_table <- rbind(t1,t2)
  
  output <-  merge(x = TDat_LMF,
                   y = time_period_table,
                   by = "Year",
                   all.x = TRUE)
  return(output)
}


# Function to subset plots based on spatial join (from Attribute function)
SubsetPlots <- function(TDat_LMF, attribute_name){
  TDat_LMF_subset <- TDat_LMF[TDat_LMF[[attribute_title]] == paste0(attribute_name),]
}

SubsetEcologicalSite_Species <- function(EcoSitePlots, Species_Indicator){
  
  EcoSite_PKs <- EcoSitePlots$PrimaryKey
  Species_plots_ecosite <- Species_Indicator[(Species_Indicator[["PrimaryKey"]] %in% EcoSite_PKs), ]
  
  return(Species_plots_ecosite)
  
}
Table_RAP <-  function(filepath){
  
  # Read in csv from RAP trend estimates from csv 
  rap_estimates <- read.csv(filepath)%>%
    dplyr::rename(c("Annual Forb & Grass Cover"= "AFGC","Perennial Forb & Grass Cover" ="PFGC", "Shrub Cover"="SHR", "Tree Cover"="TREE", "Annual Temperature (°F)"="annualTemp", "Annual Precipitation (in)"="annualPrecip", "Bare Ground" = "BG"))
  
  t <- rap_estimates %>% DT::datatable(escape = FALSE, 
                                       extensions = 'Buttons', 
                                       filter = "top" , 
                                       options = list(scrollX = TRUE ,
                                                      dom = 'Bfrtip',
                                                      buttons =
                                                        list(list(
                                                          extend = 'collection',
                                                          buttons = c('csv', 'excel'),
                                                          text = 'Download Table'))) , 
                                       caption = (paste("RAP Indicator Estimates between 1984 to 2020 in " , attribute_name)) , 
                                       rownames = FALSE)
  
  return(t)
}

Plot_RAP <-  function(filepath){
  
  # Read in csv from RAP trend estimates from csv 
  rap_estimates <- read.csv(filepath)%>%
    dplyr::rename(c("Annual Forb & Grass Cover"= "AFGC","Perennial Forb & Grass Cover" ="PFGC", "Shrub Cover"="SHR", "Tree Cover"="TREE", "Annual Temperature (°F)"="annualTemp", "Annual Precipitation (in)"="annualPrecip", "Bare Ground" = "BG"))%>%
    gather(key = "Indicator", value = "value",
           2:8)
  
  estimates <- rap_estimates[rap_estimates$Indicator != "Annual Temperature (°F)" & rap_estimates$Indicator != "Annual Precipitation (in)",] 
  
  covariates <- rap_estimates[rap_estimates$Indicator == "Annual Temperature (°F)" | rap_estimates$Indicator == "Annual Precipitation (in)",]
  
  p <- ggplot(mapping = aes(x = year, y= value)) +
    geom_col(data = covariates, aes(col = Indicator, fill = Indicator), position = "dodge")+
    geom_line(data = estimates, aes(col = Indicator, fill = Indicator), size = 1.5)+
    theme_minimal(base_size = 16)+
    scale_color_brewer(type = "qual") +
    #scale_fill_manual(values = c("darkorange1", "dodgerblue1"))+
    guides(fill = FALSE)+
    #       fill = guide_legend(order = 2))+
    labs(y = "Cover(%)", x = NULL)+
    theme(legend.position = "bottom", legend.title = element_blank())
  
  return(p)
}
SummaryFigures_WithAttributes_alt <- function(SpeciesList, Species_plots_ecosite, EcologicalSite, 
                                              SummaryVar, Interactive, Attributed_Pks, EcoSitePlots, alpha = 0.2){
  
  group_palette <- c("2011-2015" = "#E69F00", "2016-2020" = "#56B4E9")
  #Prep
  SpeciesList <- SpeciesList %>% dplyr::select(Species, ScientificName, CommonName,
                                               Family, SpeciesState,
                                               SynonymOf, UpdatedSpeciesCode) %>% 
    dplyr::mutate(link = paste("https://plants.sc.egov.usda.gov/core/profile?symbol=", Species, sep = ""))
  
  #Merge with species list so we can hover for scientific name
  Species_plots_ecosite_attributed <- merge(Species_plots_ecosite, Attributed_Pks, by = "PrimaryKey", all = TRUE) %>% 
    unique() 
  
  # the input tdat_lmf attributed already has atribute heres, no need to merge
  EcoSitePlots_Attributed <- EcoSitePlots[EcoSitePlots$EcologicalSiteId == EcologicalSite,]
  
  # Added in attribute title here instead of allotment name
  Species_plots_ecosite_attributed <- sp::merge(Species_plots_ecosite_attributed , SpeciesList , by = c("Species" , "SpeciesState")) %>% 
    dplyr::select(Species, ScientificName, CommonName, PrimaryKey, 
                  PlotID,  AH_SpeciesCover, 
                  AH_SpeciesCover_n, Hgt_Species_Avg, 
                  Hgt_Species_Avg_n, GrowthHabit, GrowthHabitSub, Duration, 
                  Noxious, SG_Group, link, !!attribute_title, Time_Period, EcologicalSiteId) %>%
    dplyr::mutate_if(is.numeric, round , digits = 2) 
  
  # Get Noxious versus Non in Standard Format
  
  Species_plots_ecosite_attributed$Noxious <- gsub("YES" , "Yes", Species_plots_ecosite_attributed$Noxious)
  Species_plots_ecosite_attributed$Noxious <- gsub("NO", "No", Species_plots_ecosite_attributed$Noxious)
  
  #Ignoring NAs - make disclosure as this may overestimate cover
  #For summarizing across all plots
  
  # I THINK WE WANT TO ADD N AND CONF INT HERE TO REPLACE BOCPLOTS WITH POINTS AND ERROR BARS
  Species_cover_summary <- Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover)) %>% 
    mutate(Tally = 1) %>%
    group_by(Species, GrowthHabit , GrowthHabitSub , 
             Duration , Noxious , ScientificName , 
             CommonName , SG_Group, link, EcologicalSiteId, Time_Period) %>% 
    summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
              StandardDeviation = sd(AH_SpeciesCover),
              MinCover = min(AH_SpeciesCover) ,
              MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
    mutate_if(is.numeric, round , digits = 2)%>%
    dplyr::select(Species, ScientificName, 
                  AveragePercentCover, StandardDeviation,
                  MinCover, MaxCover, n, GrowthHabit, 
                  GrowthHabitSub, Duration, 
                  Noxious, CommonName, SG_Group, link, EcologicalSiteId, Time_Period)
  
  NoxNonPal_Fill <- c("grey75"  , "#D55E00")
  NoxNonPal_Dot <- c("grey33" , "#993300")
  ## Setting color for attribute title
  ## FIgure out how to not hardcode ALLOT_NAME and instead use attribute_title - DONE 
  Attribute_Fill <- scales::seq_gradient_pal("#009966", "#E69F00", "Lab")(seq(0,1, length.out = length(unique(Species_plots_ecosite_attributed[[attribute_title]]))))
  
  #Remove NAs for plotting
  Species_plots_ecosite_attributed <- Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover), EcologicalSiteId==EcologicalSite)
  
  dodge1 <- position_dodge(width = 0.9)
  dodge2 <- position_dodge(width = 0.4)
  
  if(SummaryVar == "GrowthHabitSub"){
    if(Interactive){
      
      
      Plots <-  lapply(X = split(Species_plots_ecosite_attributed, Species_plots_ecosite_attributed[["GrowthHabitSub"]] , 
                                 drop = TRUE),
                       
                       FUN = function(Species_plots_ecosite_attributed){
                         
                         current_plot <- ggplot2::ggplot(Species_plots_ecosite_attributed[!is.na(Species_plots_ecosite_attributed$GrowthHabitSub)&Species_plots_ecosite_attributed$EcologicalSiteId == EcologicalSite,], 
                                                         aes(x = GrowthHabitSub, 
                                                             y = AH_SpeciesCover, 
                                                             text = paste("Primary Key: " , PrimaryKey , 
                                                                          "Plot ID: " , PlotID , "Species: " , 
                                                                          ScientificName , "Code: " , Species , 
                                                                          "Percent Cover: " , AH_SpeciesCover , 
                                                                          "Noxious: " , Noxious ,
                                                                          sep = "<br>",
                                                                          "Time Period: ", Time_Period),
                                                             fill = Time_Period)) +
                           geom_boxplot(width = 1, outlier.shape = NA) +
                           theme_light() + # remove ylims here
                           theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                 axis.title.y = element_blank() , axis.title.x = element_blank() ,  
                                 axis.line.y = element_blank()) + theme(panel.grid.major.y = element_blank() ,
                                                                        axis.title.y = element_blank()) +
                           ggtitle(paste0("Percent Cover by Functional Group: " , 
                                          Species_plots_ecosite_attributed$GrowthHabitSub, ", Ecological Site: ", EcologicalSite)) +
                           coord_flip() + 
                           scale_fill_manual(values = group_palette, drop = FALSE) +
                           facet_grid(cols = vars(GrowthHabitSub) ,
                                      rows = vars(Duration) ,
                                      switch = "y" ,
                                      scales = "free" , drop = TRUE)
                         
                         return(current_plot)
                       }
      )
    }
    
    if(!Interactive){
      Plots <- lapply(X = split(Species_plots_ecosite_attributed, Species_plots_ecosite_attributed[["GrowthHabitSub"]] , drop = TRUE), 
                      FUN = function(Species_plots_ecosite_attributed){
                        
                        current_plot <- ggplot2::ggplot(Species_plots_ecosite_attributed[Species_plots_ecosite_attributed$EcologicalSiteId == EcologicalSite,], aes(x = GrowthHabitSub , y = AH_SpeciesCover, fill = Time_Period)) +
                          geom_boxplot(width = .6 , outlier.shape = NA, position = dodge1) +
                          geom_point(size = 2, aes(color = Time_Period, shape = Noxious), position = dodge1) +
                          labs(y = "Percent Cover") + # remove ylims
                          theme_light() + 
                          scale_fill_manual(values = group_palette, drop = FALSE) +
                          theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                axis.line.y = element_blank()) + theme(panel.grid.major.y = element_blank() ,
                                                                       axis.title.y = element_blank()) +
                          ggtitle(paste("Percent Cover by Functional Group:", 
                                        Species_plots_ecosite_attributed$GrowthHabitSub, ", Ecological Site:", EcologicalSite, sep = " "
                          )) +
                          coord_flip() + facet_grid(cols = vars(GrowthHabitSub) ,
                                                    rows = vars(Duration) , switch = "y" ,
                                                    scales = "free" , drop = TRUE)
                        
                        
                        return(current_plot)
                      })
    }
  }
  
  if(SummaryVar == "Noxious"){
    if(Interactive){
      Plots <- Species_plots_ecosite_attributed %>% group_by(Noxious, Time_Period) %>% 
        filter(!is.na(Noxious)) %>% filter(!is.na(AH_SpeciesCover)) %>%
        ggplot2::ggplot((aes(x = Noxious,
                             y = AH_SpeciesCover,
                             text = paste("Primary Key : " , PrimaryKey, 
                                          "Plot ID: " , PlotID,
                                          "Species: " ,  ScientificName, 
                                          "Code: " , Species, 
                                          "Percent Cover: " , AH_SpeciesCover, 
                                          "Noxious: " , Noxious, 
                                          "Time Period: ", Time_Period, 
                                          sep = "<br>"),
                             fill = Time_Period))) +
        geom_boxplot(width = .6 , outlier.shape = NA) +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        theme_light() +
        scale_y_continuous(limits = c(0 , 100)) +
        labs(y = "Percent Cover") + 
        ggtitle(paste("Percent Cover, Noxious vs. Non-Noxious Species: " , 
                      toString(EcologicalSite))) +
        theme(axis.title.y = element_blank() , axis.text.y = element_blank() ,
              axis.ticks.y = element_blank() , axis.line.y = element_blank() , 
              axis.title.x = element_blank()) +
        theme(panel.grid.major.y = element_blank() , legend.position = "none") +
        coord_flip() + 
        facet_grid(rows = vars(Noxious) , switch = "y" , scales = "free" , 
                   drop = TRUE) 
      return(Plots)
    }
    
    if(!Interactive){
      Plots <- Species_plots_ecosite_attributed %>% group_by(Noxious, Time_Period) %>% 
        filter(!is.na(Noxious)) %>% filter(!is.na(AH_SpeciesCover)) %>%
        ggplot2::ggplot((aes(x = Noxious , y = AH_SpeciesCover, fill = Time_Period))) +
        geom_boxplot(width = .6 , outlier.shape = NA, position = dodge1) +
        geom_point(size = 2 , aes(shape = Noxious, col = Time_Period), position = dodge1) +
        theme_light() +
        scale_y_continuous(limits = c(0 , 100)) +
        labs(y = "Percent Cover") + 
        ggtitle(paste("Percent Cover, Noxious vs. Non-Noxious Species: " , 
                      toString(EcologicalSite))) +
        theme(axis.title.y = element_blank() , axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank() , 
              panel.grid.major.y = element_blank()) +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        coord_flip() + 
        facet_grid(rows = vars(Noxious) ,
                   switch = "y" , scales = "free" , drop = TRUE)
      
    }}
  
  if(SummaryVar == "Species"){
    PercentCover <- Species_plots_ecosite_attributed %>% subset(AH_SpeciesCover > 0.000000)
    # lets find max species cover here to adjust y axis
    maxcover <- max(PercentCover$AH_SpeciesCover)
    
    if(Interactive){
      Plots <-lapply(X = split(PercentCover, list(PercentCover$GrowthHabitSub , PercentCover$Duration) , drop = TRUE),
                     FUN = function(PercentCover){
                       current_plot <- ggplot2::ggplot(PercentCover , aes(x = Species , y = AH_SpeciesCover,
                                                                          text = paste("PrimaryKey: ", PrimaryKey , 
                                                                                       "Plot ID: " , PlotID , "Species: " , 
                                                                                       ScientificName , "Code: " , Species , 
                                                                                       "Percent Cover: " , AH_SpeciesCover , "Noxious: " , 
                                                                                       Noxious , 
                                                                                       "Time Period: ", Time_Period, 
                                                                                       sep = "<br>"),
                                                                          fill = Time_Period)) +
                         geom_boxplot(outlier.shape = NA) +
                         theme_light() +
                         scale_fill_manual(values = group_palette, drop = FALSE) +
                         labs(y = "Percent Cover") + 
                         ggtitle(paste("Percent Cover by Species, " , 
                                       PercentCover$GrowthHabitSub, 
                                       PercentCover$Duration ,
                                       EcologicalSite)) + # removed tostring function
                         theme(axis.title.y = element_blank() ,
                               axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(), 
                               axis.title.x = element_blank())+   
                         theme(panel.grid.major.y = element_blank() ,
                               axis.title.y = element_blank()) +
                         coord_flip() +  facet_grid(rows = vars(Species),
                                                    scales = "free" , switch = "y",  drop = TRUE) 
                       return(current_plot)
                     })
    }
    
    if(!Interactive){
      Plots <- lapply(X = split(PercentCover, list(PercentCover$GrowthHabitSub , 
                                                   PercentCover$Duration) , 
                                drop = TRUE),
                      FUN = function(PercentCover){
                        current_plot <- ggplot2::ggplot(PercentCover , aes(x = Species , y = AH_SpeciesCover, fill = Time_Period)) +
                          geom_boxplot(width = .6 , outlier.shape = NA, position = dodge1) +
                          geom_point(size = 1 , aes(shape = Noxious), position = dodge1) +
                          theme_light() +
                          scale_fill_manual(values = group_palette, drop = FALSE) +
                          labs(y = "Percent Cover") + 
                          ggtitle(paste("Percent Cover by Species, " , 
                                        PercentCover$GrowthHabitSub , 
                                        PercentCover$Duration, 
                                        EcologicalSite, sep = ",")) + 
                          theme(axis.title.y = element_blank()) +
                          coord_flip() +  facet_grid(cols = vars(GrowthHabitSub) , rows = vars(Duration) ,
                                                     switch = "y" , scales = "free" , drop = TRUE) 
                        return(current_plot)
                      })
      
    }
  }
  
  if(SummaryVar == "GroundCover"){
    
    #Prep
    #BareSoilCover
    #TotalFoliarCover
    #FH_TotalLitterCover
    #FH_RockCover
    
    Ground_Cover_Tall <- EcoSitePlots_Attributed %>% 
      dplyr::select(PlotID, PrimaryKey, BareSoilCover , 
                    TotalFoliarCover , FH_TotalLitterCover , 
                    FH_RockCover, !!attribute_title, Time_Period) %>%
      gather(key = Indicator , value = Percent, 
             BareSoilCover:FH_RockCover) %>% mutate(Tally = 1) 
    if(Interactive){
      
      
      Plots <- Ground_Cover_Tall %>% mutate_if(is.numeric , round , digits = 2) %>% 
        ggplot2::ggplot((aes(x = Indicator , y = Percent , 
                             text = paste("PlotID: " , PlotID, 
                                          "PrimaryKey: " , PrimaryKey , 
                                          "Indicator: " , Indicator ,
                                          "Percent Cover: " , Percent , 
                                          "Time Period: ", Time_Period, 
                                          sep = "<br>" ),
                             fill = Time_Period))) +
        geom_boxplot(width = .6 , outlier.shape = NA) +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        theme_light() +
        scale_y_continuous(limits = c(0 , 100)) +
        labs(y = "Ground Cover (%)" , x = "Indicator") +
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank() ,  
              axis.title.x = element_blank() , 
              axis.title.y = element_blank()) +
        coord_flip() + facet_grid(rows = vars(Indicator) ,
                                  switch = "y" ,
                                  scales = "free_y" , drop = TRUE)
      
    }
    
    if(!Interactive){
      Plots <- Ground_Cover_Tall %>% mutate_if(is.numeric , round , digits = 2) %>% 
        ggplot2::ggplot((aes(x = Indicator , y = Percent, fill = Time_Period))) +
        geom_boxplot(width = .6 , outlier.shape = NA, position = dodge1) +
        geom_point(position = dodge1) +
        theme_light(base_size = 16) +
        scale_color_manual(values = Attribute_Fill, na.value="#000000") +
        scale_y_continuous(limits = c(0 , 100)) +
        labs(y = "Ground Cover (%)" , x = "Indicator") +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank()) + 
        coord_flip() + facet_grid(rows = vars(Indicator) ,
                                  switch = "y" ,
                                  scales = "free_y" , drop = TRUE)
      
      
    }
    
    
  }
  
  if(SummaryVar == "Gap"){
    
    Gap <- EcoSitePlots_Attributed %>% dplyr::select(PlotID, PrimaryKey , 
                                                     GapCover_25_50 , GapCover_51_100 , 
                                                     GapCover_101_200 , GapCover_200_plus , 
                                                     GapCover_25_plus, Time_Period) %>% 
      gather(key = Gap_Class_cm , 
             value = Percent , GapCover_25_50:GapCover_25_plus) %>%
      mutate_if(is.numeric , round, digits = 2)
    
    # reorder factors by size
    Gap$Gap_Class_cm <- as.factor(Gap$Gap_Class_cm)
    Gap$Gap_Class_cm <-  factor(Gap$Gap_Class_cm, levels = c("GapCover_200_plus","GapCover_101_200" , "GapCover_51_100", "GapCover_25_50", 'GapCover_25_plus'))
    Gap <- Gap[order(Gap$Gap_Class_cm),]
    
    #Plot prep
    if(Interactive){
      
      Plots <- ggplot2::ggplot(data = Gap , aes(x = Gap_Class_cm , y = Percent , 
                                                text = paste("PlotID: " , PlotID , 
                                                             "PrimaryKey: ", PrimaryKey , 
                                                             "Gap Class (cm): " , Gap_Class_cm, 
                                                             "Percent Cover: " , Percent , 
                                                             "Time Period: ", Time_Period, 
                                                             sep = "<br>"),
                                                fill = Time_Period)) +
        geom_boxplot() + coord_flip() + 
        theme_light() + 
        theme(axis.title.x = element_blank() ,
              axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() , 
              axis.title.y = element_blank() , 
              axis.line.y = element_blank(), 
              panel.grid.major.y = element_blank()) +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        facet_grid(rows = vars(Gap_Class_cm) , switch = "y" ,
                   scales = "free_y" , drop = TRUE)
      
    }
    
    if(!Interactive){
      Plots <- ggplot2::ggplot(data = Gap , aes(x = Gap_Class_cm , y = Percent, fill = Time_Period)) +
        labs(y = "Percent Cover" , x = "Gap Size Class (cm)", 
             caption = paste("Percent cover of canopy gap in: ", 
                             EcologicalSite, sep = " ")) +
        geom_boxplot(position = dodge1) + 
        coord_flip() + 
        geom_point(position = dodge1) +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        theme_light(base_size = 16) + 
        scale_color_manual(values = Attribute_Fill, na.value="#000000") + 
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank(),
              panel.grid.major.y = element_blank()) +
        facet_grid(rows = vars(Gap_Class_cm) , switch = "y" ,
                   scales = "free_y" , drop = TRUE)
    }
    
  }
  
  if(SummaryVar == "SoilStability"){
    
    soil_labels <- c("SoilStability_All" = "All" , "SoilStability_Protected" = "Protected" , 
                     "SoilStability_Unprotected" = "Unprotected")
    
    SoilStability <- EcoSitePlots_Attributed %>% dplyr::select(PlotID , PrimaryKey , 
                                                               SoilStability_All , 
                                                               SoilStability_Protected , 
                                                               SoilStability_Unprotected,
                                                               Time_Period) %>%
      gather(key = Veg , value = Rating , 
             SoilStability_All:SoilStability_Unprotected) %>%
      mutate_if(is.numeric, round, digits = 2) %>% dplyr::filter(!is.na(Rating))
    
    if(Interactive){
      
      Plots <- ggplot2::ggplot(data = SoilStability , 
                               aes(x = Veg , y = Rating , fill = Time_Period, 
                                   text = paste("Primary Key: " , PrimaryKey,
                                                "Plot ID: " , PlotID , 
                                                "Rating: " , Rating , 
                                                "Time Period: ", Time_Period, 
                                                sep = "<br>"))) +
        geom_boxplot(position = dodge1) + 
        scale_fill_manual(values = group_palette, drop = FALSE) +
        coord_flip() +
        theme_light() + 
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.title = element_blank()) +
        facet_grid(rows = vars(Veg) , switch = "y" ,
                   scales = "free_y" , drop = TRUE , 
                   labeller = as_labeller(soil_labels))
    }
    
    
    if(!Interactive){
      Plots <- ggplot2::ggplot(data = SoilStability, aes(x = Veg , y = Rating, fill = Time_Period)) +
        labs(x = "Vegetation cover class", 
             y = "Soil Stability Rating",
             caption = paste("Soil stability ratings in: ", 
                             EcologicalSite)) +
        geom_boxplot(position = dodge1) +
        coord_flip() + 
        geom_point(position = dodge1) +
        theme_light(base_size = 16) +
        scale_fill_manual(values = group_palette, drop = FALSE) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.y = element_blank(),
              panel.grid.major.y = element_blank()) +
        facet_grid(rows = vars(Veg),
                   switch = "y",
                   scales = "free_y", 
                   drop = TRUE,
                   labeller = as_labeller(soil_labels))
      
    }
    
  }
  
  return(Plots)
  
}

Time_Period_Figures <- function(EcologicalSite, SummaryVar, Interactive, TDat_LMF_Attributed, Species_plots_ecosite, SpeciesList, alpha = 0.2)# TDat_Cover_Indicators is a list of indicators to summarise by
{
  
  TDat_Cover_Indicators <-  c("AH_NoxAnnForbCover","AH_NoxAnnGrassCover","AH_NoxCover","AH_NonNoxAnnForbCover", "AH_PerenGrassCover","AH_PerenForbCover","AH_TallPerenGrassCover","AH_ShortPerenGrassCover")
  TDat_Cover_Indicators <-  enquo(TDat_Cover_Indicators)
  
  ####################################### function for CIs #########################
  ci_model <-  function(data, y_var, x_var){
    
    if(any(grepl("SoilStability", data$Indicator))){ # using any() here since we split() the data by indicator below for modeling
      
      # Converting soil stability data to proportion for glm() call
      ss_fun <- function(x){
        y <- (x-1)/5
        return(y)
      }
      
      data[[y_var]] <- ss_fun(data[[y_var]])
      
      if(length(unique(data[[x_var]]))< 2) {
        object <- glm(data = data,
                      family = "quasibinomial",
                      formula = data[[y_var]] ~ 1)
        object$xlevels <- unique(data[[x_var]])
        return(object)
      }else{
        object <- glm(data = data,
                      family = "quasibinomial",
                      formula = data[[y_var]] ~ data[[x_var]])
        object$xlevels <- unique(data[[x_var]])
        return(object)
      }
      
    } else {
      if(length(unique(data[[x_var]]))< 2){
        object <- glm(data = data,
                      family = "quasibinomial",
                      formula = data[[y_var]]/100 ~ 1)
        object$xlevels <- unique(data[[x_var]])# this prevent errors when there aren't plots in both x var groups
        return(object)
      }else{
        object <- glm(data = data,
                      family = "quasibinomial",
                      formula = data[[y_var]]/100 ~ data[[x_var]])
        object$xlevels <-unique(data[[x_var]])
        return(object)
      }
    }
  }
  
  # logit transform for proportions
  logit_to_real_cis <- function(object,level=0.8) {
    
    sum.obj <- summary(object)
    z.score <- c(qnorm((1-level)/2),qnorm(1-(1-level)/2))
    
    
    logit.means <- c(sum.obj$coefficients[1,1],sum.obj$coefficients[1,1]+sum.obj$coefficients[-1,1])
    real.means <- plogis(logit.means)
    
    n.groups <- dim(sum.obj$coefficients)[1]
    
    out.df <- data.frame(Group=object$xlevels, Mean=real.means, LCI=NA, UCI=NA)  # changed from coefficients to xlevels to get real x names instead of "intercept"
    
    logit.var <- numeric(n.groups)
    for(i in 1:n.groups) {
      if(i==1) {logit.var[i] <- vcov(object)[i,i]}
      if(i>1) {
        vc <- vcov(object)[c(1,i),c(1,i)]
        logit.var[i] <- matrix(c(1,1),nrow=1,ncol=2) %*% vc %*% matrix(c(1,1),nrow=2,ncol=1)
      }
      out.df[i,c('LCI','UCI')] <- plogis(logit.means[i]+z.score*sqrt(logit.var[i]))
    }
    
    return(out.df)
  }
  
  # log transforming is for heights
  log_to_real_cis <- function(object,level=0.95) {
    
    sum.obj <- summary(object)
    z.score <- c(qnorm((1-level)/2),qnorm(1-(1-level)/2))
    
    log.means <- c(sum.obj$coefficients[1,1],sum.obj$coefficients[1,1]+sum.obj$coefficients[-1,1])
    real.means <- exp(log.means)
    
    n.groups <- dim(sum.obj$coefficients)[1]
    
    out.df <- data.frame(Group=rownames(object$xlevels),Mean=real.means,LCI=NA,UCI=NA) # changed from coeficients to xlevels to get real x names instead of "intercept"
    
    log.var <- numeric(n.groups)
    for(i in 1:n.groups) {
      if(i==1) {log.var[i] <- vcov(object)[i,i]}
      if(i>1) {
        vc <- vcov(object)[c(1,i),c(1,i)]
        logit.var[i] <- matrix(c(1,1),nrow=1,ncol=2) %*% vc %*% matrix(c(1,1),nrow=2,ncol=1)
      }
      out.df[i,c('LCI','UCI')] <- exp(log.means[i]+z.score*sqrt(log.var[i]))
    }
    
    return(out.df)
  }
  
  ################################################ DATA PREP ##################################################
  SpeciesList <- SpeciesList %>% dplyr::select(Species, ScientificName, CommonName,
                                               Family, SpeciesState,
                                               SynonymOf, UpdatedSpeciesCode) %>% 
    dplyr::mutate(link = paste("https://plants.sc.egov.usda.gov/core/profile?symbol=", Species, sep = ""))
  
  # Merge with species list so we can hover for scientific name
  Species_plots_ecosite_attributed <- merge(Species_plots_ecosite, Attributed_Pks, by = "PrimaryKey", all = TRUE) %>% 
    unique() 
  
  # Filter input datasetc to include only relevant plots within the ecosite of interest
  TDat_LMF_Attributed <- TDat_LMF_Attributed[TDat_LMF_Attributed$EcologicalSiteId == EcologicalSite,] # dataset for ground cover and growth habit
  Species_plots_ecosite_attributed <- Species_plots_ecosite_attributed[Species_plots_ecosite_attributed$EcologicalSiteId == EcologicalSite,] # species dataset for species level indicators
  
  # Remove geometry if it exists
  if (any(class(TDat_LMF_Attributed) == "sf")){
    TDat_LMF_Attributed <- st_drop_geometry(TDat_LMF_Attributed)
  }
  
  # Added in attribute title here instead of allotment name
  Species_plots_ecosite_attributed <- merge(Species_plots_ecosite_attributed , SpeciesList , by = c("Species" , "SpeciesState")) %>% 
    dplyr::select(Species, ScientificName, CommonName, PrimaryKey, 
                  PlotID,  AH_SpeciesCover, 
                  AH_SpeciesCover_n, Hgt_Species_Avg, 
                  Hgt_Species_Avg_n, GrowthHabit, GrowthHabitSub, Duration, 
                  Noxious, SG_Group, link, !!attribute_title, Time_Period, EcologicalSiteId) %>%
    dplyr::mutate_if(is.numeric, round , digits = 2) 
  
  # Get Noxious versus Non in Standard Format
  Species_plots_ecosite_attributed$Noxious <- gsub("YES" , "Yes", Species_plots_ecosite_attributed$Noxious)
  Species_plots_ecosite_attributed$Noxious <- gsub("NO", "No", Species_plots_ecosite_attributed$Noxious)
  
  # Summarizing Tdat to get sample stats across ecological site
  TDat_long <- TDat_LMF_Attributed %>% dplyr::select(PlotID, PrimaryKey, BareSoilCover , 
                                                     TotalFoliarCover , FH_TotalLitterCover , 
                                                     FH_RockCover, !!TDat_Cover_Indicators, SoilStability_All , 
                                                     SoilStability_Protected , 
                                                     SoilStability_Unprotected, GapCover_25_50 , GapCover_51_100 , 
                                                     GapCover_101_200 , GapCover_200_plus, 
                                                     GapCover_25_plus, !!attribute_title, Time_Period, EcologicalSiteId) %>% # removing un-needed cols
    gather(key = Indicator , value = Percent, BareSoilCover:GapCover_25_plus)%>% # MAKING LONG
    filter(!is.na(Percent))
  
  TDat_cis_list <- lapply(X = split(TDat_long, TDat_long[["Indicator"]]), FUN = function(TDat_long){
    mod <- ci_model(data = TDat_long, y_var = "Percent", x_var = "Time_Period")
    cis_df <- logit_to_real_cis(mod)
    return(cis_df)
  })
  
  # organising these outputs for plotting
  
  plot_org <- function(ci_list){
    # pull list into data frame
    TDat_cis_df <- do.call("rbind",(ci_list))
    # make colnmaes prettier
    TDat_cis_df$Indicator <- row.names(TDat_cis_df)
    TDat_cis_df$Indicator <-  gsub("\\.[0-9]","",TDat_cis_df$Indicator)
    TDat_cis_df$Group <- gsub("data\\[\\[x_var\\]\\]", "",TDat_cis_df$Group)
    
    # this replaces intercept with the appropriate group name
    if(any(unique(TDat_cis_df$Group)== "2016-2020")){
      TDat_cis_df$Group <- gsub("\\(Intercept\\)", "2011-2015",TDat_cis_df$Group)
    } else if(EcologicalSite == "Clayey Foothills"){
      TDat_cis_df$Group <- gsub("\\(Intercept\\)", "2011-2015",TDat_cis_df$Group)
    } else {
      TDat_cis_df$Group <- gsub("\\(Intercept\\)", "2016-2020",TDat_cis_df$Group)
    }
    ## THIS IS STILL GIVING ERRORS FOR BRUSHY LOAM
    
    # function to convert proportion back to percent
    times100 <- function(x){x * 100}
    # function to convert back to ss ratings scale of 1-6
    nuf_ss <- function(x){(x*5)+1}
    
    TDat_cis_df[!grepl("SoilStability", TDat_cis_df$Indicator),] <- TDat_cis_df[!grepl("SoilStability", TDat_cis_df$Indicator),] %>% mutate_if(is.numeric, times100)
    TDat_cis_df[grepl("SoilStability", TDat_cis_df$Indicator),] <-  TDat_cis_df[grepl("SoilStability", TDat_cis_df$Indicator),] %>% mutate_if(is.numeric, nuf_ss)
    TDat_summary <- TDat_cis_df %>% mutate_if(is.numeric, round , digits = 2)
    TDat_summary$EcologicalSite <- EcologicalSite
    return(TDat_summary)
  }
  
  TDat_summary <- plot_org(TDat_cis_list)  
  
  ### Summarize species level data (at ecosite level only)
  Species_summary <- Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover))
  
  species_cis_list <- lapply(X = split(Species_summary, Species_summary[["Species"]]), FUN = function(Species_summary){
    
    mod <- ci_model(data = Species_summary, y_var = "AH_SpeciesCover", x_var = "Time_Period")
    cis_df <- logit_to_real_cis(mod)
    return(cis_df)
  })
  
  species_cis_list <- plot_org(species_cis_list)
  
  Species_summary <- merge(x = species_cis_list,
                           y = Species_summary[,c("GrowthHabitSub", "Duration", "Species","CommonName","ScientificName", "Noxious")],
                           by.x = "Indicator",
                           by.y = "Species",
                           all.y = FALSE)
  
  
  ############################################## ADDITIONAL VARIABLES ##############################################
  
  NoxNonPal_Fill <- c("grey75"  , "#D55E00")
  NoxNonPal_Dot <- c("grey33" , "#993300")
  
  group_palette <- c("2011-2015" = "#E69F00", "2016-2020" = "#56B4E9")
  
  
  ## Setting color for attribute title
  Attribute_Fill <- scales::seq_gradient_pal("#009966", "#E69F00", "Lab")(seq(0,1, length.out = length(unique(Species_plots_ecosite_attributed[[attribute_title]]))))
  dodge1 <- position_dodge(width = 0.9)
  
  #################################################### PLOTTING ##################################################
  
  if(SummaryVar == "GrowthHabitSub"){
    
    GH_Species_summary <- TDat_summary %>%
      filter(Indicator == "AH_NonNoxAnnForbCover" |Indicator == "AH_PerenForbCover"| Indicator =="AH_PerenGrassCover"| Indicator == "AH_ShortPerenGrassCover"|Indicator =="AH_TallPerenGrassCover")
    
    if(Interactive){
      
      Plots <-  lapply(X = split(GH_Species_summary, GH_Species_summary[["Indicator"]] , 
                                 drop = TRUE),
                       
                       FUN = function(GH_Species_summary){
                         
                         current_plot <- ggplot2::ggplot(GH_Species_summary[!is.na(GH_Species_summary$Indicator),], 
                                                         aes(x = Indicator, 
                                                             y = Mean, 
                                                             text = paste("Percent Cover: " , Mean,
                                                                          "Ecological Site: ", EcologicalSite,
                                                                          "Time Period: ", Group,
                                                                          "Upper Condfidence Interval: ", UCI,
                                                                          "Lower Condfidence Interval: ", LCI,
                                                                          sep = "<br>"),
                                                             fill = Group)) +
                           
                           geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
                           geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
                           theme_light() + 
                           theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                 axis.title.y = element_blank() , axis.title.x = element_blank() ,  
                                 axis.line.y = element_blank()) +
                           scale_fill_manual(values = group_palette, drop = FALSE) +
                           theme(panel.grid.major.y = element_blank(), axis.title.y = element_blank()) +
                           ggtitle(paste0("Percent Cover by Functional Group: " , 
                                          GH_Species_summary$Indicator, ", Ecological Site: ", EcologicalSite)) + # this is throwing errors 
                           coord_flip()
                         
                         return(current_plot)
                       }
      )
    }
    
    if(!Interactive){
      
      Plots <- lapply(X = split(GH_Species_summary, GH_Species_summary[["Indicator"]] , drop = TRUE), 
                      FUN = function(GH_Species_summary){
                        
                        current_plot <- ggplot2::ggplot(GH_Species_summary[!is.na(GH_Species_summary$Indicator),],
                                                        aes(x = Indicator ,
                                                            y = Mean,
                                                            fill = Group)) +
                          geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
                          geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
                          labs(y = "Percent Cover") +
                          theme_light() + 
                          scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                                            breaks = c("2011-2015", "2016-2020"), drop = FALSE)+
                          theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                axis.line.y = element_blank()) +
                          theme(panel.grid.major.y = element_blank(),axis.title.y = element_blank()) +
                          ggtitle(paste("Percent Cover by Functional Group:", 
                                        GH_Species_summary$Indicator, ", Ecological Site:", EcologicalSite)) +
                          coord_flip()
                        
                        
                        return(current_plot)
                      })
    }
  }
  
  if(SummaryVar == "Noxious"){
    
    Noxious_summary <-  TDat_summary %>% filter(Indicator == "AH_NoxAnnForbCover" | Indicator == "AH_NoxAnnGrassCover" | Indicator == "AH_NoxCover")
    
    if(Interactive){
      Plots <- ggplot2::ggplot(Noxious_summary,(aes(x = Indicator,
                                                    y = Mean,
                                                    text = paste("Percent Cover: " , Mean, 
                                                                 "Noxious Indicator: " , Indicator, 
                                                                 "Time Period: ", Group, 
                                                                 sep = "<br>"),
                                                    fill = Group))) +
        geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        theme_light() +
        labs(y = "Percent Cover") + 
        ggtitle(paste("Percent Cover, Noxious vs. Non-Noxious Species: " , 
                      toString(EcologicalSite))) +
        theme(axis.title.y = element_blank() , axis.text.y = element_blank() ,
              axis.ticks.y = element_blank() , axis.line.y = element_blank() , 
              axis.title.x = element_blank()) +
        theme(panel.grid.major.y = element_blank() , legend.position = "none") +
        
        scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                          breaks = c("2011-2015", "2016-2020"), drop = FALSE)+
        coord_flip() + 
        facet_grid(rows = vars(Indicator) , switch = "y" , scales = "free" , 
                   drop = TRUE) 
      return(Plots)
    }
    
    if(!Interactive){
      Plots <- ggplot2::ggplot(Noxious_summary,(aes(x = Indicator , y = Mean, fill = Group))) +
        geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        theme_light() +
        labs(y = "Percent Cover") + 
        ggtitle(paste("Percent Cover, Noxious vs. Non-Noxious Species: " , 
                      toString(EcologicalSite))) +
        theme(axis.title.y = element_blank() , axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank() , 
              panel.grid.major.y = element_blank()) +
        scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                          breaks = c("2011-2015", "2016-2020"), drop = FALSE)+
        coord_flip() + 
        facet_grid(rows = vars(Indicator) ,
                   switch = "y" , scales = "free" , drop = TRUE)
      
    }}
  
  if(SummaryVar == "Species"){
    
    if(Interactive){
      Plots <-lapply(X = split(Species_summary, list(Species_summary$GrowthHabitSub , Species_summary$Duration) , drop = TRUE),
                     FUN = function(Species_summary){
                       current_plot <- ggplot2::ggplot(Species_summary , aes(x = Indicator , y = Mean,
                                                                             text = paste("Species: " , 
                                                                                          ScientificName , "Code: " , Indicator , "Common Name: ", CommonName,
                                                                                          "Percent Cover: " , Mean, "Noxious: " , 
                                                                                          Noxious , 
                                                                                          "Time Period: ", Group,
                                                                                          "Ecological Site: ", EcologicalSite,
                                                                                          sep = "<br>"),
                                                                             fill = Group)) +
                         geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
                         geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
                         theme_light() +
                         labs(y = "Percent Cover") + 
                         ggtitle(paste("Percent Cover by Species, " , 
                                       Species_summary$GrowthHabitSub, 
                                       Species_summary$Duration ,
                                       EcologicalSite)) + # removed tostring function
                         theme(axis.title.y = element_blank() ,
                               axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(), 
                               axis.title.x = element_blank())+   
                         theme(panel.grid.major.y = element_blank() ,
                               axis.title.y = element_blank()) +
                         scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                                           breaks = c("2011-2015", "2016-2020"), drop = FALSE)+
                         coord_flip() +  facet_grid(rows = vars(Indicator),
                                                    scales = "free" , switch = "y",  drop = TRUE) 
                       return(current_plot)
                     })
    }
    
    if(!Interactive){
      Plots <- lapply(X = split(Species_summary, list(Species_summary$GrowthHabitSub , 
                                                      Species_summary$Duration) , 
                                drop = TRUE),
                      FUN = function(Species_summary){
                        current_plot <- ggplot2::ggplot(Species_summary , aes(x = Indicator , y = Mean, fill = Group)) +
                          geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
                          geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
                          theme_light() +
                          labs(y = "Percent Cover") + 
                          scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                                            breaks = c("2011-2015", "2016-2020"), drop = FALSE)+
                          ggtitle(paste("Percent Cover by Species, " , 
                                        Species_summary$Duration, Species_summary$GrowthHabitSub , 
                                        EcologicalSite)) + 
                          theme(axis.title.y = element_blank()) +
                          coord_flip() +  facet_grid(rows = vars(Indicator) ,
                                                     switch = "y" , scales = "free" , drop = TRUE) 
                        return(current_plot)
                      })
      
    }
  }
  
  if(SummaryVar == "GroundCover"){
    
    GC_summary <- TDat_summary %>% filter(Indicator == "BareSoilCover" | Indicator == "FH_RockCover" | Indicator == "FH_TotalLitterCover" | Indicator == "TotalFoliarCover")
    
    if(Interactive){
      
      
      Plots <-  ggplot2::ggplot(GC_summary,(aes(x = Indicator , y = Mean , 
                                                text = paste("Indicator: " , Indicator ,
                                                             "Percent Cover: " , Mean , 
                                                             "Time Period: ", Group,
                                                             "Ecological Site: ", EcologicalSite,
                                                             sep = "<br>" ),
                                                fill = Group))) +
        geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        theme_light() +
        scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                          breaks = c("2011-2015", "2016-2020"), drop = FALSE)+
        labs(y = "Ground Cover (%)" , x = "Indicator") +
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank() ,  
              axis.title.x = element_blank() , 
              axis.title.y = element_blank()) +
        coord_flip() + facet_grid(rows = vars(Indicator) ,
                                  switch = "y" ,
                                  scales = "free_y" , drop = TRUE)
      
    }
    
    if(!Interactive){
      Plots <- ggplot2::ggplot(GC_summary,(aes(x = Indicator , y = Mean,
                                               fill = Group))) +
        geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        theme_light() +
        labs(y = "Ground Cover (%)" , x = "Indicator") +
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank() ,  
              axis.title.x = element_blank() , 
              axis.title.y = element_blank()) +
        scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                          breaks = c("2011-2015", "2016-2020"), drop = FALSE)+
        coord_flip() + facet_grid(rows = vars(Indicator) ,
                                  switch = "y" ,
                                  scales = "free_y" , drop = TRUE)
      
      
    }
    
    
  }
  
  if(SummaryVar == "Gap"){
    
    Gap_summary <-  TDat_summary %>% filter(Indicator == 'GapCover_25_plus'|Indicator =="GapCover_25_50"|Indicator =="GapCover_101_200"|Indicator =="GapCover_200_plus"|Indicator =="GapCover_51_100")
    
    # reorder factors by size
    Gap_summary$Indicator <- as.factor(Gap_summary$Indicator)
    Gap_summary$Indicator <-  factor(Gap_summary$Indicator, levels = c("GapCover_200_plus","GapCover_101_200" , "GapCover_51_100", "GapCover_25_50", 'GapCover_25_plus'))
    Gap_summary <- Gap_summary[order(Gap_summary$Indicator),]
    
    #Plot prep
    if(Interactive){
      
      Plots <- ggplot2::ggplot(data = Gap_summary , aes(x = Indicator , y = Mean , 
                                                        text = paste("Gap Class (cm): " , Indicator , 
                                                                     "Percent Cover: " , Mean , 
                                                                     "Time Period: ", Group,
                                                                     "Ecological Site: ", EcologicalSite,
                                                                     sep = "<br>"),
                                                        fill = Group)) +
        geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) + coord_flip() + 
        theme_light() + 
        scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                          breaks = c("2011-2015", "2016-2020"), drop = FALSE)+
        theme(axis.title.x = element_blank() ,
              axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() , 
              axis.title.y = element_blank() , 
              axis.line.y = element_blank(), 
              panel.grid.major.y = element_blank()) +
        facet_grid(rows = vars(Indicator) , switch = "y" ,
                   scales = "free" , drop = TRUE)
      
    }
    
    if(!Interactive){
      Plots <- ggplot2::ggplot(data = Gap_summary , aes(x = Indicator , y = Mean, fill = Group)) +
        labs(y = "Percent Cover" , x = "Gap Size Class (cm)", 
             caption = paste("Percent cover of canopy gap in: ", 
                             EcologicalSite, sep = " ")) +
        geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        coord_flip() + 
        theme_light(base_size = 16) + 
        scale_color_manual(values = Attribute_Fill, na.value="#000000") + 
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank(),
              panel.grid.major.y = element_blank()) +
        scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                          breaks = c("2011-2015", "2016-2020"), drop = FALSE)+
        facet_grid(rows = vars(Indicator) , switch = "y" ,
                   scales = "free_y" , drop = TRUE)
    }
    
  }
  
  if(SummaryVar == "SoilStability"){
    
    soil_labels <- c("SoilStability_All" = "All" , "SoilStability_Protected" = "Protected" , 
                     "SoilStability_Unprotected" = "Unprotected")
    
    Soil_summary <- TDat_summary %>% filter(Indicator == "SoilStability_All"|Indicator =="SoilStability_Protected"| Indicator  == "SoilStability_Unprotected")
    
    if(Interactive){
      
      Plots <- ggplot2::ggplot(data = Soil_summary , 
                               aes(x = Indicator , y = Mean , fill = Group, 
                                   text = paste("Ecological Site: ", EcologicalSite,
                                                "Time Period: ", Group, 
                                                "Rating: " , Mean ,
                                                sep = "<br>"))) +
        geom_errorbar(aes(ymin = LCI, ymax = UCI), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        coord_flip(ylim = c(0,6)) +
        theme_light() + 
        scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                          breaks = c("2011-2015", "2016-2020"), drop = FALSE)+
        theme(axis.text.y = element_blank() , 
              axis.ticks.y = element_blank() ,
              axis.line.y = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.title = element_blank()) +
        facet_grid(rows = vars(Indicator) , switch = "y" ,
                   scales = "free_y" , drop = TRUE , 
                   labeller = as_labeller(soil_labels))
    }
    
    
    if(!Interactive){
      Plots <- ggplot2::ggplot(data = Soil_summary, aes(x = Indicator , y = Mean, fill = Group)) +
        labs(x = "Vegetation cover class", 
             y = "Soil Stability Rating",
             caption = paste("Soil stability ratings in: ", 
                             EcologicalSite)) +
        geom_errorbar(aes(ymin =LCI, ymax = UCI), position = dodge1)+
        geom_point(shape = 23, size = 4, col = "black", position = dodge1) +
        coord_flip(ylim = c(0,6)) +
        theme_light(base_size = 16) +
        scale_color_manual(values = Attribute_Fill, na.value="#000000") +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.y = element_blank(),
              panel.grid.major.y = element_blank()) +
        scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                          breaks = c("2011-2015", "2016-2020"), drop = FALSE)+
        facet_grid(rows = vars(Indicator),
                   switch = "y",
                   scales = "free_y", 
                   drop = TRUE,
                   labeller = as_labeller(soil_labels))
      
    }
    
  }
  
  return(Plots)
  
}
### T test function
# need to use tdat long and we'll leave the species summary for now since it has overlapping observations
t.tests <-  function(TDat_LMF_Attributed, EcologicalSite, alpha){
  
  TDat_Cover_Indicators <-  c("AH_NoxAnnForbCover","AH_NoxAnnGrassCover","AH_NoxCover","AH_NonNoxAnnForbCover", "AH_PerenGrassCover","AH_PerenForbCover","AH_TallPerenGrassCover","AH_ShortPerenGrassCover")
  TDat_Cover_Indicators <-  enquo(TDat_Cover_Indicators)
  
  # Filter input dataset to include only relevant plots within the ecosite of interest
  TDat_LMF_Attributed_es <- TDat_LMF_Attributed[TDat_LMF_Attributed$EcologicalSiteId == EcologicalSite,] # dataset for ground cover and growth habit
  
  # Remove geometry if it exists
  if (any(class(TDat_LMF_Attributed_es) == "sf")){
    TDat_LMF_Attributed_es <- st_drop_geometry(TDat_LMF_Attributed_es)
  }
  
  # Summarizing Tdat to get sample stats across ecological site
  TDat_long <- TDat_LMF_Attributed_es %>% dplyr::select(PlotID, PrimaryKey, BareSoilCover , 
                                                        TotalFoliarCover , FH_TotalLitterCover , 
                                                        FH_RockCover, !!TDat_Cover_Indicators, SoilStability_All , 
                                                        SoilStability_Protected , 
                                                        SoilStability_Unprotected, GapCover_25_50 , GapCover_51_100 , 
                                                        GapCover_101_200 , GapCover_200_plus, 
                                                        GapCover_25_plus, !!attribute_title, Time_Period, EcologicalSiteId) %>% # removing un-needed cols
    gather(key = Indicator , value = Percent, BareSoilCover:GapCover_25_plus)%>% # MAKING LONG
    filter(!is.na(Percent))
  
  test <- lapply(X = split(TDat_long, TDat_long[["Indicator"]]),
                 FUN = function(TDat_long){
                   # SPILT INTO GROUPS
                   t1 <- TDat_long[TDat_long[["Time_Period"]]=="2011-2015",]
                   t2 <- TDat_long[TDat_long[["Time_Period"]]=="2016-2020",]
                   
                   # logistic transformation
                   #t1$Percent <- qlogis(t1$Percent/100)
                   # t2$Percent <- qlogis(t2$Percent/100)
                   
                   # CHECK SAMPLE SIZES
                   if(nrow(t1)>3 & nrow(t2)>3){
                     
                     # TEST NORMALITY
                     if(length(unique(t1$Percent)) > 1){
                       t1p <- shapiro.test(t1$Percent)$p.value
                     } else {t1p <- 0}
                     
                     if(length(unique(t2$Percent)) > 1){
                       t2p <- shapiro.test(t2$Percent)$p.value
                     } else {t2p <- 0}
                     
                     if(is.na(t1p)|is.na(t2p)) {
                       test <- list(p.value = NA, estimate = NA)
                     } else if(t1p > 0.05 & t2p > 0.05) {
                       test<- t.test(x = t1$Percent,
                                     y = t2$Percent,
                                     conf.level = 1-alpha)
                       
                       p.value <- round(test[["p.value"]],2)
                       mean.t1 <- round(mean(t1$Percent),2)
                       mean.t2 <- round(mean(t2$Percent),2)
                       
                       test_df <- c("Mean 2011-2015" = mean.t1, "Mean 2016-2020" = mean.t2,"p.value" = p.value)
                       
                       return(test_df)
                     } else {
                       test <- wilcox.test(x = t1$Percent,
                                           y = t2$Percent,
                                           conf.level = 1-alpha,
                                           exact = FALSE)
                       
                       p.value <- round(test[["p.value"]],2)
                       mean.t1 <- round(mean(t1$Percent),2)
                       mean.t2 <- round(mean(t2$Percent),2)
                       
                       test_df <- c("Mean 2011-2015" = mean.t1, "Mean 2016-2020" = mean.t2,"p.value" = p.value)
                       
                       return(test_df)
                     }
                   } else {test_df <-  c("Mean 2011-2015" = mean(t1$Percent), "Mean 2016-2020" = mean(t2$Percent),"p.value" = NA)}
                   return(test_df)
                 })
  
  
  test <- do.call(rbind,test) %>%
    DT::datatable(escape = FALSE,
                  extensions = 'Buttons',
                  filter = "top" ,
                  options = list(scrollX = TRUE,
                                 dom = 'Bfrtip',
                                 buttons = list(list(extend = 'collection',
                                                     buttons = c('csv',
                                                                 'excel'),                                        text = 'Download Table'))), 
                  rownames = TRUE) 
  
  return(test)
}

PlotMetadata_alt <- function(dataframe, EcologicalSite){
  Plots <- c("Plots")
  
  #Create color palettes 
  Plots_Simple <- dataframe %>% 
    filter(EcologicalSiteId == EcologicalSite)%>%
    dplyr::select(PrimaryKey, Time_Period) %>%   
    dplyr::mutate(PlotsPerYear = Plots) %>%
    dplyr::arrange(Time_Period)
  
  PlotsPerYear <- ggplot2::ggplot(Plots_Simple , aes(x= Plots, text = stat(count))) +
    geom_bar(position = "dodge",
             aes(fill = Time_Period) , width = .2) +
    scale_fill_manual(breaks = c("2011-2015","2016-2020"),
                      values = c("#E69F00", "#56B4E9")) +
    ggtitle(paste("Plots Per Time Period in", EcologicalSite)) +
    theme_minimal()+
    coord_flip() + theme(axis.text.y = element_blank())
  
  
  return(PlotsPerYear)
  
}

# Adding common ecological site names to tdat 

ES_rename <- function(lut_filepath, TDat_LMF){
  lut <- read.csv(lut_filepath)
  join <- merge(x = TDat_LMF,
                y = lut[,c(1,3)],
                by.x = "EcologicalSiteId",
                by.y = "new_es_symbol",
                all.x = TRUE,
                all.y = FALSE)
  
  join$EcologicalSiteId <- join$es_name
  join <- within(join, rm(es_name))
  join$EcologicalSiteId[is.na(join$EcologicalSiteId)] <- "UNKNOWN"
  return(join)
}
############################################### END OF FUNCTIONS ####################################

#Set the path to your R library (make sure it's on local folder not network drive)
#You must define this and run these commands everytime you restart R.
LibraryPath <- "C:\\R-4.0.3\\library"
.libPaths(LibraryPath)

#Load required packages
#load(file = "HMA_enviroment.RData")
#The following Packages are required to connect to the AIM Pub Database
library(RODBC)
library(dplyr)

#The remaining packages are required to generate the ESS Report

library(tidyverse)
library(knitr)
library(kableExtra)
library(leaflet)
library(patternplot)
library(magrittr)
library(rgdal)
library(RColorBrewer)
library(units)
library(sp)
library(sf)
library(rworldmap)
library(rworldxtra)
library(raster)
library(plotly)
library(DT)
library(SiteSummaryTool)

#################################################SET VARIABLES ##############################################

# If you are a BLM employee, set BLM <- TRUE
# If you are an external data sharing partner, set BLM <- FALSE
# For BLM- Reminder- you must be connected to vpn or plugged into the network and have established ODBC connection
# See tutorial for how to establish ODBC connection (you must do this outside of R, once you've done it once, you don't need to do it again)
BLM <- TRUE

#If BLM <- TRUE, set TerrADat_Path to NA or NULL
#If BLM <- FALSE, set TerrADat_Path to the location of your TerrADat.gdb (i.e "C:/Users/name/Documents/TerrADat.gdb")
TerrADat_Path <- NA

# Where did you download  and unzip this reporting tool and supporting documents?
# Remember to keep all supporting file names and locations as they are in this folder structure
# Set this on your LOCAL drive. You will encounter errors if running this on a network drive. 
WorkingFolder <- "C:/Users/alaurencetraynor/Documents/2020/Analysis/NM/TAFO/R"

# Set your 2 letter state abbreviation, or a string of states if you are aggregating data across state lines
State <- c("NM")

# Set your own groups using a csv lookup table to define groups of plots for each PK
Groups <- FALSE

# This is the directory of the csv containing groups and unique plot ids
groups <- NULL

# This is the column header of the group name (uniqueid is currently assumed to be "PrimaryKey")
group_name <- NULL

# When using groups of years to compare by, we need to define them:
time_period1 <- c("2011","2012","2014","2015", "2016")
time_period2 <- c("2017", "2018", "2019", "2020")

# Are you including a shapefile to append to summaries? 

# If so, set IncludeShape <- TRUE and fill out lines 38, 43, and 45
# If not, set IncludeShape <- FALSE and set lines 38, 43, and 45 to NA or NULL
IncludeShapefile <- TRUE

# IN THIS CASE WILL BE HMA SHAPEFILE 

# Set shapefile name 
# Either the shapefile (without extension) if the shapefile lives in a folder
# Or layer name if it is a feature class in a geodatabase

shapefile_path <-  "C:\\Users\\alaurencetraynor\\Documents\\2020\\Analysis\\NM\\TAFO\\TAFO.gdb"
shapefile_name <- "rgdnnm"
attribute_title <- "Reporting_Unit"
attribute_name <- "Rio Grande Del Norte National Monument"

# Confidnece interval for error bars
alpha <- 0.2

# Image for title page (should eb saved in working directory)
image_name <- "rgdnnm.jpg"
  
## That's it! Now run the rest of the lines, but no more changes are necessary. 

Rmd <- paste0(WorkingFolder, "/", "ESS_Tool_TAFO.Rmd", sep = "")

rmarkdown::render(Rmd, 
                  output_file = paste0(WorkingFolder, "/", 
                  attribute_name, "_", Sys.Date(), sep = ""))

## If you encounter pandoc error, go into your library folder and manually delete the folder titled rmarkdown
## You may need to do this in multiple locations if .libPaths() returns more than 1 file location
## Then reinstall rmarkdown from within R studio (Tools -> Install Packages)

## If you encounter errors due to certain packages not being installed, manually install each one into your specified library


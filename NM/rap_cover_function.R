#' Query RAP for mean cover data
#' @description Given a set of polygons, retrieve mean cover estimates for those polygons from the Rangeland Analysis Platform.
#' @param polygons sf polygon or multipolygon object. The polygons to retrieve mean cover estimates for.
#' @param key Character string. The API key to use.
#' @param verbose Logical. If \code{TRUE} then the function will return messages about what is happening as it runs for diagnostic purposes. Defaults to \code{FALSE}.
rap_cover <- function(polygons,
                      key,
                      verbose = FALSE){
  # Need this projection later
  wgs84_proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

  # Sanitize!
  if (!is.character(key)) {
    stop("key must be the API key as a character string")
  }
  if (!("sf" %in% class(polygons))) {
    stop("polygons must be a class sf")
  } else if (!all(sf::st_geometry_type(polygons) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("polygons must be an sf polygon or multipolygon object")
  }

  # Remove and 3D data!
  polygons <- sf::st_zm(polygons,
                        drop = TRUE)

  # Reproject to the standard WGS84 for geoJSON
  polygons <- sf::st_transform(x = polygons,
                               crs = wgs84_proj)

  # In case the polygons aren't already dissolved, we'll do that here
  polygons <- sf::st_sf(sf::st_union(polygons))

  # Add in the one necessary porperty!
  # The RAP API looks for the property mask to be TRUE
  polygons$mask <- TRUE

  # Convert the polygons to geoJSON
  # atomise = TRUE appears to be the key to making this a feature instead of a feature collection
  polygons_geojson <- geojsonsf::sf_geojson(polygons[, "mask"],
                                            atomise = TRUE)

  # Try to grab data from RAP for that geoJSON
  if (verbose) {
    message("Attempting to query RAP")
  }
  downloaded_data <- httr::POST(url = "https://us-central1-rap-data-365417.cloudfunctions.net/coverV3",
                                query = list(key = key),
                                config = httr::content_type_json(),
                                body = polygons_geojson)

  # Parse the results into a list of lists of lists of lists.......
  downloaded_data_parsed <- httr::content(downloaded_data,
                                          "parsed")


  # Get the variable names for the data
  # The first list in $properties$cover is the variable names
  variable_names <- unlist(downloaded_data_parsed$properties$cover[[1]])

  if (is.null(variable_names)) {
    stop("The query failed and RAP returned no data")
  } else if (verbose) {
    message("Data successfully retrieved")
  }

  # Make a list of data frames for each year with the appropriate variable names
  # The second through last of the lists in $properties$cover contain the data
  data_list <- lapply(X = downloaded_data_parsed$properties$cover[2:length(downloaded_data_parsed$properties$cover)],
                      variable_names = variable_names,
                      FUN = function(X, variable_names){
                        data_frame <- as.data.frame(X)
                        names(data_frame) <- variable_names
                        return(data_frame)
                      })

  # Combine all those years' data frames into one!
  data_df <- do.call(rbind,
                     data_list)

  # Make it tall for graphing purposes
  data_df_tall <- tidyr::pivot_longer(data = data_df,
                                      cols = -year,
                                      names_to = "indicator",
                                      values_to = "value")

  return(data_df_tall)
}


Plot_RAP <-  function(filepath){
  
  # Read in csv from RAP trend estimates from csv 
  data <- read.csv(filepath)
    
  p <- ggplot(data = data, aes(x = year, y= value)) +
    geom_line(aes(col = indicator), size = 1.5)+
    theme_minimal(base_size = 16)+
    scale_color_brewer(type = "qual")+
    labs(y = "Cover(%)", x = NULL)+
    theme(legend.position = "bottom", legend.title = element_blank())
  
  return(p)
}

########################### END FUNCTIONS ###################################
## Testing the function
library(arcgisbinding)
library(sf)
library(tidyverse)
arc.check_product()

# Read allotment numbers
allotments <- read.csv("C:\\Users\\alaurencetraynor\\Documents\\NM\\allotments_socorro (1).csv")

# Also read in SMA to clip
# The BLMPub.sde is being a pain her so using the URL instead
sma_url <- 'https://gis.blm.gov/arcgis/rest/services/lands/BLM_Natl_SMA_Cached_BLM_Only/MapServer/2'
sma_clause = "ADMIN_ST = 'NM' And ADMIN_AGENCY_CODE = 'BLM'"
sma <- arc.open(sma_url)
sma_poly <- arc.select(object = sma,
                       where_clause = sma_clause)
sma_poly <- arc.data2sf(sma_poly)

sf_use_s2(FALSE)

### Spitting out csvs for Allyson to use
# Theres an individual csv per allotment

for(i in allotments$Allotment_Number){
  poly_url <- 'https://gis.blm.doi.net/arcgis/rest/services/range/BLM_Natl_Grazing_Allot/MapServer/12'
  clause = paste0("ST_ALLOT = ","'", i,"'")
  print("Step 1") # I'm adding these print statements to troublehsoot issues within the loop
  poly <- arc.select(arc.open(poly_url), where_clause = clause)
  poly <- arc.data2sf(poly)
  
  print("Step 2")
  
  # Reproject SMA to the same CRS as allotments
  sma_poly <- sf::st_transform(x = sma_poly,
                           crs = st_crs(poly))
  print("Step 3")
  
  # clip to SMA
  # need st_make_valid here for invalid geometry
  clipped_poly <- sf::st_intersection(st_make_valid(poly),
                                      st_make_valid(sma_poly))
  print("Step 4")
  rap_data <- rap_cover(polygons = clipped_poly,
                        key = '')
  print("Step 5")
  write.csv(rap_data, file = paste0("C:\\Users\\alaurencetraynor\\Documents\\NM\\",i,".csv"))
}

## Plotting the data just for fun
path_base <- "C:\\Users\\alaurencetraynor\\Documents\\NM\\"

plot_list <- list()

for(allotment in allotments$Allotment_Number){
  filepath = paste0(path_base, allotment, ".csv")
  plot_list[[allotment]] <- Plot_RAP(filepath)
}

plot_list[[4]]



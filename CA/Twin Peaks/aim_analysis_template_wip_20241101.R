#### PACKAGE MANAGEMENT ########################################################
# This appears to be specific to trying to convince BLM computers using cloud
# storage on a DOI network to install packages correctly.
# Good luck!

# options("install.lock"=FALSE)

# install.packages("pkgbuild")
# library(pkgbuild)

# install.packages("gdal")
# gdal_setInstallation() #does not work
# 
# install.packages("rgdal")
# install.packages("rgeos")
# install.packages("spdplyr") #not available for this version of R?
# ### Package Setup
# # Set local library path and load required packages
library(aim.analysis)
# library(sp)
# library(dplyr)
# library(stringr)
# library(sf)
# library(rgdal)
# library(tidyverse)
# library(scales)
# library(plotly)
# library(arcgisbinding)
# arc.check_product()
# library(withr)

#### SETUP #####################################################################
##### General ------------------------------------------------------------------
# This is used as the analysis date. You can manually set it, but by default it
# will be the current date at time of execution according to the computer
# running the code.
date <- Sys.Date()

# Confidence level in percent to use for the analysis
confidence <- 80

##### Inputs -------------------------------------------------------------------
###### General -----------------------------------------------------------------
# The filepath to the folder where we have all the spatial data including points
# and polygons.
path_spatial <- "//blm.doi.net/dfs/loc/GBP/CA/CASO/projects/vegetation/AIM/GIS_data/Geodatabase/Design_Tracking/Eagle Lake/TwinPeaksHMA"


###### Polygons ----------------------------------------------------------------
# The name of the geodatabase found in path_spatial and which contains all the
# points and polygons for this analysis.
analysis_gdb <- "TwinPeaksHMA.gdb"

# The name of the feature class in analysis_gdb which contains the weight
# categories, e.g. the poststrata derived from the input designs.
wgtcats_name <- "Sample_Frame_Clip_Union"

# The variable in the weight category feature class which contains unique IDs
# for the weight categories.
wgtcats_var <- "WeightCategory"

###### Points ------------------------------------------------------------------
# The name of the points feature class containing the sampled points and which
# contains variables corresponding to those expected by the benchmarks.
# This is almost certainly coming from TerrADat.
sampled_points_name <- "SampledPoints"


# The name of the variable in the sampled point feature class that contains the
# unique identifiers for each point.
sampled_idvar <- "PlotKey"
sampled_idvar <- "PrimaryKey"

# The name of the points feature class containing the evaluated-but-unsampled
# points count towards poststrata being in the inference area but are not
# included in weight calculations.
# These should all have fates like "rejected" or "inaccessible" but it's okay if
# they don't because there's a parameter below called evaluated_unsampled_fates
# Where you can specify which fates these points will be filtered to.
# This is optional and will be ignored if set to NULL.
evaluated_unsampled_points_name <- "RejectedPoints"

# # The name of the variable in the evaluated-but-unsampled point feature class
# # that contains the unique identifiers for each point.
evaluated_unsampled_idvar <- "PlotID"

# The name of the variable in the evaluated-but-unsampled point feature class
# that contains the fate (e.g., "Inaccessible") for each point. This is only
# used to filter out points that might not have been evaluated at all.
# If you set this to NULL then all the points in the feature class will be
# assumed to be applicable.
evaluated_unsampled_fatevar <- NULL

# The values in the aim_fatevar variable in the sampling design point feature
# class which correspond to points which were unneeded (e.g. unused oversample
# or points designated for future sampling). This is likely fine to leave as-is.
evaluated_unsampled_fates = c("Inaccessible",
                              "Rejected")

###### Benchmarks --------------------------------------------------------------
# The full filepath to the Excel workbook containing the benchmarks to be used
# for this analysis
#benchmarks_filepath <- "//blm.doi.net/dfs/loc/EGIS/ProjectsNational/AIM/AIMDataTools/ArcPro/Terrestrial Benchmark Tool v4.0 BETA/Monitoring Objectives National.xlsx"
benchmarks_filepath <- "//blm.doi.net/dfs/loc/GBP/CA/CASO/projects/vegetation/AIM/GIS_data/Geodatabase/Design_Tracking/Eagle Lake/Terrestrial_benchmarktool_NationalBenchmarks.xlsx"

##### Outputs ------------------------------------------------------------------
# Core filename for all outputs/name of reporting unit. This should a
# recognizable abbreviation, e.g., "TAFO_LUP" for the Taos Field Office's Land
# Use Plan
output_filename <- "TPA"

# The filepath to write outputs to
output_path <- "C:\\Users\\alaurencetraynor\\Documents\\CA\\Twin Peaks"

##### Other --------------------------------------------------------------------
# We'll need to use Alber's Equal Area when calculating areas, but we can also
# just use it as the standard projection for all our spatial data.
# You really shouldn't change this.
projection <- sf::st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

#### READING ###################################################################
##### Weight category polygons -------------------------------------------------
wgtcat_sf <-sf::st_read(dsn = paste(path_spatial,
                                    analysis_gdb,
                                    sep = "/"),
                        layer = wgtcats_name,
                        stringsAsFactors = FALSE)
## Standardize projection
wgtcat_sf <- sf::st_transform(wgtcat_sf,
                              crs = projection)

# Rename the wgtcat variable
wgtcat_sf <- dplyr::rename(.data = wgtcat_sf,
                           wgtcat = tidyselect::matches(match = wgtcats_var))

# Add areas
#wgtcat_sf <- aim.analysis::add_area(polygons = wgtcat_sf)
wgtcat_sf$area_ha <-sf::st_area(wgtcat_sf)
wgtcat_sf <- drop_units(wgtcat_sf)
##### Points -------------------------------------------------------------------
###### Sampled points ----------------------------------------------------------
sampled_points <- sf::st_read(dsn = paste(path_spatial,
                                          analysis_gdb,
                                          sep = "/"),
                              layer = sampled_points_name,
                              stringsAsFactors = FALSE) |>
  ## Standardize projection
  sf::st_transform(x = _,
                   crs = projection)

# Standardize variable names
if ("unique_id" %in% names(sampled_points) & sampled_idvar != "unique_id") {
  sampled_points <- dplyr::select(.data = sampled_points,
                                  -unique_id)
}

sampled_points <- dplyr::rename(.data = sampled_points,
                                unique_id = tidyselect::matches(match = paste0("^",
                                                                               sampled_idvar,
                                                                               "$"))) |>
  dplyr::mutate(.data = _,
                fate = "TS",
                Reporting_Unit = output_filename)

if (any(table(sampled_points$unique_id) > 1)) {
  stop(paste("The variable", sampled_idvar, "in the sampled points contains non-unique values."))
}

###### Evaluated-but-unsampled points ------------------------------------------
if (!is.null(evaluated_unsampled_points_name)) {
  evaluated_unsampled_points <- sf::st_read(dsn = paste(path_spatial,
                                                        analysis_gdb,
                                                        sep = "/"),
                                            layer = evaluated_unsampled_points_name,
                                            stringsAsFactors = FALSE) |>
    ## Standardize projection
    sf::st_transform(x = _,
                     crs = projection)
  
  # Standardize variable names
  # evaluated_unsampled_points <- dplyr::rename(.data = evaluated_unsampled_points,
  #                                             unique_id = tidyselect::matches(match = evaluated_unsampled_idvar))
  
  # Check for fates and filter as appropriate
  if (!is.null(evaluated_unsampled_fatevar)) {
    evaluated_unsampled_points <- dplyr::mutate(.data = evaluated_unsampled_points,
                                                fate = evaluated_unsampled_fatevar) |>
      dplyr::filter(.data = _,
                    fate %in% evaluated_unsampled_fates)
  } else {
    evaluated_unsampled_points <- dplyr::mutate(.data = evaluated_unsampled_points,
                                                fate = "Evaluated but unsampled")
  }
} else {
  evaluated_unsampled_points <- NULL
}


##### Benchmarks ---------------------------------------------------------------
benchmarks <- read_benchmarks(filename = "Terrestrial_benchmarktool_NationalBenchmarks.xlsx")

benchmarks$Reporting_Unit <- output_filename

#### MUNGING ###################################################################
##### Add weight categories to points ------------------------------------------
# I'm doing this here because the points I was working with didn't have wgtcat
# assigned yet, so we can use a spatial join.
# We're joining this back to the original in case some of them didn't fall
# within one of the weight category polygons.
sampled_points <- sf::st_intersection(x = sampled_points,
                                      y = dplyr::select(.data = wgtcat_sf,
                                                        wgtcat)) |>
  dplyr::select(.data = _,
                unique_id,
                wgtcat) |>
  dplyr::left_join(x = _,
                   y = sf::st_drop_geometry(sampled_points),
                   by = "unique_id",
                   relationship = "one-to-one")

sampled_points_wgtcats <- sampled_points$wgtcat

if (!is.null(evaluated_unsampled_points)) {
  evaluated_unsampled_points <- sf::st_intersection(x = evaluated_unsampled_points,
                                                    y = dplyr::select(.data = wgtcat_sf,
                                                                      wgtcat)) |>
    dplyr::select(.data = _,
                  wgtcat)
  evaluated_unsampled_points_wgtcats <- evaluated_unsampled_points$wgtcat
} else {
  evaluated_unsampled_points_wgtcats <- NULL
}


##### Create inference area polygons -------------------------------------------
# We want a maximum inference area extent. This can only be used if there are
# no weight categories that didn't have points. Even if there are empty weight
# categories, we can use this to inform the user about how much inference area
# was lost.
desired_analysis_frame <- sf::st_union(x = wgtcat_sf) |>
  # Sorry there's no placeholder _ for the piped value. I can't use one here
  # because the argument it would be assigned to is unnamed.
  sf::st_sf(crs = projection) |>
  sf::st_make_valid(x = _) |>
  sf::st_buffer(x = _,
                dist = 0)

if (any(!(wgtcat_sf$wgtcat %in% c(sampled_points_wgtcats, evaluated_unsampled_points_wgtcats)))) {
  message("There are weight categories without points which will be dropped. This means a restricted inference area.")
  
  # We need to keep only the weight categories that contained points that have
  # been evaluated.
  wgtcat_sf <- dplyr::filter(.data = wgtcat_sf,
                             wgtcat %in% c(sampled_points_wgtcats, evaluated_unsampled_points_wgtcats))
  
  # And we'll make an inference area polygon for just those weight categories.
  # Once again, we'll stick some steps in here to make sure that the
  # geoprocessing doesn't introduce errors.
  actual_analysis_frame <- sf::st_union(x = wgtcat_sf) |>
    sf::st_sf(crs = projection) |>
    sf::st_make_valid(x = _) |>
    sf::st_buffer(x = _,
                  dist = 0)
} else {
  actual_analysis_frame <- desired_analysis_frame
}

missing_area_m2 <- sf::st_area(desired_analysis_frame) - sf::st_area(actual_analysis_frame)

##### Add weights to points ----------------------------------------------------
# Weights are calculated for only the sampled points!
sampled_points <- dplyr::summarize(.data = sf::st_drop_geometry(sampled_points),
                                   .by = "wgtcat",
                                   point_count = dplyr::n()) |>
  dplyr::left_join(x = _,
                   y = dplyr::select(.data = sf::st_drop_geometry(wgtcat_sf),
                                     wgtcat,
                                     area = tidyselect::matches(match = "area_ha")),
                   by = "wgtcat") |>
  dplyr::mutate(.data = _,
                weight = area / point_count) |>
  dplyr::select(.data = _,
                wgtcat,
                weight) |>
  dplyr::left_join(x = sampled_points,
                   y = _,
                   by = "wgtcat")

#### BENCHMARKING ##############################################################
benchmarked_points <- apply_benchmarks(data = sf::st_drop_geometry(sampled_points),
                                       indicator_lookup_path = ("//blm.doi.net/dfs/loc/GBP/CA/CASO/projects/vegetation/AIM/GIS_data/Geodatabase/Design_Tracking/Eagle Lake/R Scripts/Eagle Lake Codes/Data/indicator_lut2.csv"),
                                       benchmarks = benchmarks,
                                       data_id_vars = c("unique_id"),
                                       data_group_var = NULL,
                                       benchmark_group_var = NULL,
                                       benchmark_indicator_var = "Indicator",
                                       use_tdat_indicator_lookup = TRUE) |>
  # Not sure why there are NA values, but we'll simply strip them out because
  # all the expected benchmarked indicators are still present.
  dplyr::filter(.data = _,
                !is.na(indicator))


#### ANALYSIS ##################################################################
analysis <- analyze_cat_multi(data = dplyr::filter(.data = benchmarked_points,
                                                   # Figure out why these are necessary!!!!
                                                   !is.na(indicator),
                                                   !is.na(Condition_Category)),
                              weights = sf::st_drop_geometry(x = dplyr::select(.data = sampled_points,
                                                                               unique_id,
                                                                               weight)),
                              id_var = "unique_id",
                              cat_var = "Condition_Category",
                              wgt_var = "weight",
                              split_vars = c("indicator",
                                             "Management_Question"),
                              definitions = dplyr::distinct(dplyr::select(.data = benchmarks,
                                                                          indicator = Indicator,
                                                                          Management_Question,
                                                                          Condition_Category)),
                              conf = confidence,
                              verbose = FALSE)

#### WRITING ###################################################################
##### Tabular ------------------------------------------------------------------
###### Analysis results --------------------------------------------------------
write.csv(analysis,
          file = paste0(output_path, "/",
                        output_filename, "_analysis_",
                        date, ".csv"),
          row.names = FALSE)

##### Spatial ------------------------------------------------------------------
###### Polygons ----------------------------------------------------------------
sf::st_write(obj = desired_analysis_frame,
             dsn = output_path,
             layer = paste0(output_filename, "_desiredAOI_",
                            date, ".shp"),
             driver = "ESRI Shapefile")
sf::st_write(obj = actual_analysis_frame,
             dsn = output_path,
             layer = paste0(output_filename, "_actualAOI_",
                            date, ".shp"),
             driver = "ESRI Shapefile")

###### Points ------------------------------------------------------------------
# The output points will be written in two ways:
# Wide is probably the expected where the points have a separate variable for
# every rating.
# Tall has a separate replicate point for each rating for that location.
# Both are written out because writing to an ESRI shapefile will almost certainly
# abbreviate the wide points' variable names beyond all recognition.
# Writing to a layer in a geodatabase using arcgisbinding would sidestep the
# issue but is a total pain to get working in some IT environments and requires
# an ESRI license.
dplyr::mutate(.data = benchmarked_points,
              Management_Question = stringr::str_replace_all(string = Management_Question,
                                                         pattern = "[ ]",
                                                         replacement = "_")) |>
tidyr::pivot_wider(data = _,
                   names_from = c(indicator, Management_Question),
                   names_sep = "_",
                   values_from = Condition_Category) |>
  dplyr::left_join(x = dplyr::select(.data = sampled_points,
                                     unique_id),
                   y = _,
                   relationship = "one-to-one",
                   by = "unique_id") |>
sf::st_write(obj = _,
             dsn = output_path,
             layer = paste0(output_filename, "_benchmarked_points_wide_",
                            date, ".shp"),
             driver = "ESRI Shapefile")

dplyr::left_join(x = dplyr::select(.data = sampled_points,
                                   unique_id),
                 y = benchmarked_points,
                 relationship = "one-to-many",
                 by = "unique_id") |>
  sf::st_write(obj = _,
               dsn = output_path,
               layer = paste0(output_filename, "_benchmarked_points_tall_",
                              date, ".shp"),
               driver = "ESRI Shapefile")

?rgdal

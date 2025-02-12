##### Rawlins FO - Upper and Lower Platte watershed Proportional Area estimates
#### Overview
# Request initiated by Cheryl Newberry from Rawlins FO 
# Proportional area estimates requested for Mid-February in two reporting units - the Upper Platte watershed and the Lower Platte watershed
# Benchmarks were provided by Cheryl and are consistent with the 'Great Divide' weighted analysis performed in 2018
# Benchamrk groups are based on crew observations of plant community types on the ground and are reflected in the SDD in the 'Actual Stratum' field
# Cheryl requested LMF points not be used for analysis as LMF crews do not identify these benchmark groups similarly

### Package Setup
# Set local library path and load required packages
library(devtools)
library(aim.analysis)
library(sp)
library(dplyr)
library(binom)
library(stringr)

### Defaults and filepaths

# Projection to use. Alber's Equal Area is useful for calculating areas
projection <- sp::CRS("+proj=aea")

# Date of analysis
date <- "20200212"

# Confidence level in percent
confidence <- 80

output_path <- "C:\\Users\\alaurencetraynor\\Documents\\2019\\Analysis\\Rawlins FO\\2020\\outputs"

# Filename for benchmark tool from analysis requestor
benchmarks_filename <- "AIMTerrestrialBenchmarkTool_3.1_RAWLINS.xlsm"

###### MAYBE WRITE A FUNCTION TO CONVERT BENCHAMRK TOOL PLOT SUMMERY TAB TO SAME FORMAT AS HAF SUA RATINGS SHEET ######

# Core filename for all outputs
output_filename <- "lower_platte"

# The geodatabase and layer names for the spatial data sources
# are embedded in the code below

# The variable in AIM sample designs that contains the unique identifier for each point
aim_idvar <- "PLOT_NM"
# The variable in AIM sample designs that contains the fate for each point
aim_fatevar = "FINAL_DESIG"
# The variable in TerrADat that contains the unique identifier for each point matching AIM designs'
tdat_idvar <- "PlotID"

# The fates in sample designs which correspond to sampled/observed
observed_fates = c("TS", "Target Sampled", "Sampled")

# The fates in sample designs which correspond to unneeded
# (e.g. unused oversample or points designated for future sampling)
invalid_fates = c("NN", NA, "Non Needed", "", "Not Sampled")

# The variable that contains the areas in HECTARES in wgtcats
wgtcat_area_var = "area_hectares"

# The unique ID for poststratification polygons
wgtcats_var <- "postrata_id"

### The filepath to the folder where we have all the spatial data
path_spatial <- "C:\\Users\\alaurencetraynor\\Documents\\2019\\Analysis\\Rawlins FO\\2020\\inputs\\spatial"

### The filepath to the folder where we have the Excel stuff
path_tabular <- "C:\\Users\\alaurencetraynor\\Documents\\2019\\Analysis\\Rawlins FO\\2020\\inputs\\tabular"


### Read in
#### Reporting unit(s)
# Strata and points are already clipped to reporting units in Arc

#### Strata polygons
wgtcat_df <-sf::st_read(dsn = paste(path_spatial, "Platte Analysis.gdb", sep = "/"),
                        layer = "strata_lower_aea",
                        stringsAsFactors = FALSE)
## Standardize projection
wgtcat_df <- sf::st_transform(wgtcat_df,
                              crs = projection)
## Make spatial 
wgtcat_spdf <- methods::as(wgtcat_df, "Spatial")
wgtcat_spdf@data[["wgtcat"]] <- wgtcat_spdf@data[[wgtcats_var]]

sf::st_geometry(wgtcat_df) <- NULL
wgtcats <- as.data.frame(wgtcat_df)[,c(4,10,13)]
colnames(wgtcats)[2] <- "wgtcat"
wgtcats$wgtcat <- as.character(wgtcats$wgtcat)

#### Benchmarks
# make sure all columns (including required proportions) are filled out - otherwise apply_benchamrks() will give errors
benchmarks <- read_benchmarks(filename = benchmarks_filename,
                              filepath = path_tabular)

#### Read in Terradat points
# Make sure points and polygons do not include Z/M dimensions...
sampled_points <- sf::st_read(dsn = paste(path_spatial, "Platte Analysis.gdb", sep = "/"),
                              layer = "terradat_lower_join",
                              stringsAsFactors = FALSE)
## Standardize projection
sampled_points <- sf::st_transform(sampled_points,
                                   crs = projection)
## Make spatial 
sampled_points <- methods::as(sampled_points, "Spatial")

# give the reporting unit identitty to the points and add final desi
sampled_points@data[["Reporting.Unit"]] <- "Lower Platte"
sampled_points@data[["fate"]] <- "Target Sampled"

# Standardise columns
sampled_points@data[["unique_id"]] <- sampled_points@data[[tdat_idvar]]
sampled_points@data[["wgtcat"]] <- sampled_points@data[[wgtcats_var]]
sampled_points@data[["wgtcat"]] <- as.character(sampled_points@data[["wgtcat"]])

#### Read in design points
# Make sure points and polygons do not include Z/M dimensions...
design_points <- sf::st_read(dsn = paste(path_spatial, "Platte Analysis.gdb", sep = "/"),
                             layer = "design_points_lower_join",
                             stringsAsFactors = FALSE)
## Standardize projection
design_points <- sf::st_transform(design_points,
                                  crs = projection)
## Make spatial 
design_points <- methods::as(design_points, "Spatial")

# Standardise columns
design_points@data[["unique_id"]] <- design_points@data[[aim_idvar]]
design_points@data[["fate"]] <- design_points@data[[aim_fatevar]]
design_points@data[["wgtcat"]] <- design_points@data[[wgtcats_var]]
design_points@data[["wgtcat"]] <- as.character(design_points@data[["wgtcat"]])

### Add benchmark values
# First is benchmark groups
benchmark_group_lut <- read_benchmarkgroups(path = path_tabular,
                                            id_var = "PrimaryPlotKey",
                                            filename = benchmarks_filename)

# Then apply the benchmarks by benchmark group
sampled_points@data[["OtherShrubHgt_Avg"]] <- as.numeric(sampled_points@data[["OtherShrubHgt_Avg"]])
sampled_points@data[["SagebrushHgt_Avg"]] <- as.numeric(sampled_points@data[["SagebrushHgt_Avg"]])

# AIM 2.0 has several slightly different indicator names - need to update indicator.lookup function to refelct these
sampled_benchmarked <- apply_benchmarks(data = sampled_points,
                                        idvars = c("PrimaryKey", "Reporting.Unit"),
                                        benchmark_group_lut = benchmark_group_lut,
                                        benchmarks = benchmarks,
                                        verbose = TRUE)

sampled_points_benchmarked <- merge(x = data.frame(sampled_points@data[, c("unique_id","PrimaryKey")]),
                                    y = sampled_benchmarked,
                                    by = "PrimaryKey")

# Checking against the ratings confirms if all the rated plots are represented (important)
# and if all the sampled_plots are rated (not important, but informative)
if (!all(sampled_points_benchmarked[["Plot_ID"]][sampled_points_benchmarked] %in% sampled_points$unique_id)) {
  warning("NOT ALL RATED AIM PLOTS ARE PRESENT IN sampled_points!")
}

all(sampled_points$unique_id %in% sampled_points_benchmarked[["Plot_ID"]]) # This is false because sampled points still contains winter/summer plots

# filter sampled plots by those that were rated:
# sampled_points <- sampled_points[sampled_points$unique_id %in% sampled_points_benchmarked[["Plot_ID"]], ] 

### Replace all sampled points with their TerrADat counterparts
all_points <- rbind(design_points[!(design_points$fate %in% observed_fates), c("unique_id", "fate", "wgtcat")], 
                    sampled_points[, c("unique_id", "fate", "wgtcat")])

###### Analyze ######
weight_info <- weight.gen(pts = all_points,
                          pts.fatefield = "fate",
                          pts.groupfield = "wgtcat",
                          frame.spdf = wgtcat_spdf,
                          frame.groupfield = "wgtcat",
                          target.values = c("Target Sampled", "TARGET SAMPLED", "TS"),
                          unknown.values = c("Unknown", "UNKNOWN", "UNK"),
                          nontarget.values = c("NT"),
                          inaccessible.values = c("IA"),
                          unneeded.values = invalid_fates)
point_weights <-  weight_info[["point.weights"]]
all_points <-  aim.analysis::add_coords(all_points,
                                        xynames = c("x", "y"))

benchmarked_points <- merge(x = all_points@data,
                            y = sampled_points_benchmarked,
                            all.x = TRUE)

all_points[["Reporting.Unit"]] <- "Lower Platte"
weight_info[["point.weights"]][["Reporting.Unit"]] <- "Lower Platte"

# a definitions list for analyze_cat_multi()
cat_defs <-  readxl::read_xlsx(path = paste(path_tabular,benchmarks_filename, sep = "/"),
                               sheet = "Definitions",
                               na = "NA")
ind_lut <- indicator.lookup()
cat_defs <-  merge(x = cat_defs,
                   y = ind_lut,
                   by.x = "indicator",
                   by.y = "indicator.name",
                   all.x = TRUE,
                   all.y = FALSE)[,2:4]
names(cat_defs)[3] <- "indicator"

benchmarked_points$Benchmark.Group %in% cat_defs$Benchmark.Group
unique(benchmarked_points$Benchmark.Group)
unique(cat_defs$Benchmark.Group)

# analyze-cat_multi not working - lets try subsetting the data
data <- split(x=benchmarked_points,
      f=benchmarked_points$indicator)

analysis <- analyze_cat_multi(data = benchmarked_points,
                              weights = point_weights,
                              id_var = "unique_id",
                              cat_var = "Condition.Category",
                              wgt_var = "WGT",
                              split_vars = c("indicator", "Benchmark.Group"),
                              definitions = cat_defs,
                              conf = 80,
                              verbose = TRUE)

### Formatting Outputs
analysis <- analysis[, c("Subpopulation",
                         "Indicator",
                         "Category",
                         "NResp",
                         "Estimate.P",
                         "Estimate.U")]
analysis <- analysis[analysis$Category!="Total",]

# adjusted counts = sum of all plots within each indicator (n=5) * estimated proportion
analysis_summary <- analysis[analysis$Category!="Total",] %>%
  group_by(Indicator)%>%
  summarise(count = sum(NResp),
            category_count = n(),
            total_observed_area = sum(Estimate.U))

analysis$adjusted_counts <- analysis$NResp*(analysis$Estimate.P/100)

# Basic math, but important!!!
alpha <- 1 - confidence / 100
analysis_summary$observation_count <- by(analysis$adjusted_counts, analysis$Indicator, sum)

analysis <-  merge(x= analysis,
                   y = analysis_summary,
                   all.x = TRUE,
                   by = "Indicator")

analysis$proportions <- analysis$adjusted_counts / analysis$observation_count

# Get a value for the quantile based on the alpha and the number of categories
chi <- stats::qchisq(p = 1 - (alpha / analysis$category_count),
                     df = 1)

# Calculate the bounds!
# Note that these are symmetrical, just not around the proportion.
# They're symmetrical around (chi + 2 * counts) / (2 * (chi + observation_count))

lower_bounds <- (chi + 2 * analysis$adjusted_counts - sqrt(abs(chi^2 + 4 * analysis$adjusted_counts * chi * (1 - analysis$proportions)))) / (2 * (chi + analysis$observation_count))
upper_bounds <- (chi + 2 * analysis$adjusted_counts + sqrt(abs(chi^2 + 4 * analysis$adjusted_counts * chi * (1 - analysis$proportions)))) / (2 * (chi + analysis$observation_count))

# A proportion can never be greater than 1 or less than 0
# So we'll add bounds any CIs in case that happens
# That's definitely a thing that can happen if the magnitude of sqrt(chi^2 + 4 * counts * chi * (1 - proportions))
# is large enough
lower_bounds_capped <- lower_bounds
lower_bounds_capped[lower_bounds < 0] <- 0
upper_bounds_capped <- upper_bounds
upper_bounds_capped[upper_bounds > 1] <- 1

# Build the output  
analysis$lower_bound <- lower_bounds_capped
analysis$upper_bound <- upper_bounds_capped


analysis[["lower_bound"]] <- analysis[["lower_bound"]] * 100
analysis[["upper_bound"]] <- analysis[["upper_bound"]] * 100

# Calculate the hectares using the Goodman binomial confidence intervals from percentage

analysis[[paste0("LCB", confidence, "Pct.U.Goodman")]] <- analysis[["lower_bound"]] / 100 * analysis[["total_observed_area"]]
analysis[[paste0("UCB", confidence, "Pct.U.Goodman")]] <- analysis[["upper_bound"]] / 100 * analysis[["total_observed_area"]]

ind_lut <- indicator.lookup()
analysis <- merge(x=analysis,
                  y=ind_lut,
                  by.x ="Indicator",
                  by.y = "indicator")

analysis <- analysis[,c("indicator.name",
                        "Category",
                        "Subpopulation",
                        "NResp",
                        "Estimate.P",
                        "Estimate.U",
                        "lower_bound",
                        "upper_bound",
                        "LCB80Pct.U.Goodman",
                        "UCB80Pct.U.Goodman")]

names(analysis) <- c("Indicator",
                     "Category",
                     "Reporting Unit",
                     "Number of plots",
                     "Estimated percent of sampled area",
                     "Estimated hectares",
                     paste0(c("Lower confidence bound of percent area (", "Upper confidence bound of percent area ("), confidence, "%, Goodman multinomial) "),
                     paste0(c("Lower confidence bound of hectares (", "Upper confidence bound of hectares ("), confidence, "%, Goodman multinomial) "))

fates <- unique(all_points[["fate"]])

wgtcat_summary <- weight_info[["frame.summary"]]
names(wgtcat_summary) <- c("wgtcat",
                           "total_count",
                           "count_Target Sampled",
                           "count_NT",
                           "count_IA",
                           "count_NA",
                           "count_UNK",
                           "area_ha",
                           "prop.dsgn.pts.obsvd",
                           "sampled.area.ha",
                           "weight")

for (fate in fates) {
  wgtcat_summary[[paste0("count_", fate)]][is.na(wgtcat_summary[[paste0("count_", fate)]])] <- 0
}

wgtcat_summary[["in_inference"]] <- (wgtcat_summary[["weight"]])>0

wgtcat_summary$area_units <- "hectares"
wgtcat_summary$unobserved_area <- wgtcat_summary$area_ha - wgtcat_summary$sampled.area.ha

wgtcat_summary <- wgtcat_summary[, c("wgtcat", "area_ha", "area_units",
                                     paste0("count_", fates),
                                     "prop.dsgn.pts.obsvd",
                                     "sampled.area.ha", "unobserved_area",
                                     "in_inference")]

wgtcats$wgtcat <-  as.character(wgtcats$wgtcat)
wgtcat_summary <- merge(x = wgtcat_summary,
                        y = wgtcats[, c("DMNNT_STRTM", "wgtcat")],
                        by.x = "wgtcat",
                        by.y = "wgtcat")

names(wgtcat_summary) <- c("poststratum_id", "area", "area_units",
                           "count_NA","count_IA","observed_point_count",
                           "observed_proportion",
                           "observed_area", "unobserved_area",
                           "in_inference", "design_stratum")

# Add in where they're from
all_points <- sp::merge(x = all_points,
                        y = point_weights[,c("unique_id","WGT")],
                        all.x = TRUE,
                        by = "unique_id")

# reformat benchmark_points to wide format

benchmarked_points_wide <- tidyr::pivot_wider(data = benchmarked_points,
                                              names_from = "indicator",
                                              values_from = "Condition.Category")
benchmarked_points_wide <- dplyr::select(benchmarked_points_wide,
                                         -c(fate,
                                            wgtcat,
                                            x,
                                            y,
                                            PrimaryKey,
                                            Reporting.Unit,
                                            Benchmark.Group,
                                            `NA`))

all_points <- sp::merge(x = all_points,
                        y = benchmarked_points_wide,
                        by = "unique_id",
                        all.x = TRUE)

names(all_points@data) <- c("plot_id", "fate", "wgtcat","x","y", "reporting_unit",
                            "wgt", "Sagebrush Height", "Non Invasive Perennial Grass Height", "Non Invasive Perennial Grass Cover", "Sagebrush Cover", "Non Invasive Perennial Forb Cover", "Bare Soil Cover", "Invasive Annual Grass Cover", "Invasive Annual Forb Cover")

all_points <- all_points[, c("plot_id", "fate", "reporting_unit", "wgtcat", "wgt", "Sagebrush Height", "Non Invasive Perennial Grass Height", "Non Invasive Perennial Grass Cover", "Sagebrush Cover", "Non Invasive Perennial Forb Cover", "Bare Soil Cover", "Invasive Annual Grass Cover", "Invasive Annual Forb Cover")]



### Writing Outputs

write.csv(point_weights,
          file = paste0(output_path, "/",
                        output_filename,
                        "_pointweights_",
                        date, ".csv"),
          row.names = FALSE)
write.csv(wgtcat_summary,
          file = paste0(output_path, "/",
                        output_filename,
                        "_wgtcatsummary_",
                        date, ".csv"),
          row.names = FALSE)
write.csv(analysis,
          file = paste0(output_path, "/",
                        output_filename,
                        "_analysis_",
                        date, ".csv"),
          row.names = FALSE)
rgdal::writeOGR(all_points,
                dsn = output_path,
                layer = paste0(output_filename,
                               "_points_",
                               date),
                driver = "ESRI Shapefile",
                overwrite_layer = TRUE)

#################################################################

# Graphics
library(ggplot2)
analysis1 <-subset(analysis, Category=="Meeting"|Category =="Not Meeting")
# add in zeros for not meeting
analysis_zero <-  analysis1
analysis_zero$Category <- "Not Meeting"
analysis_zero$`Number of plots` <- 0
analysis_zero$`Estimated percent of sampled area` <-  0
analysis_zero$`Estimated hectares` <-  0
analysis_zero$`Lower confidence bound of percent area (80%, Goodman multinomial) ` <-  0
analysis_zero$`Upper confidence bound of percent area (80%, Goodman multinomial) ` <- 0
analysis_zero$`Lower confidence bound of hectares (80%, Goodman multinomial) ` <- 0
analysis_zero$`Upper confidence bound of hectares (80%, Goodman multinomial) ` <- 0
analysis1 <-  rbind(analysis1, analysis_zero)
analysis1$Category <- factor(analysis1$Category,levels(factor(analysis1$Category))[c(2,1)])

analysis2 <-analysis[analysis$Category!="Meeting",]
analysis2 <-analysis[analysis$Category!="Not Meeting",]
# reorder factors
analysis2$Category <- factor(analysis2$Category,levels(factor(analysis2$Category))[c(3,4,1,2)])

ggplot2::ggplot(data = analysis2[analysis2$Category!="Uknown/Irrelevant",],
                ggplot2::aes(y = `Estimated hectares`,
                             x = Category, fill = Category))+
  ggplot2::geom_col(alpha = 0.6, position = "dodge")+
  ggplot2::geom_errorbar(ggplot2::aes(ymin = `Lower confidence bound of hectares (80%, Goodman multinomial) `,
                                      ymax = `Upper confidence bound of hectares (80%, Goodman multinomial) `),
                         width = 0.5)+
  facet_wrap(.~Indicator)+
  coord_flip()+
  theme_minimal(base_size = 16)+
  labs(x = "")+
  scale_fill_manual(values = c("#FF0000",
                               "#FFFF00",
                               "#3CE922"))

ggsave(device = "jpeg", dpi = 400, width = 17, filename = "lower_platte1.jpeg", path = output_path)

ggplot2::ggplot(data = analysis1,
                ggplot2::aes(y = `Estimated hectares`,
                             x = Category, fill = Category))+
  ggplot2::geom_col(alpha = 0.6, position = "dodge", width = 0.7)+
  ggplot2::geom_errorbar(ggplot2::aes(ymin = `Lower confidence bound of hectares (80%, Goodman multinomial) `,
                                      ymax = `Upper confidence bound of hectares (80%, Goodman multinomial) `),
                         width = 0.5)+
  facet_wrap(.~Indicator, ncol = 2)+
  coord_flip()+
  labs(x="")+
  theme_minimal(base_size = 16)+
  scale_fill_manual(values = c("#FF0000",
                               "#3CE922"))

ggsave(device = "jpeg", dpi = 400, width = 12, filename = "lower_platte2.jpeg", path = output_path)

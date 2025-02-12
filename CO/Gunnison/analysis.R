## KNOWN ISSUES ----
# Workbook data:
# Several plot IDs had prefix numbers added by crews (e.g. Cotton-411 recorded as Cotton-1411) which were removed
# Cotton-411 plot coordinates are 250 m from the design location
# No design information for the plots without strata prefixes (0043, 0071, 0155)
# Oak-138 plot coordinates are 25 m from design coordinates for Cotton-068
# Oak-138 does not appear in designs, but the most likely match in the designs, Oak-183, is 8.8 km from the recorded coordinates

# Read in the stuff from the workbook


data.terrestrial <- readxl::read_excel("data/GUFO_Zone6_BenchmarkTool_20190417.xlsm",
                                       sheet = "TerrADat-Terrestrial Data")

data.remote <- readxl::read_excel("data/GUFO_Zone6_BenchmarkTool_20190417.xlsm",
                                  sheet = "TerrADat - RS Data")

indicator.lut <- readxl::read_excel("data/GUFO_Zone6_BenchmarkTool_20190417.xlsm",
                                    sheet = "Reference (Read-Only)")[,1:2]

data.combined <- merge(x = data.terrestrial[!is.na(data.terrestrial$PrimaryKey), c("Plot ID", "PrimaryKey", names(data.terrestrial)[names(data.terrestrial) %in% indicator.lut[["Indicator Name"]]])],
                       y = data.remote[!is.na(data.remote$PrimaryKey), c("Plot ID", "PrimaryKey", names(data.remote)[names(data.remote) %in% indicator.lut[["Indicator Name"]]])])

for (name in indicator.lut[["Indicator Name"]][indicator.lut[["Indicator Name"]] %in% names(data.combined)]) {
  names(data.combined)[names(data.combined) == name] <- indicator.lut$Indicator[indicator.lut[["Indicator Name"]] == name]
}

names(data.combined)[names(data.combined) == "Plot ID"] <- "PlotID"

# Read in the benchmarks
benchmarks <- aim.analysis::read.benchmarks(data.path = "data",
                                            filename = "GUFO_Zone6_BenchmarkTool_20190417.xlsm")

benchmarks <- merge(x = benchmarks,
                    y = indicator.lut,
                    by.x = "Indicator",
                    by.y = "Indicator Name")

names(benchmarks)[names(benchmarks) == "Indicator.y"] <- "indicator"

benchmark.groups <- aim.analysis::read.benchmarkgroups("data/GUFO_Zone6_BenchmarkTool_20190417.xlsm")
names(benchmark.groups)[1] <- "PrimaryKey"

data.benchmark.grouped <- merge(x = data.combined,
                          y = benchmark.groups,
                          all.x = TRUE)

data.benchmark.group.tall <- unique(tidyr::gather(data.benchmark.grouped,
                                       key = "indicator",
                                       value = "value",
                                       -PrimaryKey, -PlotID, -benchmark.group))

data_benchmark_assigned <- unique(merge(x = data.benchmark.group.tall,
                                 y = benchmarks,
                                 by.y = c("Benchmark.Group", "indicator"),
                                 by.x = c("benchmark.group", "indicator")))
eval_results <- mapply(vector1 = mapply(value = data_benchmark_assigned[["value"]], inequality = data_benchmark_assigned[["evalstring1"]],
                                        FUN = function(value, inequality) {
                                          eval(parse(text = gsub(inequality, pattern = "x", replacement = paste(value))))
                                        }),
                       vector2 = mapply(value = data_benchmark_assigned[["value"]], inequality = data_benchmark_assigned[["evalstring2"]],
                                        FUN = function(value, inequality) {
                                          eval(parse(text = gsub(inequality, pattern = "x", replacement = paste(value))))
                                        }),
                       FUN = function(vector1, vector2){
                         vector1 & vector2
                       })

data_benchmarked <- data_benchmark_assigned[eval_results, c("PrimaryKey", "PlotID", "Reporting.Unit", "benchmark.group", "Indicator", "indicator", "value", "Condition.Category")]

data_benchmarked <- data_benchmarked[!is.na(data_benchmarked[["PlotID"]]),]

data_benchmarked <- merge(data_benchmarked,
              data.terrestrial[, c("Plot ID", "Latitude", "Longitude")],
              by.x = "PlotID",
              by.y = "Plot ID")

write.csv(data_benchmarked,
          "gufo_data_benchmarked_20190417.csv")

# Weight stuff
# These two shapefiles were made laboriously by hand with a lot of design checking
# They should represent the weight categories and the points with the sampling status of the points
points.spdf <- rgdal::readOGR(dsn = "C:/Users/Nelson/Documents/Projects/Colorado/GUFO/analysis/data",
                              layer = "points_plus_wgtcats",
                              stringsAsFactors = FALSE)


wgtcats.spdf <- rgdal::readOGR(dsn = "C:/Users/Nelson/Documents/Projects/Colorado/GUFO/analysis/data",
                               layer = "wgtcats_20190227",
                               stringsAsFactors = FALSE)

# If the points were oversample but not sampled, they just don't matter for the weight calcs, so drop them
weight_info <- aim.analysis::weight.gen(pts = points.spdf@data[points.spdf@data[["used"]] != "Oversample", ],
                                        pts.fatefield = "used",
                                        pts.groupfield = "wgtcat",
                                        frame.spdf = wgtcats.spdf,
                                        frame.groupfield = "lut_bpsgro",
                                        target.values = c("Sampled", "SAMPLED"),
                                        unknown.values = c("Unknown", "UNKNOWN"),
                                        nontarget.values = c(NA),
                                        inaccessible.values = c("Rejected", "REJECTED"),
                                        unneeded.values = c("Unsampled", "UNSAMPLED"))

weight_info[["point.weights"]][, "ru"] <- "Zone 6"


# Run the math
analysis <- aim.analysis::analyze(evaluated.points = data_benchmarked,
                                  point.weights = weight_info[["point.weights"]],
                                  points.joinfield = "PlotID",
                                  points.valuefield = "Condition.Category",
                                  points.keyfield = "Indicator",
                                  points.idfield = "PlotID",
                                  points.splitfield = "Reporting.Unit",
                                  points.xcoordfield = "Longitude",
                                  points.ycoordfield = "Latitude",
                                  weights.joinfield = "PLOTID",
                                  weights.weightfield = "WGT",
                                  weights.reportingfield = "ru",
                                  conf = 80)

analysis_cleaned <- do.call(rbind,
                lapply(X = split(analysis, list(analysis[["Indicator"]])),
               FUN = function(X){
                 df <- X[, c("Subpopulation", "Indicator", "Category", "NResp", 
                             "Estimate.P", "StdError.P",
                             "LCB80Pct.W", "UCB80Pct.W",
                             "LCB80Pct.P", "UCB80Pct.P")]
                 blank <- data.frame("Subpopulation" = df[1, "Subpopulation"],
                                     "Indicator" = df[1, "Indicator"],
                                     "Category" = c("Meeting", "Not Meeting"),
                                     "NResp" = 0,
                                     "Estimate.P" = 0,
                                     "StdError.P" = 0,
                                     "LCB80Pct.W" = 0,
                                     "UCB80Pct.W" = 0,
                                     "LCB80Pct.P" = 0,
                                     "UCB80Pct.P" = 0,
                                     stringsAsFactors = FALSE)
                 
                 output <- rbind(df[df[["Category"]] != "Total", ],
                                 blank[!blank[["Category"]] %in% df[["Category"]], ])
                 
                 names(output) <- c("Reporting Unit",
                                    "Indicator",
                                    "Condition Category",
                                    "Number of plots",
                                    "Estimated Percent",
                                    "Standard Error",
                                    "Lower Bound (80% confidence)",
                                    "Upper Bound (80% confidence)",
                                    "Lower Bound (80% confidence; Wilson's binomial)",
                                    "Upper Bound (80% confidence; Wilson's binomial)")
                 
                 return(output)
               }))

aim.analysis::write.analysis(analysis.output = analysis_cleaned,
                             name = "gufo_analysis_20190417",
                             out.path = "C:/Users/Nelson/Documents/Projects/Colorado/GUFO/analysis")

write.csv(analysis_cleaned,
          "gufo_analysis_20190417.csv")

write.csv(weight_info[["point.weights"]],
          "gufo_pointweights_ALLPOINTS_20190417.csv")

library(RODBC)
library(tidyverse)

# Pull in list of plots we want
path <- 'C:\\Users\\alaurencetraynor\\Documents\\GSENM'
file <- "benchmarked_points.csv"

file_path <- paste0(path, "/", file)

pts <- read.csv(file_path)

# connect to aim pub
con1 <- RODBC::odbcConnect(dsn = "AIMPub", rows_at_time = 1)

# Pull in tdat species ind layer
# Im selecting for UT and BRTE to reduce the size
terradat_UT <- RODBC::sqlQuery(con1, "SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TerrestrialIndicators WHERE SpeciesState = 'UT'")

# Filter to just the plots in GSENM
GSENM_data <- terradat_UT[terradat_UT$PrimaryKey %in% pts$PrimaryKey,]

# Filter down to last 5 and last 10 years
# actually lets just add fields for this

GSENM_data$LastFiveYears <- NA

GSENM_data$LastTenYears <- NA

current_date <- Sys.Date()
fiveyearsago <- as.POSIXct(current_date-(365*5))
tenyearsago <- as.POSIXct(current_date-(365*10))

# update datevisited field to be recognised as date time
GSENM_data$DateVisited <- as.POSIXct(GSENM_data$DateVisited)


GSENM_data[["LastFiveYears"]] <- ifelse(GSENM_data[["DateVisited"]] >= fiveyearsago,
                                   GSENM_data[["LastFiveYears"]] <- TRUE,
                                   FALSE)

GSENM_data[["LastTenYears"]] <- ifelse(GSENM_data[["DateVisited"]] >= tenyearsago,
                                        GSENM_data[["LastTenYears"]] <- TRUE,
                                        FALSE)

GSENM_data <- GSENM_data[,-c(156,157)]

write.csv(GSENM_data, "C:\\Users\\alaurencetraynor\\Documents\\GSENM\\gsenm_iirh.csv")

# lets fix some of the ratings
unique(GSENM_data$RH_BioticIntegrity) # this has both M and MO for moderate - update to M, theres also blanks

GSENM_data$RH_BioticIntegrity[GSENM_data$RH_BioticIntegrity == "MO"] <- "M"
GSENM_data$RH_BioticIntegrity[GSENM_data$RH_BioticIntegrity == ""] <- NA

unique(GSENM_data$RH_HydrologicFunction) # this has both M and MO for moderate - update to M, theres also blanks

GSENM_data$RH_HydrologicFunction[GSENM_data$RH_HydrologicFunction == "MO"] <- "M"
GSENM_data$RH_HydrologicFunction[GSENM_data$RH_HydrologicFunction == ""] <- NA

unique(GSENM_data$RH_SoilSiteStability) # this has both M and MO for moderate - update to M, theres also blanks

GSENM_data$RH_SoilSiteStability[GSENM_data$RH_SoilSiteStability == "MO"] <- "M"
GSENM_data$RH_SoilSiteStability[GSENM_data$RH_SoilSiteStability == ""] <- NA

# lets reorder as well
GSENM_data$RH_BioticIntegrity  <- as.factor(GSENM_data$RH_BioticIntegrity)
GSENM_data$RH_BioticIntegrity <- factor(GSENM_data$RH_BioticIntegrity, levels = c("NS", "SM", "M", "ME", "EX") )

GSENM_data$RH_HydrologicFunction  <- as.factor(GSENM_data$RH_HydrologicFunction)
GSENM_data$RH_HydrologicFunction <- factor(GSENM_data$RH_HydrologicFunction, levels = c("NS", "SM", "M", "ME", "EX") )

GSENM_data$RH_SoilSiteStability  <- as.factor(GSENM_data$RH_SoilSiteStability)
GSENM_data$RH_SoilSiteStability <- factor(GSENM_data$RH_SoilSiteStability, levels = c("NS", "SM", "M", "ME", "EX") )

# cut down to five years
five_years <- GSENM_data[GSENM_data$LastFiveYears == TRUE & !is.na(GSENM_data$RH_SoilSiteStability),]
ten_years <- GSENM_data[GSENM_data$LastTenYears == TRUE & !is.na(GSENM_data$RH_SoilSiteStability),]

# lets summarise these
five_year_summary <- five_years %>%
  pivot_longer(cols = c(RH_BioticIntegrity:RH_SoilSiteStability), names_to = "Attribute", values_to = "Rating") %>% 
  group_by(Attribute, Rating) %>% 
  summarise(`Count of Plots` = n()) %>% 
  pivot_wider(names_from = Attribute, values_from = `Count of Plots`)
  
ten_year_summary <- ten_years %>%
  pivot_longer(cols = c(RH_BioticIntegrity:RH_SoilSiteStability), names_to = "Attribute", values_to = "Rating") %>% 
  group_by(Attribute, Rating) %>% 
  summarise(`Count of Plots` = n()) %>% 
  pivot_wider(names_from = Attribute, values_from = `Count of Plots`)

write.csv(five_year_summary, "C:\\Users\\alaurencetraynor\\Documents\\GSENM\\five_year_summary.csv", row.names = FALSE)
write.csv(ten_year_summary, "C:\\Users\\alaurencetraynor\\Documents\\GSENM\\ten_year_summary.csv", row.names = FALSE)

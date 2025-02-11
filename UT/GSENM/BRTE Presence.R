library(RODBC)

# Pull in list of plots we want
path <- 'C:\\Users\\alaurencetraynor\\Documents\\GSENM'
file <- "benchmarked_points.csv"

file_path <- paste0(path, "/", file)

pts <- read.csv(file_path)

# connect to aim pub
con1 <- RODBC::odbcConnect(dsn = "AIMPub", rows_at_time = 1)

# Pull in tdat species ind layer
# Im selecting for UT and BRTE to reduce the size
terradat_species <- RODBC::sqlQuery(con1, "SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TerrADatSpeciesIndicators WHERE State = 'UT' AND Species = 'BRTE';")

# Filter to just the plots in GSENM
GSENM_species <- terradat_species[terradat_species$PrimaryKey %in% pts$PrimaryKey,]

GSENM_species$BRTE_Present <- TRUE

# Join back to points layer
out_table <- merge(x = pts,
                   y = GSENM_species[,c("PrimaryKey", "AH_SpeciesCover", "Hgt_Species_Avg", "BRTE_Present")],
                   all.x = TRUE,
                   by = "PrimaryKey")

write.csv(x = out_table, file = paste0(path, "/", "BRTE_presence.csv"))
                   

# BIOCRUST CALCS FOR VALE DO

library(tidyverse)
library(RODBC)

devtools::install_github('Landscape-Data-Commons/terradactyl')
library(terradactyl)

#Create connection
conn <- RODBC::odbcConnect(dsn = "AIMPub", rows_at_time = 1)

ws <- "C:\\Users\\alaurencetraynor\\Documents\\2022\\Analysis\\ORWA"

# Import list of plots from jennifers spreadsheets

ecoregion1_aim <- readxl::read_xlsx(paste0(ws, "\\", "AnnualGrasses_Lessthan5_LevelIVEcoregion_80a_ReviewforReference_ColoredIndicators.xlsx"))

ecoregion2_aim <- readxl::read_xlsx(paste0(ws, "\\", "AnnualGrasses_Lessthan5_LevelIVEcoregion_80f_ReviewforReference_ColoredIndicators.xlsx"))

ecoregion1_lmf <- readxl::read_xlsx(paste0(ws, "\\", "AnnualGrasses_Lessthan5_LevelIVEcoregion_80a_ReviewforReference_ColoredIndicators.xlsx"), sheet = "LMF")

ecoregion2_lmf <- readxl::read_xlsx(paste0(ws, "\\", "AnnualGrasses_Lessthan5_LevelIVEcoregion_80f_ReviewforReference_ColoredIndicators.xlsx"), sheet = "LMF")

plotlist <- c(ecoregion1_aim$PrimaryKey, ecoregion2_aim$PrimaryKey,ecoregion1_lmf$PrimaryKey, ecoregion2_lmf$PrimaryKey)

# Add all of Terradat (this is the combine layer)
terradat <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMPubDBO.TerrestrialIndicators;')

# filter to points in 80a and 80f from jennifers spreadsheet
terradat_orwa <- terradat[terradat$PrimaryKey %in% plotlist,]

# grab LPI detail from terradat
terradat_lpi <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TBLLPIDETAIL;')

terradat_lpi_orwa <- terradat_lpi[terradat_lpi$PrimaryKey %in% terradat_orwa$PrimaryKey,]

# grab tbl lines
tbllines <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TBLLINES;')

tbllpiheader <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TBLLPIHEADER;')

tbllpiheader <- tbllpiheader[,c("LineKey", "RecKey")]



# grab LPI detail from lmf
lmf_lpi <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.PINTERCEPT;')
lmf_lpi_orwa <- lmf_lpi[lmf_lpi$PrimaryKey %in% terradat_orwa$PrimaryKey,]

tdat_tall <- tidyr::pivot_longer(data = terradat_lpi_orwa,
                    cols = c(TopCanopy:SoilSurface),
                    names_to = "layer",
                    values_to = "code")

tdat_tall <- merge(x = tdat_tall,
                   y = tbllpiheader,
                   by = "RecKey",
                   all.x = T,
                   all.y = F)

tdat_tall <- tdat_tall[,c("PrimaryKey", "layer", "code", "PointNbr", "LineKey")]

tdat_tall$bsc <- "NotBSC"
tdat_tall[tdat_tall$code %in% c("M", "LC","CY"), "bsc"] <- "BSC" # do these make sense? excludng VL for now

aim_cover_bsc <- terradactyl::pct_cover(lpi_tall = tdat_tall,
                                    tall = F,
                                    hit = "any",
                                    by_line = F,
                                    bsc)
hist(aim_cover_bsc$BSC)
summary(aim_cover_bsc)


# now the same for LPI

lmf_tall <- terradactyl::gather_lpi_lmf(PINTERCEPT = lmf_lpi_orwa)

lmf_tall$bsc <- "NotBSC"
lmf_tall[lmf_tall$code %in% c("M", "LC","CY"), "bsc"] <- "BSC" # do these make sense? excludng VL for now

lmf_cover_bsc <- terradactyl::pct_cover(lpi_tall = lmf_tall,
                                    tall = F,
                                    hit = "any",
                                    by_line = F,
                                    bsc)

hist(lmf_cover_bsc$BSC)
summary(lmf_cover_bsc)

## Join and Export
ecoregion1_aim <- merge(x = ecoregion1_aim,
                        y = aim_cover_bsc,
                        by = "PrimaryKey",
                        all.x = T,
                        all.y = F)

write.csv(ecoregion1_aim, "C:\\Users\\alaurencetraynor\\Documents\\2022\\Analysis\\ORWA\\BSC_cover_80a_aim.csv")

ecoregion2_aim <- merge(x = ecoregion2_aim,
                        y = aim_cover_bsc,
                        by = "PrimaryKey",
                        all.x = T,
                        all.y = F)

write.csv(ecoregion2_aim, "C:\\Users\\alaurencetraynor\\Documents\\2022\\Analysis\\ORWA\\BSC_cover_80f_aim.csv")

ecoregion1_lmf <- merge(x = ecoregion1_lmf,
                        y = lmf_cover_bsc,
                        by = "PrimaryKey",
                        all.x = T,
                        all.y = F)

write.csv(ecoregion1_lmf, "C:\\Users\\alaurencetraynor\\Documents\\2022\\Analysis\\ORWA\\BSC_cover_80a_lmf.csv")

ecoregion2_lmf <- merge(x = ecoregion2_lmf,
                        y = lmf_cover_bsc,
                        by = "PrimaryKey",
                        all.x = T,
                        all.y = F)

write.csv(ecoregion2_lmf, "C:\\Users\\alaurencetraynor\\Documents\\2022\\Analysis\\ORWA\\BSC_cover_80f_lmf.csv")

RODBC::odbcCloseAll()

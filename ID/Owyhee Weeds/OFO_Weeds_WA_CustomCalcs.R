# BIOCRUST CALCS FOR VALE DO

library(tidyverse)
library(RODBC)

#devtools::install_github('Landscape-Data-Commons/terradactyl')
library(terradactyl)

#Create connection
conn<-NA
conn <- RODBC::odbcConnect(dsn = "AIMPub", rows_at_time = 1)

ws <- "\\\\ilmidso3ds1.blm.doi.net\\so\\users\\jenelson\\My Documents\\AIM\\AnalysisRequests\\Weeds"

# Import list of plots from Benchmark Tool

Owyhee.aim.lmf <- readxl::read_xlsx(paste0(ws, "\\IdahoWeeds_112022\\", "OFO_Weeds_WA_AIM_points_TableToExcel.xlsx"))

plotlist <- Owyhee.aim.lmf$PrimaryKey

# Add all of Terradat (this is the combined AIM and LMF layer)
# although will need to split them later for calcs since LMF is wierd
terradat <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMPubDBO.TerrestrialIndicators;')

# filter to points in 80a and 80f from jennifers spreadsheet
terradat_OFO <- terradat[terradat$PrimaryKey %in% plotlist,]

# grab LPI detail from terradat
terradat_lpi <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TBLLPIDETAIL;')

terradat_lpi_OFO <- terradat_lpi[terradat_lpi$PrimaryKey %in% terradat_OFO$PrimaryKey,]

# grab tbl lines
tbllines <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TBLLINES;')

tbllpiheader <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TBLLPIHEADER;')

tbllpiheader <- tbllpiheader[,c("LineKey", "RecKey")]

# grab LPI detail from lmf
lmf_lpi <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.PINTERCEPT;')
lmf_lpi_OFO <- lmf_lpi[lmf_lpi$PrimaryKey %in% terradat_OFO$PrimaryKey,]

tdat_tall <- tidyr::pivot_longer(data = terradat_lpi_OFO,
                                 cols = c(TopCanopy:SoilSurface),
                                 names_to = "layer",
                                 values_to = "code")

tdat_tall <- merge(x = tdat_tall,
                   y = tbllpiheader,
                   by = "RecKey",
                   all.x = T,
                   all.y = F)

tdat_tall <- tdat_tall[,c("PrimaryKey", "layer", "code", "PointNbr", "LineKey")]

#Calculate custom indicators
tdat_tall$IAG <- "NotIAG"
tdat_tall$BRTE <- "NotBRTE"
tdat_tall$TACA8 <- "NotTACA8"
tdat_tall$VEDUVENTE <- "NotVEDUVENTE"
tdat_tall$AECY <- "NotAECY"
tdat_tall[tdat_tall$code %in% c("BRTE", "TACA8", "VEDU", "VENTE", "AECY"), "IAG"] <- "IAG" 
tdat_tall[tdat_tall$code %in% c("BRTE"), "BRTE"] <- "BRTE"
tdat_tall[tdat_tall$code %in% c("TACA8"), "TACA8"] <- "TACA8"
tdat_tall[tdat_tall$code %in% c("VEDU", "VENTE"), "VEDUVENTE"] <- "VEDUVENTE"
tdat_tall[tdat_tall$code %in% c("AECY"), "AECY"] <- "AECY"

aim_cover_IAG <- terradactyl::pct_cover(lpi_tall = tdat_tall,
                                        tall = F,
                                        hit = "any",
                                        by_line = F,
                                        IAG)

hist(aim_cover_IAG$IAG)
summary(aim_cover_IAG)

aim_cover_BRTE <- terradactyl::pct_cover(lpi_tall = tdat_tall,
                                        tall = F,
                                        hit = "any",
                                        by_line = F,
                                        BRTE)

hist(aim_cover_BRTE$BRTE)
summary(aim_cover_BRTE)

aim_cover_TACA8 <- terradactyl::pct_cover(lpi_tall = tdat_tall,
                                        tall = F,
                                        hit = "any",
                                        by_line = F,
                                        TACA8)

hist(aim_cover_TACA8$TACA8)
summary(aim_cover_TACA8)

aim_cover_VEDUVENTE <- terradactyl::pct_cover(lpi_tall = tdat_tall,
                                        tall = F,
                                        hit = "any",
                                        by_line = F,
                                        VEDUVENTE)

hist(aim_cover_VEDUVENTE$VEDUVENTE)
summary(aim_cover_VEDUVENTE)

aim_cover_AECY <- terradactyl::pct_cover(lpi_tall = tdat_tall,
                                        tall = F,
                                        hit = "any",
                                        by_line = F,
                                        AECY)
hist(aim_cover_AECY$AECY)
summary(aim_cover_AECY)
aim_cover_AECY$AECY<-rep(0, nrow(aim_cover_AECY))

# now the same for LPI

lmf_tall <- terradactyl::gather_lpi_lmf(PINTERCEPT = lmf_lpi_OFO)

lmf_tall$IAG <- "NotIAG"
lmf_tall$BRTE <- "NotBRTE"
lmf_tall$TACA8 <- "NotTACA8"
lmf_tall$VEDUVENTE <- "NotVEDUVENTE"
lmf_tall$AECY <- "NotAECY"
lmf_tall[lmf_tall$code %in% c("BRTE", "TACA8", "VEDU", "VENTE", "AECY"), "IAG"] <- "IAG" 
lmf_tall[lmf_tall$code %in% c("BRTE"), "BRTE"] <- "BRTE"
lmf_tall[lmf_tall$code %in% c("TACA8"), "TACA8"] <- "TACA8"
lmf_tall[lmf_tall$code %in% c("VEDU", "VENTE"), "VEDUVENTE"] <- "VEDUVENTE"
lmf_tall[lmf_tall$code %in% c("AECY"), "AECY"] <- "AECY"


lmf_cover_IAG <- terradactyl::pct_cover(lpi_tall = lmf_tall,
                                        tall = F,
                                        hit = "any",
                                        by_line = F,
                                        IAG)

hist(lmf_cover_IAG$IAG)
summary(lmf_cover_IAG)

lmf_cover_BRTE <- terradactyl::pct_cover(lpi_tall = lmf_tall,
                                         tall = F,
                                         hit = "any",
                                         by_line = F,
                                         BRTE)

hist(lmf_cover_BRTE$BRTE)
summary(lmf_cover_BRTE)

lmf_cover_TACA8 <- terradactyl::pct_cover(lpi_tall = lmf_tall,
                                          tall = F,
                                          hit = "any",
                                          by_line = F,
                                          TACA8)

hist(lmf_cover_TACA8$TACA8)
summary(lmf_cover_TACA8)

lmf_cover_VEDUVENTE <- terradactyl::pct_cover(lpi_tall = lmf_tall,
                                              tall = F,
                                              hit = "any",
                                              by_line = F,
                                              VEDUVENTE)

lmf_cover_VEDUVENTE$VEDUVENTE<-rep(0, nrow(lmf_cover_VEDUVENTE))
hist(lmf_cover_VEDUVENTE$VEDUVENTE)
summary(lmf_cover_VEDUVENTE)


lmf_cover_AECY <- terradactyl::pct_cover(lpi_tall = lmf_tall,
                                         tall = F,
                                         hit = "any",
                                         by_line = F,
                                         AECY)

lmf_cover_AECY$AECY<-rep(0, nrow(lmf_cover_AECY))
hist(lmf_cover_AECY$AECY)
summary(lmf_cover_AECY)


# just gonna throw this into a single sheet

OFO_lmf<-cbind(lmf_cover_IAG[,c("PrimaryKey", "IAG")], lmf_cover_BRTE$BRTE, lmf_cover_TACA8$TACA8, lmf_cover_VEDUVENTE$VEDUVENTE, lmf_cover_AECY$AECY)
colnames(OFO_lmf)<-c("PrimaryKey", "AH_IAGCover", "AH_BRTECover", "AH_TACA8Cover", "AH_VEDUVENTECover", "AH_AECYCover")

OFO_aim <- cbind(aim_cover_IAG[,c("PrimaryKey", "IAG")], aim_cover_BRTE$BRTE, aim_cover_TACA8$TACA8, aim_cover_VEDUVENTE$VEDUVENTE, aim_cover_AECY$AECY)
colnames(OFO_aim)<-c("PrimaryKey", "AH_IAGCover", "AH_BRTECover", "AH_TACA8Cover", "AH_VEDUVENTECover", "AH_AECYCover")

OFO_aim_lmf<-rbind(OFO_aim, OFO_lmf)

#Merge with original data
Owyhee.aim.lmf.customcalcs <- merge(x = Owyhee.aim.lmf,
                                    y = OFO_aim_lmf,
                                    by = "PrimaryKey",
                                    all.x = TRUE,
                                    all.y = FALSE)


# getting an error from spatial columns when trying to write to csv
all_final <- apply(all_final,2,as.character)
write.csv(Owyhee.aim.lmf.customcalcs, paste0(ws, "\\IdahoWeeds_112022\\", "OFO_Weeds_WA_AIM_points_customcalcs.csv"))




RODBC::odbcCloseAll()


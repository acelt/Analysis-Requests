#### AIM data access using arcgisbinding package example script ####

### For this script to run you will need:
# 1) To be connected to the BLM internal VPN to access AIM data
# 2) To be running a 64 bit version of R (the script was tested using [64 bit] R 4.0.3)

#### More information on the RODBC package here: https://cran.r-project.org/web/packages/RODBC/RODBC.pdf

### BEFORE RUNNING THIS SCIRPT YOU NEED TO HAVE SET UP AN ODBC CONNECTION TO THE AIM SDE

library(tidyverse)
library(RODBC)
library(sf)

#Create connection
conn <- RODBC::odbcConnect(dsn = "AIMPub", rows_at_time = 1)

RODBC::sqlTables(conn)

soil_pits <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TBLSOILPITS;' )
horizons <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO. TBLSOILPITHORIZONS;' )

all_plots <- sf::st_read(dsn = "C:\\Users\\alaurencetraynor\\Documents\\OR\\ValeDO\\ValeDO.gdb", layer = "All_AIMLMF_80a80f_Ecoregion")

claa_plots <- sf::st_read(dsn = "C:\\Users\\alaurencetraynor\\Documents\\OR\\ValeDO\\ValeDO.gdb", layer = "AIMandLMFplots_pastures")


soil_pit_join <- merge(x = all_plots,
                       y = soil_pits,
                       by = "PrimaryKey",
                       all.x = T,
                       all.y = F)

# filter to just first horizon for now
horizons <- horizons[horizons$HorizonDepthUpper == 0,]

horizon_join <- merge(x = soil_pit_join,
                      y = horizons,
                      by = "PrimaryKey",
                      all.x = T,
                      all.y = F)
summary(horizon_join$Texture)

# remove NAs
horizon_join <- horizon_join[!is.na(horizon_join$Texture) & horizon_join$Texture != "bed" & horizon_join$Texture != "SI",] # theres no way theres silt here

# tidying categories for figure
horizon_join$Texture <- gsub("FSL", "SL", horizon_join$Texture)
horizon_join$Texture <- gsub("VSL", "SL", horizon_join$Texture)
horizon_join$US_L4NAME <- gsub( "80a. Dissected High Lava Plateau", "Ecoregion 80a.", horizon_join$US_L4NAME)
horizon_join$US_L4NAME <- gsub("80f. Owyhee Uplands and Canyons","Ecoregion 80f.", horizon_join$US_L4NAME)


# classify these based on ribbon length
horizon_join <- horizon_join %>% 
  mutate(TextureClass = ifelse(Texture == "C"|Texture == "SIC"|Texture == "SC","Clays",
                               ifelse(Texture == "CL"|Texture == "SICL"|Texture == "SCL","Clay Loams",
                                      ifelse(Texture == "L"|Texture == "SIL"|Texture == "SL", "Loams",
                                             ifelse(Texture == "LS"|Texture == "S", "Sands",NA)))))

# reorder
horizon_join$TextureClass <- as.factor(horizon_join$TextureClass)
horizon_join$TextureClass <- factor(horizon_join$TextureClass, levels = c("Clays","Clay Loams","Loams","Sands"))
horizon_join$Texture <- as.factor(horizon_join$Texture)
horizon_join$Texture <- factor(horizon_join$Texture, levels = c("C","SIC","SC","CL","SICL","SCL","L","SIL","SL","LS","S"))

# define color pallete

#plot
ggplot(data = horizon_join, aes(x = TextureClass, fill = Texture))+
  geom_bar(position = position_dodge2(preserve = "single", width = 0.7), alpha = 0.9)+
  facet_wrap(.~US_L4NAME)+
  theme_bw(base_size = 14)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1 ))+
  scale_fill_manual(values = c("#DEEBF7", "#9ECAE1", "#3182BD",
                               "#FEE6CE", "#FDAE6B", "#E6550D",
                               "#EFEDF5", "#BCBDDC", "#756BB1",
                               "#E5F5E0","#31A354"))+
  xlab(label = "Soil Texture Class in Surface Horizon")+
  ylab(label = "Count of AIM Plots")

ggsave("soils_figure.jpeg", device = "jpeg", dpi = 300, path = 'C:\\Users\\alaurencetraynor\\Documents\\OR\\ValeDO', width = 8)



# export to csv as well
write.csv(horizon_join, file = "C:\\Users\\alaurencetraynor\\Documents\\OR\\ValeDOsoils_data.csv")


# Close the connection
RODBC::odbcCloseAll()

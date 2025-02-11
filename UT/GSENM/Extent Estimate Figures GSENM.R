## Title: National Reporting Figures
## Author: Alex T
## Date: 9th March, 2022



########################### FUNCTIONS ####################################################################
\
#' Create Figures for extent estimates from spsurvey outputs
#' @description Code to plot extent estimate figures from spsurvey output. Uses ggplot. Requires a tidy data set with each row as a unique observation/estimate and each column as a variable.
#' @param data_path A string describing the file path to an excel file containing the outputs of spsurvey's weighted analysis function.
#' @param cat_field A string for the column name which contains the condition categories. Defaults to "Category"
#' @param lower_ci A string for the column name which contains the lower bound of the confidence interval within the data specified by data_path. Defaults to "LCB90Pct.P"
#' @param upper_ci A string for the column name which contains the upper bound of the confidence interval within the data specified by data_path. Defaults to "UCB90Pct.P"
#' @param ru_field A string for the column name which contains the reporting unit within the data specified by data_path. Defaults to "Subpopulation"
#' @param ind_field A string for the column name which contains the indicator within the data specified by data_path. Defaults to "Indicator"
#' @param est_field A string for the column name which contains the estimate value within the data specified by data_path. This could be the estimated percent of the reporting unit with each condition class or the actual area. Defaults to "Estimate.P".
#' @param palette A string describing the palette choice for the plot. This currently has three options:"viridis", "magma", and "brewer". Defaults to "viridis"
#' @param cat_order A vector of strings specifying the desired order of condition categories for display in the figure.
#' @param theme A string describing the ggplot2 theme type that will be used. Options include: "classic", "minimal", and "bw". Defaults to "bw".
#' @param plot_type Specifies which plot type to use. Options in include: "facet", "all indicators", and "individual"
#' @param output_dir Output directory for exported graphics. Defaults to current working directory.

make_plot <- function(data_path,
                      cat_field = "Category",
                      lower_ci = "LCB90Pct.P",
                      upper_ci = "UCB90Pct.P",
                      ru_field ="Subpopulation" ,
                      ind_field = "Indicator",
                      est_field = "Estimate.P",
                      palette = "viridis",
                      cat_order = c("Good", "Fair", "Poor", "NoData"),
                      theme = "bw",
                      plot_type,
                      output_dir = getwd()){
  require(tidyverse)
  require(readxl)
  require(viridis)
  
  # read in data and select columns
  data <-  readxl::read_excel(path = data_path)
  
  # Need to reorder factors in condition category field.
  data[[cat_field]] <- factor(data[[cat_field]], levels = cat_order)
  
  # set width for point and errorbar dodge
  dodge1 <- position_dodge(width = 0.5)
  
  # plot data
  
  # we have perhaps 3 options for plot layout: 1) looping through each reporting unit and faceting by indicator, 
  # 2) looping through each reporting unit and plotting indicator on the y axis or 
  # 3) looping through both reporting unit and indicatorto create a single plot for each incicator/ru combo
  if(plot_type == "facet"){
    for(i in unique(data[[ru_field]])){ # in this case looping through unique reporting units - we could also loop through indicator as well if we wanted a single plot for each
      
      plot_data <- subset(data, data[[ru_field]]==i & data[[cat_field]] %in% cat_order)
      
      p <- ggplot(data = plot_data, aes(x = .data[[est_field]], y = .data[[cat_field]], col = .data[[cat_field]])) +
        geom_point(size =4, shape = 18, position = dodge1)+
        facet_wrap(.~.data[[ind_field]])+ # we could either facet by indicator or plot indicator on the y axis
        geom_errorbar(width =0.5 ,aes(xmin = .data[[lower_ci]], xmax = .data[[upper_ci]]), position = dodge1)+
        labs(x = "Estimated Percent of Area (%)", y = "Condition Category") # could parameterise these too if needed
      
      # testing out a few different themes/ color palettes. Will probably need to decide on one of these
      if(palette == "viridis"){
        p <- p + viridis::scale_color_viridis(discrete = TRUE)
      } 
      if(palette == "magma"){
        p <- p + viridis::scale_color_viridis(option="magma", discrete = TRUE)
      } 
      
      if(palette == "brewer"){
        p <- p + scale_color_brewer(type = "div", palette = 3)
      }
      
      if(theme == "classic"){
        p <- p + theme_classic(base_size = 20, base_family = "serif")
      }
      
      if(theme == "minimal"){
        p <- p + theme_minimal(base_size = 20, base_family = "serif")
      }
      if(theme == "bw"){
        p <- p + theme_bw(base_size = 20, base_family = "serif")
      }
      # export plot
      ggsave(plot = p,
             filename = paste0(i, "_plot_", palette, "_",theme, ".jpeg"),
             device = "jpeg",
             width = 2*length(unique(data[[ind_field]])), # making this wider for the facetting
             height = 10,
             path = output_dir,
             dpi = 400)
  }
  
  }
  
  if(plot_type == "all indicators"){
    for(i in unique(data[[ru_field]])){ # in this case looping through unique reporting units - we could also loop through indicator as well if we wanted a single plot for each
      
      plot_data <- subset(data, data[[ru_field]]==i & data[[cat_field]] %in% cat_order)
      
      p <- ggplot(data = plot_data, aes(x = .data[[est_field]], y = .data[[ind_field]], col = .data[[cat_field]])) +
        geom_point(size =4, shape = 18)+
        geom_errorbar(width =0.5 ,aes(xmin = .data[[lower_ci]], xmax = .data[[upper_ci]]))+
        labs(x = "Estimated Percent of Area (%)", y = "Indicator") # could parameterise these too if needed
      
      # testing out a few different themes/ color palettes. Will probably need to decide on one of these
      if(palette == "viridis"){
        p <- p + viridis::scale_color_viridis(discrete = TRUE)
      } 
      if(palette == "magma"){
        p <- p + viridis::scale_color_viridis(option="magma", discrete = TRUE)
      } 
      
      if(palette == "brewer"){
        p <- p + scale_color_brewer(type = "div", palette = 3)
      }
      
      if(theme == "classic"){
        p <- p + theme_classic(base_size = 20, base_family = "serif")
      }
      
      if(theme == "minimal"){
        p <- p + theme_minimal(base_size = 20, base_family = "serif")
      }
      if(theme == "bw"){
        p <- p + theme_bw(base_size = 20, base_family = "serif")
      }
      # export plot
      ggsave(plot = p,
             filename = paste0(i, "_plot_", palette, "_",theme, ".jpeg"),
             device = "jpeg",
             width = 10,
             height = 10,
             path = output_dir,
             dpi = 400)
  }
 
  

  }
  if(plot_type == "individual"){
    
    for(i in unique(data[[ru_field]])){ # in this case looping through unique reporting units - we could also loop through indicator as well if we wanted a single plot for each
      for(j in unique(data[[ind_field]])){
        
        plot_data <- subset(data, data[[ru_field]] == i & data[[cat_field]] %in% cat_order & data[[ind_field]] == j)
        
        p <- ggplot(data = plot_data, aes(x = .data[[est_field]], y = .data[[cat_field]], col = .data[[cat_field]])) +
          geom_point(size =4, shape = 18, position = dodge1)+
          #facet_wrap(.~.data[[ind_field]])+ # we could either facet by indicator or plot indicator on the y axis
          geom_errorbar(width =0.5 ,aes(xmin = .data[[lower_ci]], xmax = .data[[upper_ci]]), position = dodge1)+
          labs(x = "Estimated Percent of Area (%)", y = "Condition Category") # could parameterise these too if needed
        
        # testing out a few different themes/ color palettes. Will probably need to decide on one of these
        if(palette == "viridis"){
          p <- p + viridis::scale_color_viridis(discrete = TRUE)
        } 
        if(palette == "magma"){
          p <- p + viridis::scale_color_viridis(option="magma", discrete = TRUE)
        } 
        
        if(palette == "brewer"){
          p <- p + scale_color_brewer(type = "div", palette = 3)
        }
        
        if(theme == "classic"){
          p <- p + theme_classic(base_size = 20, base_family = "serif")
        }
        
        if(theme == "minimal"){
          p <- p + theme_minimal(base_size = 20, base_family = "serif")
        }
        if(theme == "bw"){
          p <- p + theme_bw(base_size = 20, base_family = "serif")
        }
        # export plot
        ggsave(plot = p,
               filename = paste0(i, "_plot_", palette, "_",theme, "_", j, ".jpeg"),
               device = "jpeg",
               width = 10,
               height = 5,
               path = output_dir,
               dpi = 400)
        
      }
      
  }

  }
  
}


############################### SETUP #########################################################
library(tidyverse)
library(sf)
library(readxl)
library(spsurvey)
library(viridis)

# Where ill save outputs
out_dir <- "C:\\Users\\alaurencetraynor\\Documents\\2022\\Analysis\\UT\\GSENM"

# This is where the benchmark results are saved
in_dir <- "\\blm\\dfs\\loc\\EGIS\\ProjectsNational\\AIM\\Data\\Development\\Reports\\Benchmark\\Test Outputs\\GSENM"

results_table <-"benchmarked_points.xlsx"

################################## IMPORT DATA ################################################
data <- read_xlsx(path = paste0(out_dir,"\\",results_table),
          sheet = "Raw Data")

library(RODBC)

#Create connection
conn <- RODBC::odbcConnect(dsn = "AIMPub", rows_at_time = 1)

# View the available tables
RODBC::sqlTables(conn)

#Add combined terrestrial (terradat+lmf) layer
spec_ind <- RODBC::sqlQuery(conn, 'SELECT * FROM ilmocAIMTerrestrialPub.ILMOCAIMPUBDBO.TerrestrialSpecies;') # The '*' here means retrieve everything from this table

# filter to GSENM

spec_ind <- spec_ind[spec_ind$PrimaryKey %in% data$PrimaryKey,]

##################################### ANALYZE ########################################

## Looking at perennial grass forb cover
summary(data$AH_NonNoxPerenForbGrassCover)

# Add field
data$PerennialGrassForb <- NA

# break this into <1, 1-5% and >5 groups
data[data$AH_NonNoxPerenForbGrassCover <1, "PerennialGrassForb"] <- "Unsuitable"
data[data$AH_NonNoxPerenForbGrassCover >= 1 & data$AH_NonNoxPerenForbGrassCover <= 5 , "PerennialGrassForb"] <- "Marginal"
data[data$AH_NonNoxPerenForbGrassCover >5, "PerennialGrassForb"] <- "Suitable"

summary(as.factor(data$PerennialGrassForb))

# looking at BRTE cover
brte_cover <- spec_ind[spec_ind$Species == "BRTE",c("PrimaryKey", "AH_SpeciesCover")]

# merge with data
data <- merge(x = data,
              y = brte_cover,
              all = TRUE,
              by = 'PrimaryKey')

# Add brte benchmark field
data$Cheatgrass <- NA

# NAs in AH-Species cover are really 0%
data[is.na(data$AH_SpeciesCover),"AH_SpeciesCover"] <- 0

# split up by benchmark <1, 1-5% and >5 groups
data[data$AH_SpeciesCover <1, "Cheatgrass"] <- "Suitable"
data[data$AH_SpeciesCover >= 1 & data$AH_SpeciesCover <= 5 , "Cheatgrass"] <- "Marginal"
data[data$AH_SpeciesCover >5, "Cheatgrass"] <- "Unsuitable"
  
summary(as.factor(data$Cheatgrass))
##################################### PLOT #########################################################


# these are the indicators from teh benchmarks 
indicators <- c('AH_NoxAnnGrassCover',"AH_SagebrushCover","AH_NoxCover", "BareSoilCover", "GapCover_200_plus")

data_subset <- data[,c("PrimaryKey","Benchmark Group","DateVisited","EcologicalSiteId","Latitude_NAD83","Longitude_NAD83", indicators)]

# Maps
ggplot(data = data_subset, aes(x = Longitude_NAD83, y = Latitude_NAD83, size = AH_SagebrushCover, col = `Benchmark Group`))+
  geom_point()+
  theme_void()+
  theme(legend.position = "bottom")

# BOXPLOTS
ggplot(data = data_subset, aes(y = AH_SagebrushCover, x = `Benchmark Group`, fill = `Benchmark Group`))+
  geom_hline(yintercept=5, linetype="dashed", 
             color = "red", size=0.8)+
  geom_hline(yintercept=10,
             color = "red", size=0.8)+
  geom_boxplot(alpha = 0.5, outlier.shape = NA)+
  geom_jitter(alpha = 0.5, aes(col = `Benchmark Group`))+
  theme_bw(base_size = 16)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust= 1))+
  labs(y = "Any Hit Sagebrush Cover (%)")

# counts of plots meeting objectives
# lets look at everything together

data_objectives <- pivot_longer(data = data, cols = c("Fire risk", "Wind erosion", "Water erosion", "Invasive abundance","Soil erosion", "Invasive presence","Native Plants - Sagebrush"), names_to = "Objective", values_to = "Condition Category")

# data_objectives <- data_objectives %>%
#   group_by(Objective)%>%
#   summarise()
data_objectives$`Condition Category` <- as.factor(data_objectives$`Condition Category`)
levels(data_objectives$`Condition Category`)
# REORDER FOR LEGEND
data_objectives$`Condition Category` <- factor(data_objectives$`Condition Category`, levels = c("Meeting", "Not Meeting", "Suitable", "Marginal", "Unsuitable","Minimal Departure"))

ggplot(data = data_objectives, aes(x = Objective, ..count..))+
  geom_bar(aes(fill = `Condition Category`))+
  theme_bw(base_size = 16)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust= 1))+
  scale_fill_viridis(option = "viridis", discrete = TRUE)+
  labs(y = "Count of AIM/LMF plots")


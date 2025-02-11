
# Setup
library(tidyverse)
library(gt)
library(sf)
library(arcgisbinding)
arc.check_product()

setwd("C:\\Users\\alaurencetraynor\\Documents\\OMDPNM")

benchmarked_points <-read.csv("benchmarked_points_r.csv")

all_data <- read.csv("values.csv")

usnvc_lut <- arc.select(arc.open("U:\\My Documents\\Analysis\\LCDO\\omdpnm\\omdpnm.gdb\\AIM_usnvc"))

# merge to bring in groups from patrick
benchmarked_points <- merge(x = benchmarked_points,
                            y = usnvc_lut[,c("PrimaryKey","usnvcgrp")],
                            by = "PrimaryKey")

# make elevation field prettier
benchmarked_points <- benchmarked_points %>%
  mutate(HighElevation = ifelse(HighElevation == "1", "High Elevation (>5,250 feet)", "Low Elevation (<5,250 feet)"),
         Group_ = usnvcgrp) %>% 
  select(-usnvcgrp) %>% 
  filter(!is.na(Group_),
         !is.na(HighElevation))
  

ind_summary <- lapply(X = split(benchmarked_points, benchmarked_points$indicator),
                      FUN = function(benchmarked_points){
                        group_summary <- benchmarked_points %>%
                          group_by(HighElevation,Group_, Condition.Category) %>%
                          summarise(PlotCount = n())
                      }
)

# make pretty summary tables

col_list <- c("Meeting",
              "Not Meeting",
              "Non Natives Absent",
              "75-100% Native",
              "50-75% Native",
              "<50% Native",
              "0-5% Cover",
              "5-10% Cover",
              "10-100% Cover",
              "0-5% Shrubs",
              "5-15% Shrubs",
              "15-30% Shrubs",
              ">30% Shrubs")

# make a pretty table
pretty_table <- function(data,VariableName){
  
  # create nice order
  cols_order <- unlist(lapply(unique(data$HighElevation), function(x) paste(x, col_list, sep = "_")))
  
  table <- data %>% 
    pivot_wider(names_from = c(HighElevation,Condition.Category),
                values_from = PlotCount,
                names_glue = "{HighElevation}_{Condition.Category}")%>% # need to order variables better as well
    ungroup()
  
  cols_order <- cols_order[cols_order %in% colnames(table)] # make subset
  
  table <- table %>% 
    select(c("Group_", cols_order)) %>% 
    gt(rowname_col = "Group_") %>% 
    tab_spanner_delim(delim = "_") %>% 
    tab_header(title = "AIM/LMF Plot Counting Summary") %>% 
    tab_stubhead(label = "USNVC Group")
  return(table)
}

table_list <- lapply(X = ind_summary,
                     FUN = pretty_table
)


# do the same for macro groups

mac_ind_summary <- lapply(X = split(benchmarked_points, benchmarked_points$indicator),
                          FUN = function(benchmarked_points){
                            group_summary <- benchmarked_points %>%
                              group_by(HighElevation,Macrogro_1, Condition.Category) %>%
                              summarise(PlotCount = n())
                          }
)

# make a pretty table
mac_pretty_table <- function(data,VariableName){
  
  # create nice order
  cols_order <- unlist(lapply(unique(data$HighElevation), function(x) paste(x, col_list, sep = "_")))
  
  table <- data %>% 
    pivot_wider(names_from = c(HighElevation,Condition.Category),
                values_from = PlotCount,
                names_glue = "{HighElevation}_{Condition.Category}")%>% # need to order variables better as well
    ungroup()
  
  cols_order <- cols_order[cols_order %in% colnames(table)] # make subset
  
  table <- table %>% 
    select(c("Macrogro_1", cols_order)) %>% 
    gt(rowname_col = "Macrogro_1") %>% 
    tab_spanner_delim(delim = "_") %>% 
    tab_header(title = "AIM/LMF Plot Counting Summary") %>% 
    tab_stubhead(label = "USNVC Macrogroup")
  return(table)
}

mac_table_list <- lapply(X = mac_ind_summary,
                         FUN = mac_pretty_table
)
# make tables of actual continuous data

# merge to get groups
# can also use values.csv
cont_data <- all_data %>% 
  pivot_longer(cols = 2:12,
               names_to = "indicator",
               values_to = "value")

group_lut <- unique(benchmarked_points[,c("PrimaryKey","Macrogro_1","Group_","HighElevation")])

cont_data <- merge(x = cont_data,
                   y = group_lut,
                   by = "PrimaryKey",
                   all.x = TRUE,
                   all.y = FALSE) %>% 
  filter(!is.na(Group_),
         !is.na(HighElevation))


cont_summary <- lapply(X = split(cont_data, cont_data$indicator),
                       FUN = function(data){
                         group_summary <- data %>%
                           group_by(HighElevation,Group_) %>%
                           summarise(Mean = mean(value),
                                     StDev = sd(value),
                                     Count = n()) %>% 
                           pivot_longer(cols = c(Mean,StDev,Count), names_to = "stat", values_to = "value") %>% 
                           ungroup()
                       }
)

# make pretty tables
col_list <- c("Mean","StDev","Count")

pretty_cont_table <- function(data){
  
  # create nice order
  cols_order <- unlist(lapply(unique(data$HighElevation), function(x) paste(x, col_list, sep = "_")))
  
  table <- data %>% 
    pivot_wider(names_from = c(HighElevation,stat),
                values_from = value,
                names_glue = "{HighElevation}_{stat}")%>% # need to order variables better as well
    ungroup()
  
  cols_order <- cols_order[cols_order %in% colnames(table)] # make subset
  
  table <- table %>% 
    select(c("Group_", cols_order)) %>% 
    gt(rowname_col = "Group_") %>% 
    tab_spanner_delim(delim = "_") %>% 
    tab_header(title = "AIM/LMF Plot Continuous Summary Statistics") %>% 
    tab_stubhead(label = "USNVC Group") %>% 
    fmt_number(columns = c(2,3,5,6),
               decimals = 2) %>% 
    fmt_number(columns = c(4,7),
               decimals = 0)
  return(table)
}

cont_table_list <- lapply(X = cont_summary,
                          FUN = pretty_cont_table
)

# run the markdown doc
rmarkdown::render("markdown.rmd")

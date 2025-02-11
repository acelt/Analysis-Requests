# Setup packages
library(readxl)
library(tidyverse)

# write a funciton to tidy individual sheets and then loop over each sheet

# Paths
base_path <- "\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\NM\\Benchmarks"
bm_workbook <- paste0(base_path, "/LCDO Benchmarks Table.xlsx")
office <- "Las Cruces District Office"

# grab a list of all the sheets in the workbook
sheet_list <- excel_sheets(bm_workbook) # there will be a notes page with info and then each subsequent sheet is the name of a ES

# theres going to be some difference between indicators from different methods so:
# these will also inlcude gap since thats also a percentage
# could generate this list by includ all unique entries in the indicator col with % in the title too
percent_indicators = c("Bare Soil Cover (%)", "Total Foliar Cover (%)", "Noxious Cover from LPI (%)", "Noxious Perennial Forb Cover from LPI (%)", "Noxious Annual Forb Cover from LPI (%)", "Noxious Perennial Grass Cover from LPI (%)", "Noxious Annual Grass Cover from LPI (%)", "Noxious Succulent Cover from LPI (%)", "Noxious Shrub Cover from LPI (%)", "Noxious Tree Cover from LPI (%)", "Non-Noxious Perennial Forb Cover from LPI (%)", "Non-Noxious Annual Forb Cover from LPI (%)", "Non-Noxious Perennial Grass Cover from LPI (%)", "Non-Noxious Annual Grass Cover from LPI (%)", "Non-Noxious Succulent Cover from LPI (%)", "Non-Noxious Sub-shrub Cover from LPI (%)", "Non-Noxious Shrub Cover from LPI (%)", "Non-Noxious Shrub Cover excluding PRGL2 from LPI (%)", "PRGL2 Percent Cover (%)", "Non-Noxious Tree Cover from LPI (%)", "Land Cover with Canopy Gaps 25-100 cm (%)", "Land Cover with Canopy Gaps over 101 cm (%)", "Herbaceous Litter Cover from LPI (%)", "Woody Litter Cover from LPI (%)", "Total Litter Cover from LPI (%)", "Total Rock Cover from LPI (%)")

# soil stability is bounded by 1-6
ss_indicators <-  c('Average Soil Stability', 'Soil Stability under Perennial Cover',"Soil Stability of Bare Soil")

# heights have minimum of 0cm and max of 1800cm
height_indicators <- c("Average Height of Woody Plants (cm)", "Average Height of Herbaceous Plants (cm)")

# Counts of species don't technically have a maximum...

# heres a LUT for indicator names:
indicator_lut <- read.csv(file = paste0(base_path, "/Indicator LUT.csv"))[1:35, 1:2]

benchmark_list <- list()
  
# Ill loop over these sheets (exluding the first)
for(i in sheet_list[2:length(sheet_list)]){
  print(i)
  #i <-  "SANDY_R042XB012NM" # for testing
  sheet <- read_excel(bm_workbook, sheet = i, skip = 0) # skipping the first row to avoid weird col names - this only works for LCDO
  
  if(all(is.na(sheet[,2]))){
    print(paste0("sheet ",i, " is empty"))
    next
  } # skip sheet if its empty
  
  # gather condition category cols
  sheet_long <- pivot_longer(sheet, cols = c(2:4), names_to = "Condition Category", values_to = "Inequality") # removing hardcoding of category types - hopefully these are alwats in cols 2:4
  
  # remove NAs now since they are causing trouble
  sheet_long <- sheet_long[!is.na(sheet_long$Inequality),]
  sheet_long <- sheet_long[sheet_long$Inequality != "N/A",]
  
  # if theres a "|" need another row for additonal inequality
  #Split out inequalities
  # using if to catch errors where there are no "|"
  if(any(str_detect(sheet_long$Inequality, "\\|"))){
    sheet_long <- sheet_long %>% 
      mutate(Inequality2 = if_else(str_detect(Inequality, "\\|"), str_split(Inequality, "\\|", simplify = TRUE)[,2],"NA"),
             Inequality = if_else(str_detect(Inequality, "\\|"), str_split(Inequality, "\\|", simplify = TRUE)[,1], Inequality))
  } else{
    sheet_long$Inequality2 <- NA
  }
  
  # add to additional row
  sheet_long <- sheet_long %>% 
    pivot_longer(cols = c("Inequality","Inequality2"), values_to = "Inequality", values_drop_na = TRUE) %>% 
    select(-name) %>% 
    filter(!Inequality %in% c("","N/A"))  # remove NAs
  
  # Split up inequality into individual parts and add lower and upper limits
  sheet_long <- sheet_long %>% 
    mutate(Value1 = str_extract(Inequality, "[-]{0,1}[[:digit:]]+\\.{0,1}[[:digit:]]*"), # crazy regex here to grab numbers with decimals
           Value3 = str_extract_all(Inequality, "\\d+$"),
           Relation = str_extract(Inequality, "\\D+"))
  
  sheet_long$Value3[is.na(sheet_long$Value1)] <- ""
  
  sheet_long$Value2 <- sheet_long$Value3
  
  sheet_long$Value2[sheet_long$Value1 == sheet_long$Value3] <- ""
  
  sheet_long <- sheet_long %>% 
    select(-Value3)
  
  # fill in lower limits
  # this  does mean Value 2 is sometimes smaller than Value1 so well need to rearrange
  sheet_long$Value2[sheet_long$Relation %in% c("<","<=") & sheet_long$Value2 == "" & sheet_long$Indicator %in% percent_indicators] <- 0
  sheet_long$Value2[sheet_long$Relation %in% c("<","<=") & sheet_long$Value2 == "" & sheet_long$Indicator %in% ss_indicators] <- 1
  sheet_long$Value2[sheet_long$Relation %in% c("<","<=") & sheet_long$Value2 == "" & sheet_long$Indicator %in% height_indicators] <- 0
  
  # and upper
  sheet_long$Value2[sheet_long$Relation %in% c(">", ">=") & sheet_long$Value2 == "" & sheet_long$Indicator %in% percent_indicators] <- 100
  sheet_long$Value2[sheet_long$Relation %in% c(">", ">=") & sheet_long$Value2 == "" & sheet_long$Indicator %in% ss_indicators] <- 6
  sheet_long$Value2[sheet_long$Relation %in% c(">", ">=") & sheet_long$Value2 == "" & sheet_long$Indicator %in% height_indicators] <- 1800
  
  # assuming we will rename value1 to the lower limit and value2 to uppe limit:
  sheet_long <- sheet_long %>% 
    mutate(across(starts_with("Value"), as.numeric)) %>% 
    mutate(Lower_Limit = if_else(Value1>Value2,Value2, Value1, missing=Value1),  # fill in a few NAs
           Upper_Limit = if_else(Value1>Value2, Value1, Value2)) # I think this looks right, although theres some ambiguity with the inequality symbols
  
  # sorting out LL and UL relations
  sheet_long <- sheet_long %>% 
    rename(UL_Relation = Relation)
  
  sheet_long$UL_Relation <- gsub(">","<",  sheet_long$UL_Relation)
  
  # fill in NAs with == since those dont have an upper limit
  sheet_long$UL_Relation[is.na(sheet_long$UL_Relation)] <- "=="
  # I think in those instances just duplicating the lower limit to the upper should work with the benchmark tool as well
  sheet_long$Upper_Limit[is.na(sheet_long$Upper_Limit)] <- sheet_long$Lower_Limit[is.na(sheet_long$Upper_Limit)]
  
  # Im not quite sure of a reliable method to deriving the appropriate lower limit relation..so this will need to be checked to make sure no values are excluded
  # Generally one or both of the relations needs to be inclusive (<=), so if the upper isnt already we'll make the lower inclusive
  sheet_long <- sheet_long %>% 
    mutate(LL_Relation = if_else(UL_Relation %in% "<",">=", 
                                 if_else(Lower_Limit == 0, ">=",">"),">"),# however they both could be inclusive... this should mostly be the case when the lower value is 0
           UL_Relation = if_else(Indicator %in% percent_indicators & Upper_Limit == 100,"<=",
                                 if_else(Indicator %in% ss_indicators & Upper_Limit == 6,"<=",
                                         if_else(Indicator %in% height_indicators & Upper_Limit == 1800, "<=",UL_Relation)))) # also if the upper limit is a maximum value (i.e., 100/6/1800 for cover, soil stability and heights) it should be inclusive
  
   # Tidy up indicator names to match terradat fields
  bm_sheet <- merge(x = sheet_long,
                    y = indicator_lut,
                    by.x = "Indicator",
                    by.y = "Nmdat",
                    all.x = TRUE,
                    all.y = FALSE) 
  
  # Not sure if I should keep the custom indicators in here?? cant really use them in BM tool as is so removing them for now
  bm_sheet_rename <- bm_sheet %>% 
    filter(!is.na(Terradat)) %>% 
    mutate(Management_Question = "Land Health", # this may not be the best label but keeping it for now
           Objective_name = paste0(office, "_", Terradat),
           Benchmark_Group = i, # Add benchmark group from sheet name
           Reporting_Unit = office,
           Unit = NA,
           Proportion_Relation = NA,
           Required_Proportion = NA,
           ) %>% 
    select(-Indicator) %>% 
    rename(Benchmark_Source = Source,
           Indicator = Terradat,
           Condition_Category = `Condition Category`) %>% 
    select(Objective_name, # reordering here to match benchmark tool
           Management_Question,
           Benchmark_Source,
           Benchmark_Group,
           Reporting_Unit,
           Lower_Limit,
           LL_Relation,
           Indicator,
           UL_Relation,
           Upper_Limit,
           Unit,
           Condition_Category,
           Proportion_Relation,
           Required_Proportion,
           Inequality)# leaving this is to double check everything looks good in excel
 
  # exporting
 write.csv(bm_sheet_rename, file = paste0(base_path,"/outputs/", i, ".csv"), row.names = FALSE)
  
 # may want to add each tab to a list element so we can rbind them all together eventually
 benchmark_list[[i]] <- bm_sheet_rename
   
}

bm_df <- do.call("rbind", benchmark_list)
# before we write to csv lets fix field types so append in Arc is simple
bm_df$Lower_Limit <- as.numeric(bm_df$Lower_Limit)
bm_df$Upper_Limit <- as.numeric(bm_df$Upper_Limit)

write.csv(bm_df, file = paste0(base_path,"/outputs/", office, ".csv"), row.names = FALSE)

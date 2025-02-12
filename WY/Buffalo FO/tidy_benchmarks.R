library(tidyverse)

# convert benchmarks from monitoring plan into benchmark tool machine readable format
# I should probably just gericise this and make into a function but for now...
base_path  <- "\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\WY\\Buffalo FO\\Buffalo FO\\"
doc <- paste0(base_path, "Monitoring Objectives Buffalo FO.csv")

data <- read.csv(doc, header = TRUE) |> 
  rename("Benchmark Group" = "ESD.Code",
         "BareSoilCover" = "Bare.Ground",
         "SoilStability_All" = "Soil.Aggregate.Stability") |> 
  select(-ESD.Name) |> 
  pivot_longer(cols = c(BareSoilCover, SoilStability_All), names_to = "Indicator", values_to = "Limits") |> 
  separate(col = Limits, sep = "-", into = c("Lower Limit","Upper Limit")) |> 
  mutate(`Lower Limit` = gsub("\\? ","", `Lower Limit`)) |> 
  mutate(`Upper Limit` = gsub("\\%", "", `Upper Limit`),
         `Condition Category` = "Meeting") |> 
  filter(`Benchmark Group` != "")

# now to check for 0s and 100s
rows_to_add <- list()

# Check each row for presence of 0s
for(i in unique(data$`Benchmark Group`)){
  data_bs <- data[data$Indicator == "BareSoilCover",]
  if(!any(data_bs[data_bs$`Benchmark Group` == i, c("Lower Limit","Upper Limit")] == "0")){
    # Add the inequality row for this case
    # data.frame() is adding . to colnames so using tibble() instead
    new_row <- tibble(`Lower Limit` = "0",
                      `Upper Limit` = data_bs[data_bs$`Benchmark Group` == i, "Lower Limit"][[1]],
                      `Benchmark Group` = i,
                      `Indicator` = "BareSoilCover",
                      `Condition Category` = "Not Meeting")
    rows_to_add[[length(rows_to_add) + 1]] <- new_row
  }
  # Check rows for presence of 100s
  if(!any(data_bs[data_bs$`Benchmark Group` == i, c("Lower Limit","Upper Limit")] == "100")){
    # Add the inequality row for this case
    new_row <- tibble(`Lower Limit` = data_bs[data_bs$`Benchmark Group` == i, "Upper Limit"][[1]],
                       `Upper Limit` = "100",
                       `Benchmark Group` = i,
                       `Indicator` = "BareSoilCover",
                       `Condition Category` = "Not Meeting")
    rows_to_add[[length(rows_to_add) + 1]] <- new_row
  }
}

# Combine the original dataframe with the new rows
new_rows_df <- do.call(rbind, rows_to_add)
data_newrows <- rbind(data, new_rows_df)

# Upper limit for soil stability is always 6
data_newrows[data_newrows$Indicator == "SoilStability_All","Upper Limit"] <- "6"

# Adding other rows
data_final <- data_newrows |> 
  mutate(`Objective Name` = paste0(Indicator, "_BUFO"),
         `Management Question` = "Pre-fire condition",
         `Benchmark Source` = "BUFO Monitoring Plan",
         `Reporting Unit` = "Buffalo Field Office",
         `LL Relation` = ifelse(`Lower Limit` == "0", ">=", ifelse(`Condition Category` == "Not Meeting", ">", ">=")),
         `UL Relation` = ifelse(`Upper Limit` == "100", "<=", ifelse(`Condition Category` == "Not Meeting", "<", "<=")),
         `Proportion Relation` = "<",
         `Required Proportion` = "80",
         "Unit" = "") |> 
  select(`Objective Name`,
         `Management Question`,
         `Benchmark Source`,
         `Benchmark Group`,
         `Reporting Unit`,
         `Lower Limit`,
         `LL Relation`,
         `Indicator`,
         `UL Relation`,
         `Upper Limit`,
         `Unit`,
         `Condition Category`,
         `Proportion Relation`,
         `Required Proportion`)

# save
write.csv(data_final, paste0(base_path, "SoilBenchmarks.csv"), row.names = FALSE)

doc2 <- paste0(base_path, "foliarcoverbenchmarks.csv")

# OK now the same for foliar cover benchmarks
data <- read.csv(doc2, header = TRUE) |> 
  rename("Benchmark Group" = "ESD.Code",
         "AH_NonNoxPerenGrassCover" = "Grasses",
         "AH_NonNoxPerenForbCover" = "Forbs",
         "AH_ShrubCover" = "Shrub") |> 
  select(-ESD.Name) |> 
  pivot_longer(cols = c(AH_NonNoxPerenGrassCover, AH_NonNoxPerenForbCover,AH_ShrubCover), names_to = "Indicator", values_to = "Limits") |> 
  separate(col = Limits, sep = "-", into = c("Lower Limit","Upper Limit")) |> 
  mutate(`Lower Limit` = gsub("\\? ","", `Lower Limit`)) |> 
  mutate(`Upper Limit` = gsub("\\%", "", `Upper Limit`),
         `Condition Category` = "Meeting") |> 
  filter(`Benchmark Group` != "") |> 
  select(-Forage.Production.in.Lbs.Acre)# drop this for now - we may be able to add it back in for benchmarks RAP production

# now to check for 0s and 100s
rows_to_add <- list()

# Check each row for presence of 0s
for(i in unique(data$`Benchmark Group`)){
  for(j in c("AH_NonNoxPerenGrassCover", "AH_NonNoxPerenForbCover","AH_ShrubCover")){
    data_bs <- data[data$Indicator == j,]
    
    if(!any(data_bs[data_bs$`Benchmark Group` == i, c("Lower Limit","Upper Limit")] == "0")){
      # Add the inequality row for this case
      # data.frame() is adding . to colnames so using tibble() instead
      new_row <- tibble(`Lower Limit` = "0",
                        `Upper Limit` = data_bs[data_bs$`Benchmark Group` == i, "Lower Limit"][[1]],
                        `Benchmark Group` = i,
                        `Indicator` = j,
                        `Condition Category` = "Not Meeting")
      rows_to_add[[length(rows_to_add) + 1]] <- new_row
    }
    # Check rows for presence of 100s
    if(!any(data_bs[data_bs$`Benchmark Group` == i, c("Lower Limit","Upper Limit")] == "100")){
      # Add the inequality row for this case
      new_row <- tibble(`Lower Limit` = data_bs[data_bs$`Benchmark Group` == i, "Upper Limit"][[1]],
                        `Upper Limit` = "100",
                        `Benchmark Group` = i,
                        `Indicator` = j,
                        `Condition Category` = "Not Meeting")
      rows_to_add[[length(rows_to_add) + 1]] <- new_row
    }
  }
  }

# Combine the original dataframe with the new rows
new_rows_df <- do.call(rbind, rows_to_add)
data_newrows <- rbind(data, new_rows_df)

# Adding other rows
data_final <- data_newrows |> 
  mutate(`Objective Name` = paste0(Indicator, "_BUFO"),
         `Management Question` = "Pre-fire condition",
         `Benchmark Source` = "BUFO Monitoring Plan",
         `Reporting Unit` = "Buffalo Field Office",
         `LL Relation` = ifelse(`Lower Limit` == "0", ">=", ifelse(`Condition Category` == "Not Meeting", ">", ">=")),
         `UL Relation` = ifelse(`Upper Limit` == "100", "<=", ifelse(`Condition Category` == "Not Meeting", "<", "<=")),
         `Proportion Relation` = "<",
         `Required Proportion` = "80",
         "Unit" = "") |> 
  select(`Objective Name`,
         `Management Question`,
         `Benchmark Source`,
         `Benchmark Group`,
         `Reporting Unit`,
         `Lower Limit`,
         `LL Relation`,
         `Indicator`,
         `UL Relation`,
         `Upper Limit`,
         `Unit`,
         `Condition Category`,
         `Proportion Relation`,
         `Required Proportion`)

# save
write.csv(data_final, paste0(base_path, "FoliarCoverBenchmarks.csv"), row.names = FALSE)

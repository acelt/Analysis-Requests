library(readxl)

file_path <- "\\\\blm.doi.net\\dfs\\loc\\EGIS\\ProjectsNational\\AIM\\Projects\\CA\\NorthernCA_DO_EagleLake_FO\\Data\\DIMA\\2021/DIMA Exported Data"

all_files <- list.files(file_path, full.names = TRUE, pattern = "*.xlsx")

data <- lapply(all_files, function(x) read_xlsx(x))

for(i in all_files){
  data <- rbind(data, read_xlsx(i, sheet = 1))
}

tables_names <- all_files <- list.files(file_path, full.names = FALSE, pattern = "*.xlsx")
tables_names <-  gsub(".xlsx","",tables_names)

names(data) <- tables_names

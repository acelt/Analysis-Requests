library(tidyverse)
library(arcgisbinding)
library(readxl)
arc.check_product()

# pull in SDD 
# this is the backup from September
sdd_gdb <- "C:\\Users\\alaurencetraynor\\Documents\\ID\\OFO_WA.gdb"
point_eval <- paste0(sdd_gdb, "/","TerrestrialPointEvaluation_backupOFO")
sample_points <- paste0(sdd_gdb,"/" ,"TerrestrialSamplePoints_backupOFO")

# also the clipped version of tdat
tdat_ofo <- paste0(sdd_gdb, "/", "aim_ofo")

# read to data frame
point_eval_df <- arc.select(arc.open(point_eval))
sample_points_df <- arc.select(arc.open(sample_points))

tdat_ofo_df <- arc.select(arc.open(tdat_ofo))

# Pull in all the various years of plot tracking excels
dir <- "\\\\blm.doi.net\\dfs\\loc\\EGIS\\ProjectsNational\\AIM\\Projects\\ID\\Owyhee_FO\\Data\\SampleDesign\\"
plot_track_2016 <- paste0("\\\\blm.doi.net\\dfs\\loc\\EGIS\\ProjectsNational\\AIM\\Projects\\ID\\Owyhee_FO\\Data\\DIMA/2016/IDAIM OWFO_Plot Tracking _20161026.xlsx")
plot_track_2016 <- readxl::read_xlsx(plot_track_2016)
plot_track_2017 <- paste0(dir,"2017/IDAIM OWFO_Plot Tracking_20171114.xlsx")
plot_track_2017 <- readxl::read_xlsx(plot_track_2017)

# 2018 data is only from State ESD design - not included here

plot_track_2019 <- paste0(dir, "2019/IDAIM OWFO 2019_Plot Tracking_20200310.xlsx")
plot_track_2019 <- readxl::read_xlsx(plot_track_2019)

# Pull in Survey plots feature class
survey_dir <- "\\\\blm.doi.net\\dfs\\loc\\EGIS\\ProjectsNational\\AIM\\Data\\Production\\TerraDat\\Ingest Survey123\\2020\\ID\\RawDownload\\OUT_BLM_ID_AIM_2020_Plots_Service\\ID2020RawDownload.gdb\\"
plot_track_2020 <- paste0(survey_dir, "Plots")
plot_track_2020 <- arc.select(arc.open(plot_track_2020), where_clause = "Design = 'OwyheeFOLUP2016'")

survey_dir <- "\\\\blm.doi.net\\dfs\\loc\\EGIS\\ProjectsNational\\AIM\\Data\\Production\\TerraDat\\Ingest Survey123\\2021\\ID\\ID2021RawDownloadCleaned.gdb\\"

plot_track_2021 <- paste0(survey_dir, "Plots")
plot_track_2021 <- arc.select(arc.open(plot_track_2021), where_clause = "ProjectName = 'OwyheeFOLUP2016'")

# looking at number of rejections and sampled points and compare to terradat/SDD
# grab the relevant info pk, date, designation
# these plot tracking spreadhsheets dont have pks so well use plot id

plot_track_2016 <- plot_track_2016 %>% 
  select(PlotIDPlotName,
         DateVisited,
         PlotStatus,
         RejectionCriteria) %>% 
  mutate(Year = "2016")

plot_track_2017 <- plot_track_2017 %>% 
  select(PlotIDPlotName,
         DateVisited,
         PlotStatus,
         RejectionCriteria)%>% 
  mutate(Year = "2017")
  
plot_track_2019 <- plot_track_2019 %>% 
  select(PlotIDPlotName,
         DateVisited,
         PlotStatus,
         RejectionCriteria)%>% 
  mutate(Year = "2019")

plot_track_allyears <- rbind(plot_track_2016, plot_track_2017, plot_track_2019) %>% 
  rename(Date = DateVisited,
         PlotID = PlotIDPlotName)

# summarise Plots fc from 2020 and 2021

plot_track_2020 <- plot_track_2020 %>% 
  select(PlotID,
         last_edited_date, # using this although its not the same as data established
         EvalStatus,
         RejectedReason) %>% 
  mutate(Date = as.Date(last_edited_date)) %>% 
  select(-last_edited_date) %>% 
  rename(RejectionCriteria = RejectedReason,
         PlotStatus = EvalStatus)%>% 
  mutate(Year = "2020")

plot_track_2021 <- plot_track_2021 %>% 
  select(PlotID,
         EditDate, # using this although its not the same as data established
         EvalStatus,
         RejectedReason)%>% 
  mutate(Date = as.Date(EditDate)) %>% 
  select(-EditDate) %>% 
  rename(RejectionCriteria = RejectedReason,
         PlotStatus = EvalStatus)%>% 
  mutate(Year = "2021")

plot_track_allyears <- rbind(plot_track_allyears, plot_track_2020, plot_track_2021)


# make consistent
plot_track_allyears <- plot_track_allyears %>% 
  mutate(PlotStatus = ifelse(PlotStatus == "Eval","Sampled",
                             ifelse(PlotStatus == "NotEval", "Not Sampled",
                                    ifelse(PlotStatus == "OverSample", NA,PlotStatus))))

summary(as.factor(plot_track_allyears$PlotStatus))
# 78 not sampled
# 97 rejected
# 250 sampled
# 111 NAs

# figuring out final designaion
final_designation <- plot_track_allyears %>% 
  group_by(PlotID) %>% 
  summary(final_designation =)
# filter point eval down to just 2 owyhee designs
designs <-  c("ID_OwyheeFO_FlintCreek_2020_HAF_SampleFrame_1","ID_OwyheeFO_2016_LUP_SampleFrame_1")
ofo_design_points <- sample_points_df[sample_points_df$TerrestrialSampleFrameID %in% designs, "SamplePointKey"]

point_eval_ofo <- point_eval_df[point_eval_df$SamplePointKey %in% ofo_design_points,]
summary(as.factor(point_eval_ofo$FinalDesignation))
# Inaccessible     Non Target Target Sampled           NA's 
#       87             22            206             88 

## Need to check plot tracking sequentially by year since there are many tmep rejections
# this has more rejections and fewer sampled points

# removing modhaf designs
tdat_ofo_df <-  tdat_ofo_df %>% 
  filter(!ProjectName %in% c("Idaho BOSH 2019","Idaho State ESD 2018","BOSH_2021","Owyhee_ModHAF_2021","OwyheeFOMHAFFlintCreek2020"))

# 250 sampled points
write.csv(plot_track_allyears, "C:\\Users\\alaurencetraynor\\Documents\\ID\\plot_tracking_allyears.csv")

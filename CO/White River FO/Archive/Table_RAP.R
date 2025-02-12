Table_RAP <-  function(filepath){
  
  # Read in csv from RAP trend estimates from csv 
  rap_estimates <- read.csv(filepath)%>%
    dplyr::rename(c("Annual Forb & Grass Cover"= "AFGC","Perennial Forb & Grass Cover" ="PFGC", "Shrub Cover"="SHR", "Tree Cover"="TREE", "Annual Temperature (°F)"="annualTemp", "Annual Precipitation (in)"="annualPrecip", "Bare Ground" = "BG"))
  
  t <- rap_estimates %>% DT::datatable(escape = FALSE, 
                extensions = 'Buttons', 
                filter = "top" , 
                options = list(scrollX = TRUE ,
                               dom = 'Bfrtip',
                               buttons =
                                 list(list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel'),
                                   text = 'Download Table'))) , 
                caption = (paste("RAP Indicator Estimates between 1984 to 2020 in " , attribute_name)) , 
                rownames = FALSE)
  
  return(t)
}
tool_exec <- function(in_params, out_params) 
{
  
  require(arcgisbinding)
  require(sf)
  require(tidyverse)
  require(viridis)
  library(terradactyl)

  
  arc.progress_label('Loaded required R packages')
  
  arc.check_product()
  
  arc.progress_label('ArcPro License verified')
  
  in_data<-in_params[[1]]
  
  species_list<-in_params[[2]]
  
  indicator_name<-in_params[[3]]
  
  species_list <- unlist(indicators)# Arcpro reads this in as a list which will fail in the arc.select below
  
  output_dir <- in_params[[5]]
  
  plot_data<-arcgisbinding::arc.open(in_data)
  
  # selecting only relevant variables
  plot_data_arc <- arcgisbinding::arc.select(object = plot_data, fields = c(indicators,group))
  
  # elongate plot data 
  plot_data_df <- pivot_longer(plot_data_arc, cols = all_of(indicators), names_to = "indicator", values_to = "value" )
  
  # making variable for figure size based on number of unique groups
  width<- 10 + length(unique(plot_data_df[[group]]))
  height<- 8 + length(unique(plot_data_df[[group]]))
  
  
  if(plot_type == "Boxplot"){
    plot <- ggplot(data = plot_data_df, aes(x = .data[["value"]])) +
      geom_boxplot(aes(fill = .data[[group]], y = .data[[group]]),alpha = 0.5) +
      geom_point(aes(fill = .data[[group]], y = .data[[group]]), position = "jitter") +
      facet_wrap(~indicator, scales = "free") +
      theme_bw(base_size = 20)+
      theme(legend.position = "")+
      viridis::scale_fill_viridis(discrete = TRUE)
  }
  
  if(plot_type == "Histogram"){
    plot <- ggplot(data = plot_data_df, aes(x = .data[["value"]], fill = .data[[group]]))+
      geom_histogram(position = "dodge", binwidth = 10)+
      theme_bw(base_size = 20)+
      theme(legend.position = "bottom")+
      viridis::scale_fill_viridis(discrete = TRUE)
    
  }
  
  if(plot_type == "Benchmarks"){
    # reorganise the dataset
    
    for(indicator in unique(indicators)){
      
      plot_data <- plot_data_arc[,c(group,indicator)]
      
      p <- ggplot(data = plot_data, aes(x = .data[[group]], fill = .data[[indicator]])) +
        geom_bar(position = "dodge")+
        viridis::scale_fill_viridis(discrete = TRUE)+
        theme_bw(base_size = 20, base_family = "serif")
      
      # export plot
      ggsave(plot = p,
             filename = paste0(indicator,"_", plot_type, ".jpeg"),
             device = "jpeg",
             width = width,
             height = height,
             path = output_dir,
             dpi = 400)
      
    }
    
  } else{
    ggsave(filename = paste0(output_dir,"/", plot_type, ".jpeg"), plot = plot,device = "jpeg",height = height,width = width,units = "in",limitsize = FALSE)
  }
  
}





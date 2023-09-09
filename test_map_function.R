library(ggplot2)# For plots
library(tidyverse)
library(ggpattern)
library(plotly) # For interactive plot
library(sp) # For maptools package
library(maptools) # For regional map
library(broom) # For tidying the data
library(shiny) # For R-Shiny
library(mapproj) # For Coord_map() function

mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
mismanaged_gdp_10 <- mismanaged_vs_gdp %>%
  filter(Year == 2010) %>%
  select(Entity, `Per capita mismanaged plastic waste (kilograms per person per day)`) %>%
  rename("Daily_Mismanaged_Waste"= `Per capita mismanaged plastic waste (kilograms per person per day)`)


heatmap <- function(df, column_name, region) {

  # Select only the data that the user has selected to view
  plot_df <- df %>%
    select(Entity, {{column_name}}) %>%
    rename(value = {{column_name}}, region = Entity)


  ##### Create regional map  #####
  if (region == "Africa") { # Create map for Africa

    africa <-  maps::map("world", fill=TRUE, plot=FALSE) %>%
      maptools::pruneMap(xlim = c(-20,60), ylim = c(-40, 40)) %>%
      tidy()
    sub_map <- left_join(africa , plot_df, "region") # Join Africa map with plot_df

  } else if (region == "North_America") { # Create map for North America

    north_america <-  maps::map("world", fill=TRUE, plot=FALSE) %>%
      maptools::pruneMap(xlim = c(-180, -50), ylim = c(0, 80)) %>%
      tidy()
    sub_map <- left_join(north_america , plot_df, "region") # Join North America map with plot_df

  } else if (region == "Europe") { # Create map for Europe

    europe <-  maps::map("world", fill=TRUE, plot=FALSE) %>%
      maptools::pruneMap(xlim = c(-27,36), ylim = c(34,67)) %>%
      tidy()
    sub_map <- left_join(europe , plot_df , "region") # Join Europe map with plot_df

  } else if (region == "Oceania") { # Create map for Oceania

    oceania <-  maps::map("world", fill=TRUE, plot=FALSE) %>%
      maptools::pruneMap(xlim = c(100, 180), ylim = c(-50, 0)) %>%
      tidy()
    sub_map <- left_join(oceania , plot_df , "region") # Join Oceania map with plot_df

  } else if (region == "Asia") { # Join Europe map with plot_df

    asia <-  maps::map("world", fill=TRUE, plot=FALSE) %>%
      maptools::pruneMap(xlim = c(20,180), ylim = c(-10, 70)) %>%
      tidy()
    sub_map <- left_join(asia , plot_df , "region") # Join Asia map with plot_df

  } else if(region == "South_America") { # Create map for South_America

    South_America <-  maps::map("world", fill=TRUE, plot=FALSE) %>%
      maptools::pruneMap(xlim = c(-90,-30), ylim = c(-60, 20)) %>%
      tidy()
    sub_map <- left_join(South_America , plot_df , "region")# Join South America map with plot_df
  } else { # Create map for the world

    world <-  maps::map("world", fill=TRUE, plot=FALSE) %>%
      maptools::pruneMap(xlim = c(-180,180), ylim = c(-60, 90)) %>%
      tidy()
    sub_map <- left_join(world , plot_df , "region")  } # Join World map with plot_df

  ##### Custom titles for data type  #####
  legendTitle = ""
  if(column_name == 'Daily_Plastic_Waste'){
    plotTitle = "Per capita plastic waste (kilograms per person per day)."
    legendTitle = "kgs"
  }else if(column_name == 'Daily_Mismanaged_Waste'){
    plotTitle = "Per capita mismanaged plastic waste (kilograms per person per day)."
    legendTitle = "kgs"
  }else if(column_name == 'Mismanaged_Waste'){
    plotTitle = "Mismanaged waste in ton, by country."
    legendTitle = "tons"
  }

  # Create a heatmap using Plotly
  heatmap <- ggplot(sub_map, tooltip = "text") +
    geom_polygon(color = "white",aes(x = long, y = lat,
                                     fill = value, group = group,
                                     text = paste(region, "<br>", value, "tons"))) +
    scale_fill_continuous(low="thistle2", high="darkred", trans = "sqrt",
                          guide="colorbar", na.value = "#9f9f99") +
    theme(panel.background = element_rect(fill="white"),
          axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          legend.title = element_text(size=10)) +
    labs(fill=legendTitle, title= plotTitle) +
    coord_map()


  heatmap_plotly <- ggplotly(heatmap, tooltip = "text")

  return(heatmap_plotly)

}

heatmap(mismanaged_gdp_10, "Daily_Mismanaged_Waste", "World")

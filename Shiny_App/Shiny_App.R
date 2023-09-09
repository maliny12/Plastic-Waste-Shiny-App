#################### ABOUT THE APP #####################
## Author : Maliny Po
## Source Data: [TidyTuesday/Data](https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-05-21#coast_vs_wastecsv)
## Purpose: The app aims to make people more aware of this problem and encourage conversations around this issue, all in an interactive and fun way.
##Link : https://malinypo.shinyapps.io/Global_Plastic_Waste/
##Session Info:
## R version 4.2.2 (2022-10-31)
## Platform: macOS Monterey 12.6.5
########################################################


########## Load Library ##########

library(ggplot2)# For plots
library(tidyverse)
library(ggpattern)
library(plotly) # For interactive plot
library(sp) # For maptools package
library(maptools) # For regional map
library(broom) # For tidying the data
library(shiny) # For R-Shiny
library(mapproj) # For Coord_map() function



########## Load Data ##########

# load the Coast_vs_waste data
# This data is Mismanaged waste in ton, by country
coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")
coast_waste_10 <- coast_vs_waste %>%
  filter(Year == 2010 & Entity != "World") %>% # Filter for observations in 2010
  select(Entity,`Mismanaged plastic waste (tonnes)`) # Keep only the relevant columns

# Load the Waste_vs_gdp data
# This data is per-capital plastic waste
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")
waste_gdp_10 <- waste_vs_gdp %>%
  filter(Year == 2010) %>% # Filter for observations in 2010
  select(Entity,`Per capita plastic waste (kilograms per person per day)`) # Keep only the relevant columns


# Load the mismanaged_vs_gdp
# This data is per-capital mismanaged wasted
mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
mismanaged_gdp_10 <- mismanaged_vs_gdp %>%
  filter(Year == 2010) %>%
  select(Entity, `Per capita mismanaged plastic waste (kilograms per person per day)`)

#Rename Entities to match with WorldData
waste_gdp_10$Entity[waste_gdp_10$Entity == "United States"] <- "USA"
waste_gdp_10$Entity[waste_gdp_10$Entity == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
waste_gdp_10$Entity[waste_gdp_10$Entity == "United Kingdom"] <- "UK"
coast_waste_10$Entity[coast_waste_10$Entity == "United States"] <- "USA"
coast_waste_10$Entity[coast_waste_10$Entity == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
coast_waste_10$Entity[coast_waste_10$Entity == "United Kingdom"] <- "UK"
mismanaged_gdp_10$Entity[mismanaged_gdp_10$Entity == "United States"] <- "USA"
mismanaged_gdp_10$Entity[mismanaged_gdp_10$Entity == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
mismanaged_gdp_10$Entity[mismanaged_gdp_10$Entity == "United Kingdom"] <- "UK"

# Create a new data set with no NA Value for the barplot funtion
coast_waste_no_NA <- coast_waste_10 %>%na.omit()

# Row bind all 3 datasets
df <- left_join(waste_gdp_10, mismanaged_gdp_10, by='Entity') %>%
  left_join(., coast_waste_10, by='Entity') %>%
  rename("Daily_Plastic_Waste" = `Per capita plastic waste (kilograms per person per day)` ,
         "Daily_Mismanaged_Waste" = `Per capita mismanaged plastic waste (kilograms per person per day)`,
         "Mismanaged_Waste" = `Mismanaged plastic waste (tonnes)`)

########## Heatmap Function  ##########

# Create a function that return a heat map based on user's selection
# Parameters:
# df : Data frame containing all 3 datasets (waste_gdp_10, mismanaged_gdp_10, and coast_waste_10)
# column_name : User selection for data type
# region : User selection for region
# It returns an interactive heat map based on selected data set

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
                                     text = paste(region, "<br>", value, legendTitle))) +
    scale_fill_continuous(low="lightpink", high="indianred", trans = "sqrt",
                          guide="colorbar", na.value = "lightskyblue4") +
    theme(panel.background = element_rect(fill="skyblue"),
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

########## Bar Chart Function  ##########

# Function for bar chart
# This function is intended to take in user input and plot plastic waste generated by the respected country
# User can select multiple counties at once

barchart <- function(coast_waste_no_NA, country_list) {

  # Make a new dataset containing only the countries users want to see
  new_df <- coast_waste_no_NA %>%
    filter(coast_waste_no_NA$Entity %in% country_list) %>%
    rename("Value" = `Mismanaged plastic waste (tonnes)`)

  barchart <- ggplot(new_df,
                     aes(
                       x = Value,
                       y= Entity,
                       fill = Entity,
                       text = paste(Entity, "<br>", Value, "tons")
                       )
                     ) +
    geom_col() +
    theme(legend.position = "Top") +
    labs(x = " ", y = "", title = "Mismanaged plastic waste (tons)") +
    theme_classic()

  barchart_plotly <- ggplotly(barchart, tooltip = "text") %>%
    layout(legend = list(orientation = "h"))

  return(barchart_plotly)

}


########## Shiny App  ##########

data_type <- c('Daily_Plastic_Waste', 'Daily_Mismanaged_Waste', 'Mismanaged_Waste')
region <- c("World", "North_America","Europe", "Oceania", "Asia", "South_America","Africa" )

# UI
ui <- navbarPage("Global Plastic Pollution", id = "nav",
                 tabPanel("Interactive map",
                          div(class = "outer",
                              tags$head(
                                includeCSS("www/styles.css"),
                              ),
                              # Insert heat_map
                              fluidRow(
                                column(width = 9,
                                       plotlyOutput("heatmap_plot", width = "1480px", height = "900px"),

                                ),
                                column(width = 3,
                                       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                     draggable = TRUE, top = 60, left = 20, right = 20, bottom = "auto",
                                                     width = 400, height = "auto",
                                                     h2(""),
                                                     selectInput("column_name",label = "Select Data Type",data_type),
                                                     selectInput("region", label = "Select Region", region),
                                                     selectInput("countries", "Select countries:",
                                                                 choices = unique(coast_waste_no_NA$Entity),
                                                                 multiple = TRUE),
                                                     # Insert bar_plot
                                                     plotlyOutput("bar_plot")
                                       )
                                )
                              )
                          )
                 ),
                 tabPanel(title = "Data Explorer",
                          imageOutput("header_img"),
                          br(),
                          hr(),
                          h4(strong("About the App")),
                          p(style="text-align: justify; font-size = 25px",
                            "The creator of this app is Maliny Po. "),
                          p(style="text-align: justify; font-size = 25px",
                            "Plastic pollution is a big and growing issue that's harming oceans and wildlife. The purpose of this app is to
                            raise awareness about global plastic pollution, put into perspective the magnitide of waste produced
                            per day, and promote discussion about effective waste management policies."), 
                          
                          hr(),
                          
                          hr(),
                          h4(strong("Project Description")),
                          p(style="text-align: justify; font-size = 25px",
                            "Plastic pollution is a global crisis rapidly spiralling out of control, 
                            leaving a detrimental mark on our environment, oceans, and future generations. 
                            While plastic is a versatile material that has transformed various industries, 
                            its disposal and mismanagement have resulted in dire consequences. If you wish to 
                            learn more about the global plastic pollution problem, consider reading the",
                            a(href = "https://malinypo.quarto.pub/global-plastic-waste",
                              "Plastic Pollution blog post"),
                            ", written by Maliny Po. The blog examine how much each plastic waste each 
                            country produces, and the urgent need for 
                            effective waste management systems, focusing on plastic emissions into the ocean"),
                          hr(),
                          
                          hr(),
                           h4(strong("Data Description")),
                           p(style="text-align: justify; font-size = 25px",
                             "The data used for this assessment is the `Plastic Waste` data that is found at", 
                             a(href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-05-21#coast_vs_wastecsv",
                               "TidyTuesday"),
                             "."),
                           p(style="text-align: justify; font-size = 25px",
                             "- Coast_vs_waste dataset provides an annual estimate of plastic discharge. 
                                      It is worth noting that this figure for each nation does not encompass waste 
                                      exported abroad, which could have a higher likelihood of ending up in the ocean."),
                           p(style="text-align: justify; font-size = 25px",
                             "- Waste_vs_gdp.csv dataset contains information on the daily amount of plastic 
                                      waste generated per individual, measured in kilograms per person per day. This measurement
                                      reflects the general per capita rate of plastic waste production before accounting for waste
                                      management, recycling, or incineration. Therefore, it does not directly indicate the potential 
                                      risk of polluting waterways or marine environments."),
                           p(style="text-align: justify; font-size = 25px",
                             "- Finally in mismanaged_vs_gdp.csv dataset, mismanaged plastic waste refers 
                                      to plastic that is either littered or improperly disposed of. The total for each 
                                      country does not include waste shipped overseas, where it may also need to be 
                                      appropriately managed."),
                           hr(),
                 ),
                 inverse = T)

# Server
server <- function(input, output) {

  # Output for heatmap
  output$heatmap_plot <- renderPlotly({
    column_name <- input$column_name  # Use appropriate input value
    region <- input$region  # Use appropriate input value
    plot <- heatmap(df, column_name, region)  # Call your heatmap function
    plot
  })

  # Output for barchart
  output$bar_plot <- renderPlotly({
    if (length(input$countries) > 0) {
      country_list <- input$countries
    } else {
      country_list <- c("China", "Australia", "USA")
    }
    plot <- barchart(coast_waste_no_NA, country_list)  # Call your barchart function
    plot
  })
  
  # Output for Data Explorer Page
  output$header_img <- renderImage({
    
    list(src = "figures/plastic_world.jpeg",
         width = "0",
         height = 500)
    
  }, deleteFile = F)
}

# Run the Shiny app
shinyApp(ui, server)






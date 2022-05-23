library(shiny)
library(lubridate)

#Install Packages for Leaflet %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

knitr::opts_chunk$set(echo = TRUE)

# Store string containing all required packages
my_packages <- c('bipartite', 'RColorBrewer')
# Store all installed packages
ya_installed <- library()$results[,1]

# Check whether required packages are already installed and grab only those that still need installation
need_install<-my_packages[!(my_packages %in% ya_installed)]

#install required packages
lapply({need_install}, install.packages, character.only = TRUE)
# Store all installed packages
ya_loaded <- (.packages())

# Check whether required packages are already installed and grab only those that still need installation
need_load<-my_packages[!(my_packages %in% ya_loaded)]

# Load required packages
lapply(need_load, require, character.only = TRUE)


library(tidyverse)
library(tidymodels)

library(knitr)
library(xaringanthemer)


library(maps)
library(ggplot2)
library(tidyverse)
library(corrplot)

library(dplyr)
#library(dsbox)
library(skimr)
library(robotstxt)
library(tidyr) 
library(leaflet) ## For leaflet interactive maps
library(sf) ## For spatial data
library(RColorBrewer) ## For colour palettes
library(htmltools) ## For html
library(leafsync) ## For placing plots side by side
library(kableExtra) ## Table output
library(geojsonio)





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#County map data set cleaning and manipulation  
#Importing state/County JSON containing fip and geometry of the counties
all_counties <- geojsonio:: geojson_read(x= "https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json", what= "sp")
#AK<-geojsonio::geojson_read(x = "https://raw.githubusercontent.com/billcccheng/us-county-boundary-api/master/states/AK.json", what = "sp")

incar <- read.csv("incarceration_trends.csv")
year_1970 <- incar


year_1970<- year_1970 %>%
  mutate(county_name = str_to_lower(str_replace_all(county_name, pattern = " County", replacement = "")))

all_counties <- st_as_sf(all_counties)
all_counties <- all_counties %>%
  mutate(NAME = str_to_lower(NAME))

joint_info_map <- year_1970 %>%
  left_join(all_counties, by = c("county_name" = "NAME")) %>%
  st_as_sf()


joint_info_map  <- joint_info_map  %>%
  #filter(total_pop) %>%
  mutate(total_pop = total_pop)
#this is for changing color 
bins_1 <- c(0, 0.5,1,2,4,15)
pal_1 <- colorBin("Blues", domain = joint_info_map$total_jail_pop/(joint_info_map$total_pop/1000), bins = bins_1)

labels_1 <- sprintf("<strong>%s</strong><br/>%g Total Jail Pop/Total Pop",
                    joint_info_map$county_name, joint_info_map$total_jail_pop/(joint_info_map$total_pop/1000)) %>% 
  lapply(htmltools::HTML)

#%%%%%%%%%%%%%%%%%%%%%%%%%



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Maine Prisoner Advocacy Coalition"), 
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year",
                  label = "Year:",
                  min = lubridate::ymd("19950101"),
                  max = lubridate::ymd("20160101"),
                  value = lubridate::ymd("20000101"),
                  step = 1,
                  timeFormat = "%Y")),
    
    
    
    
    mainPanel(
      leafletOutput("countymap")
      )
    )
  )



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  #Reactive year selection for slider state map
  county_year <- reactive({
    joint_info_map %>%
      filter(year == year(input$year))
  })
 
  # Reactive Labels for leaflet map
  # county labels 
  labels_year_county <- sprintf("<strong>%s</strong><br/>%g Total Jail Pop/Total Pop",
                               county_year()$county_name, county_year()$total_jail_pop/(county_year()$total_pop/1000)) %>% 
  lapply(htmltools::HTML)
  
  output$countymap <- renderLeaflet({
    leaflet(data = county_year()) %>%
        addTiles()%>%
        setView(lng = -80,
                lat = 34.5,
                zoom = 4) %>%
        addPolygons(fillColor = ~pal(joint_info_map$total_jail_pop/(joint_info_map$total_pop/1000)),
                    fillOpacity = 1,
                    color = "black",
                    opacity = 1,
                    weight = 1,
                    highlight = highlightOptions(
                      weight = 3,
                      color = "blue",
                      fillOpacity = .2,
                      bringToFront = TRUE),
                    label = labels_1) %>%
        addLegend(
          position = "topright",
          pal = pal_1,
          values = ~total_jail_pop/(total_pop/1000),
          title = "Ratio of Jail Pop per 1000",
          opacity = 1)%>%
      addPolygons(fillColor = ~pal(county_year()$total_jail_pop/(county_year()$total_pop/1000)),
                  fillOpacity = 1,
                  color = "black",
                  opacity = 0.1,
                  weight = 1,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "blue",
                    fillOpacity = .2,
                    bringToFront = TRUE),
                  label = ~labels_year_county())
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
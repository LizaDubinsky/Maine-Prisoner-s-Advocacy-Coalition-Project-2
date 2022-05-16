library(shiny)
library(leaflet)
library(dplyr)
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

#install.packages("xaringanthemer")
library(tidyverse)
library(tidymodels)
#library(palmerpenguins)
library(knitr)
library(xaringanthemer)
#install.packages("maps")
#install.packages("corrplot")

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
#install.packages("geojsonio")
#install.packages("rgdal")
#install.packages("leaflet")
library(leaflet)
#install.packages("geojsonio")
library(geojsonio)
library(sf)
library(leaflet)
#install.packages("sf")
library(RColorBrewer)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#READ IN JSON FOR STATE DATA%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
states <- geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
                                  , what = "sp")
class(states)

library(readr)
parole_df <- read.csv("all_parole_years.csv")

states <- st_as_sf(states)
states <- states %>%
  mutate(name = str_to_lower(name))

parole_df <- parole_df %>%
  mutate(state = as.factor(str_to_lower(state)))

state_parole <- parole_df %>%
  left_join(states, by = c("state"="name")) %>%
  st_as_sf()




#this is for changing color 
bins <- c(0, 100, 200, 300, 400, 500, 600, 700, 800, 2000)
pal <- colorBin("Spectral", domain = state_parole$number_on_parole_per_100000_us_adult_residents, bins = bins)

labels <- sprintf("<strong>%s</strong><br/>%g Parole/100000 US Adults",
                  state_parole$state, state_parole$number_on_parole_per_100000_us_adult_residents) %>% lapply(htmltools::HTML)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Leaflet
state_map<-leaflet(data = state_parole) %>%
  addTiles() %>%
  setView(lng = -80,
          lat = 34.5,
          zoom = 4) %>%
  addPolygons(fillColor = ~ pal(state_parole$number_on_parole_per_100000_us_adult_residents),
              fillOpacity = 1,
              color = "blue",
              opacity = 0.1,
              weight = 1,
              highlight = highlightOptions(
                weight = 3,
                color = "blue",
                fillOpacity = .2,
                bringToFront = TRUE),
              label = labels) %>%
  addLegend(
    position = "topright",
    pal = pal,
    values = ~number_on_parole_per_100000_us_adult_residents,
    title = "# of U.S. Adults on Parole/100000.",
    opacity = 1)



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
                  timeFormat = "%Y")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("mymap")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  #Set YEAR with Slider
  state_parole_year <- reactive({
    state_parole %>%
      filter(year == year(input$year))
    })
  
  #labels_year <- reactive({ sprintf("<strong>%s</strong><br/>%g Parole/100000 US Adults",
 #                   state_parole_year()$state, state_parole_year()$number_on_parole_per_100000_us_adult_residents) %>% 
  #  lapply(htmltools::HTML)})
  
  labels_year <- reactive({paste("Parole/100000 US Adults",
        state_parole_year()$state, state_parole_year()$number_on_parole_per_100000_us_adult_residents)})

  
  
  # maybe subset by state for the color and the label
  output$mymap <- renderLeaflet({
    state_map %>%
      addTiles()%>%
      addPolygons(fillColor = ~ pal(state_parole_year()$number_on_parole_per_100000_us_adult_residents),
                  fillOpacity = 1,
                  color = "blue",
                  opacity = 0.1,
                  weight = 1,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "blue",
                    fillOpacity = .2,
                    bringToFront = TRUE),
                  label = labels_year())#%>% #labels() --- reactive 
    
    
      
      
      
      
      
      
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

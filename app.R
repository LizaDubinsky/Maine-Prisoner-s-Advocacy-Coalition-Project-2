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
#fixing the data set where there was repetition.
state_parole$year[633]<- strtoi(1999)
state_parole$year[648]<- strtoi(1999)

#%%%%%%%% Merging Labels
state_parole$label <- ""
for(i in 1:nrow(state_parole))
{
  state_parole$label[i]<- paste(state_parole$state[i], state_parole$number_on_parole_per_100000_us_adult_residents[i], sep = ": ")
  
  state_parole$label[i]<- paste(state_parole$label[i], "# on Parole/100,000 Adults", sep = " ")
}
#%%%%%%%%



#this is for changing color 
bins <- c(0, 100, 200, 300, 400, 500, 600, 700, 800, 2000)
pal <- colorBin("Greens", domain = state_parole$number_on_parole_per_100000_us_adult_residents, bins = bins)

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

#%%%%%%%%%%%%%%%%%%%%%%%%%%
#County leaflet map 
county_map <- joint_info_map%>%
  leaflet() %>%
  addTiles()%>%
  setView(lng = -80,
          lat = 34.5,
          zoom = 4) %>%
  addPolygons(fillColor = ~pal(total_jail_pop/(total_pop/1000)),
              fillOpacity = 1,
              color = "blue",
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
    
    #Drop Down Menu for Counties################################################
    selectInput(
      inputId = "data_choice",
      label= "Data Selection",
      choices=c("Proportion of Jail Population", "Proportion of Violent Crimes", 
                 "Proportion of Non-Violent Crimes"),
      selected = NULL,
      multiple = FALSE,
      selectize = TRUE,
      width = 400,
      size = 3
    ),
    ############################################################################
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("mymap")#,
      #leafletOutput("countymap")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  #Reactive year selection for slider state map
  state_parole_year <- reactive({
    state_parole %>%
      filter(year == year(input$year))
    })
  # reactive year selection for slider county map 
  # maybe join the reactive above
 # county_pop_year <- reactive({
  #  joint_info_map %>%
   #   filter(year == year(input$year))
  #})
  
  # Reactive Labels for leaflet map 
  labels_year <- reactive({ sprintf("<strong>%s</strong><br/>%g Parole/100000 US Adults",
                    state_parole_year()$state, state_parole_year()$number_on_parole_per_100000_us_adult_residents) %>% 
    lapply(htmltools::HTML)})
  
  # county labels 
  #labels_year_county <- sprintf("<strong>%s</strong><br/>%g Total Jail Pop/Total Pop",
   #                             county_pop_year()$county_name, county_pop_year()$total_jail_pop/(county_pop_year()$total_pop/1000)) %>% 
    #lapply(htmltools::HTML)
  
  output$mymap <- renderLeaflet({
    leaflet(data = state_parole_year()) %>%
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
        opacity = 1) %>%
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
                  label = ~labels_year())
    
    
  })
  
  
}

 
# Run the application 
shinyApp(ui = ui, server = server)

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

#install.packages("xaringanthemer")
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

incar <- read.csv("incarceration_trends.csv")
year_1970 <- incar


year_1970<- year_1970 %>%
  mutate(county_name = str_to_lower(str_replace_all(county_name, pattern = " County", replacement = "")))

fips_new_england <- c("09","23","25","33","50","44")
all_counties<- all_counties[which(all_counties$STATE %in% fips_new_england),]


all_counties <- st_as_sf(all_counties)
all_counties <- all_counties %>%
  mutate(NAME = str_to_lower(NAME))


joint_info_map <- year_1970 %>%
  left_join(all_counties, by = c("county_name" = "NAME")) %>%
  st_as_sf()
#get somethign that gets the reads the 01 , 02 etc as the states, fips 
# list of fips, filter out, and matching the states with fips 
new_england <- c("ME","VT","NH","MA","CT","RI")

joint_info_map<- joint_info_map[which(joint_info_map$state %in% new_england),]

joint_info_map  <- joint_info_map  %>%
 # filter(year %in% c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2017,2016,2018))%>%
  mutate(total_pop = total_pop)

#this is for changing color 
bins <- c(0, 0.5,1,2,4,15,194)
pal <- colorBin("Blues", domain = joint_info_map$total_jail_pop/(joint_info_map$total_pop)*1000, bins = bins)

labels_1 <- sprintf("<strong>%s</strong><br/>%g Number of people in jail per a 1000",
                    joint_info_map$county_name, joint_info_map$total_jail_pop/(joint_info_map$total_pop)*1000) %>% 
  lapply(htmltools::HTML)

#%%%%%%%%%%%%%%%
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Ratio of Jail Population per 1000 people in New England Counties"), 
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year",
                  label = "Year:",
                  min = lubridate::ymd("19700101"),
                  max = lubridate::ymd("20180101"),
                  value = lubridate::ymd("20000101"),
                  step = 1,
                  timeFormat = "%Y")
    ),
    
    # Show a plot of the generated distribution
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


  #county labels
  labels_year_county <- reactive({ sprintf("<strong>%s</strong><br/>%g Total Jail Pop/Total Pop",
                               county_year()$county_name, county_year()$total_jail_pop/(county_year()$total_pop)*1000) %>%
  lapply(htmltools::HTML)})

  output$countymap <- renderLeaflet({
    leaflet(data = county_year()) %>%
      addTiles() %>%
      setView(lng = -70,
              lat = 44.3,
              zoom = 6) %>%
      addPolygons(fillColor = ~ pal(joint_info_map$total_jail_pop/(joint_info_map$total_pop)*1000),
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
        pal = pal,
        values = ~total_jail_pop/(total_pop)*1000,
        title = "Ratio of Jail Pop per 1000",
        opacity = 1) %>%
      addTiles() %>%
      addPolygons(fillColor = ~ pal(county_year()$total_jail_pop/(county_year()$total_pop)*1000),
                  fillOpacity = 1,
                  color = "black",
                  opacity = 1,
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




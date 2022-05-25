library(shiny)
library(dplyr)
library(lubridate)

#Install Packages for Leaflet %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

knitr::opts_chunk$set(echo = TRUE)


library(tidyverse)
library(tidymodels)
library(knitr)
library(xaringanthemer)
library(maps)
library(ggplot2)
library(tidyverse)
library(corrplot)
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
  
  
  #Reactive year selection for slider state map
  state_parole_year <- reactive({
    state_parole %>%
      filter(year == year(input$year))
  })
  
  # Reactive Labels for leaflet map 
  labels_year <- reactive({ sprintf("<strong>%s</strong><br/>%g Parole/100000 US Adults",
                                    state_parole_year()$state, state_parole_year()$number_on_parole_per_100000_us_adult_residents) %>% 
      lapply(htmltools::HTML)})
  
  
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

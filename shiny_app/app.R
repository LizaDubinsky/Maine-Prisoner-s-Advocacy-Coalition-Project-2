library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Maine Prisoner Advocacy Coalition"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "year",
                        label = "Year:",
                        min = lubridate::ymd("19700101"),
                        max = lubridate::ymd("20160101"),
                        value = lubridate::ymd("20000101"),
                        step = 1,
                        timeFormat = "%Y")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("mymap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mymap <- renderLeaflet({
        
       
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

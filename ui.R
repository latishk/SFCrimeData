library(leaflet)
require(lubridate)
library(plotly)
library(shiny)
#attach("myworkspace.RData")
navbarPage("SF Crime Data", id="nav",
           
  tabPanel("Interactive Map",
    div(class="outer",
        tags$head(
          includeCSS("styles.css")
        ),
        leafletOutput("map", width="100%", height="100%"),
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 60, left = "20", right = "auto", bottom = "auto",
                      width = 400, height = "auto",
                      h2("Sample Data of 10000 rows"),
                      sliderInput(inputId = "year", label = "Slide to Change the year", value = 2014, min = min(year(sfcrime$Date)), max = 2015),
                      h4("Zoom in and click to see more info")
        )
        
    ),
    tags$div(id="cite",'Data From SF Open Data ', tags$em('SFPD: Crime Incidents'), ' by https://data.sfgov.org/browse?category=Public+Safety.')
        
  ),
  tabPanel("Exploratory Data analysis", 
    # plotOutput("districtwiseCrimeDistribution", hover = "plot1_click"),
    # verbatimTextOutput("info"),
    plotlyOutput("districtwiseCrimeDistribution1", width = "100%", height = "700px"),
    textInput(inputId = "add", label ="Input Address", value = "2300 Block of 24TH AV"),
    numericInput(inputId="k", label = "Input K to show K most likely Crimes based on stats in 1 mile square", value = 5),
    actionButton("go", "Plot"),
    plotlyOutput("k_nearest"),
    plotlyOutput("k_nearest_time", width = "100%", height = "700px")
  )
      
)

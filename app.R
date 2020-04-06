#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(DT)

# Define UI for application that draws a histogram
ui <- navbarPage(title = "COVID-19 Monitor", 
                 tabPanel("Global Crisis Map", 
                          tabsetPanel(
                              tabPanel("Confirmed Cases", 
                                       column(
                                           width = 9, 
                                           leafletOutput("confirmedMap")
                                       ), 
                                       column(
                                           width = 3, 
                                           dataTableOutput("confirmedTable")
                                       )
                              ), 
                              tabPanel("Death Cases", 
                                       column(
                                           width = 9, 
                                           leafletOutput("deathsMap")
                                       ), 
                                       column(
                                           width = 3, 
                                           dataTableOutput("deathsTable")
                                       )
                              ), 
                              tabPanel("Recovered Cases", 
                                       column(
                                           width = 9, 
                                           leafletOutput("recoveredMap")
                                       ), 
                                       column( 
                                           width = 3, 
                                           dataTableOutput("recoveredTable")
                                       )
                              ),
                              tabPanel("Active Cases", 
                                       column(
                                           width = 9, 
                                           leafletOutput("activeMap")
                                       ), 
                                       column(
                                           width = 3, 
                                           dataTableOutput("activeTable")
                                       )
                              )
                          )
                          ),
                 tabPanel("Forecast", 
                          ), 
                 tabPanel("Viz", 
                          )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
}

# Run the application 
shinyApp(ui = ui, server = server)

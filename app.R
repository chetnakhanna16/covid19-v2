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
library(tidyverse)

get_data <- function(){
    all.cases <- read_csv("dataset.csv")
    return(all.cases)
}

# Define UI for application that draws a histogram
ui <- navbarPage(title = "COVID-19 Monitor", 
                 tabPanel("Global Crisis Map", 
                          fluidRow(
                              textOutput("countryCount"),
                              textOutput("confirmedCount"), 
                              textOutput("recoveredCount"), 
                              textOutput("deathsCount"), 
                              textOutput("activeCount"), 
                              textOutput("hitsCount")
                          ), 
                          fluidRow(
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
                          )
                          ),
                 tabPanel("Forecast", 
                          ), 
                 tabPanel("Viz", 
                          )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    df <- get_data() %>% 
        filter(Date == "2020-04-06")
    
    output$countryCount <- renderText(nrow(df)) 
    
    output$confirmedCount <- renderText(sum(df$Confirmed)) 
    
    output$deathsCount <- renderText(sum(df$Deaths)) 
    
    output$recoveredCount <- renderText(sum(df$Recovered)) 
    
    output$activeCount <- renderText(sum(df$Active))
    
    output$confirmedMap <- renderLeaflet(
        leaflet(df, options = leafletOptions(minZoom = 1, maxZoom = 5, worldCopyJump = TRUE)) %>%  
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Confirmed*5, label = ~as.character(paste(Country, "-", 
                                                                               "Confirmed: ", Confirmed)), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.3, color = "red") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) %>% 
            setMaxBounds(-180, 90, 180, -90) 
    )
    
    output$deathsMap <- renderLeaflet(
        leaflet(df, options = leafletOptions(minZoom = 1, maxZoom = 5, worldCopyJump = TRUE)) %>% 
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Deaths*5, label = ~as.character(paste(Country, "-", 
                                                                            "Deaths: ", Deaths)), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.3, color = "purple") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) %>% 
            setMaxBounds(-180, 90, 180, -90) 
    )
    
    output$recoveredMap <- renderLeaflet(
        leaflet(df, options = leafletOptions(minZoom = 1, maxZoom = 5, worldCopyJump = TRUE)) %>% 
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Recovered*5, label = ~as.character(paste(Country, "-", 
                                                                               "Recovered: ", Recovered)), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.3, color = "green") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) %>% 
            setMaxBounds(-180, 90, 180, -90) 
    )
    
    output$activeMap <- renderLeaflet(
        leaflet(df, options = leafletOptions(minZoom = 1, maxZoom = 5, worldCopyJump = TRUE)) %>% 
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Active*5, label = ~as.character(paste(Country, "-", 
                                                                            "Active: ", Active)), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.3, color = "orange") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) %>% 
            setMaxBounds(-180, 90, 180, -90) 
    )
    
    output$confirmedTable <- DT::renderDataTable(
        datatable(
            df %>% 
                group_by(Country) %>% 
                summarise(Total = sum(Confirmed)) %>% 
                arrange(-Total) %>% 
                head(10), 
            rownames = FALSE, 
            options = list(searching = FALSE, 
                           paging = FALSE, 
                           info = FALSE)
        ) %>% 
            formatStyle(
                c('Country', 'Total'), 
                color = 'black', backgroundColor = NULL, fontWeight = 'bold'
            )
    )
    
    output$deathsTable <- DT::renderDataTable(
        datatable(
            df %>% 
                group_by(Country) %>% 
                summarise(Total = sum(Deaths)) %>% 
                arrange(-Total) %>% 
                head(10), 
            rownames = FALSE, 
            options = list(searching = FALSE, 
                           paging = FALSE, 
                           info = FALSE)
        ) %>% 
            formatStyle(
                c('Country', 'Total'), 
                color = 'black', backgroundColor = NULL, fontWeight = 'bold'
            )
    )
    
    output$recoveredTable <- DT::renderDataTable(
        datatable(
            df %>% 
                group_by(Country) %>% 
                summarise(Total = sum(Recovered)) %>% 
                arrange(-Total) %>% 
                head(10), 
            rownames = FALSE, 
            options = list(searching = FALSE, 
                           paging = FALSE, 
                           info = FALSE)
        ) %>% 
            formatStyle(
                c('Country', 'Total'), 
                color = 'black', backgroundColor = NULL, fontWeight = 'bold'
            )
    )
    
    output$activeTable <- DT::renderDataTable(
        datatable(
            df %>% 
                group_by(Country) %>% 
                summarise(Total = sum(Active)) %>% 
                arrange(-Total) %>% 
                head(10), 
            rownames = FALSE, 
            options = list(searching = FALSE, 
                           paging = FALSE, 
                           info = FALSE)
        ) %>% 
            formatStyle(
                c('Country', 'Total'), 
                color = 'black', backgroundColor = NULL, fontWeight = 'bold'
            )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

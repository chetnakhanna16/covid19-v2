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
                          tabsetPanel(
                              tabPanel("Confirmed Cases", 
                                       leafletOutput("confirmedMap", width = "100%", height = 600), 
                                       absolutePanel(top = 250, left = 50, 
                                                     class = "panel panel-default", 
                                                     width = 200, fixed = TRUE,
                                                     draggable = TRUE, height = "auto", 
                                                     dataTableOutput("confirmedTable")
                                       )
                              ), 
                              tabPanel("Death Cases", 
                                       leafletOutput("deathsMap", width = "100%", height = 600), 
                                       absolutePanel(top = 250, left = 50, 
                                                     class = "panel panel-default", 
                                                     width = 200, fixed = TRUE,
                                                     draggable = TRUE, height = "auto", 
                                                     dataTableOutput("deathsTable")
                                       )
                              ), 
                              tabPanel("Recovered Cases", 
                                       leafletOutput("recoveredMap", width = "100%", height = 600), 
                                       absolutePanel(top = 250, left = 50, 
                                                     class = "panel panel-default", 
                                                     width = 200, fixed = TRUE,
                                                     draggable = TRUE, height = "auto", 
                                                     dataTableOutput("recoveredTable")
                                       )
                              ),
                              tabPanel("Active Cases", 
                                       leafletOutput("activeMap", width = "100%", height = 600), 
                                       absolutePanel(top = 250, left = 50, 
                                                     class = "panel panel-default", 
                                                     width = 200, fixed = TRUE,
                                                     draggable = TRUE, height = "auto", 
                                                     dataTableOutput("activeTable")
                                       )
                              )
                          ), 
                          absolutePanel(
                              top = 175, right = 50, 
                              class = "panel panel-default", 
                              width = 150, fixed = TRUE,
                              draggable = TRUE, height = "auto", 
                              textOutput("countryCount"),
                              textOutput("confirmedCount"), 
                              textOutput("recoveredCount"), 
                              textOutput("deathsCount"), 
                              textOutput("activeCount"), 
                              textOutput("hitsCount"),
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
                       radius = ~Confirmed*4, label = ~as.character(paste(Country, "-", 
                                                                               "Confirmed: ", Confirmed)), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.6, color = "aqua") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) %>% 
            setMaxBounds(-180, 90, 180, -90) %>% 
            addProviderTiles("CartoDB.DarkMatter") %>% 
            addMiniMap(
                tiles = providers$CartoDB.DarkMatter,
                toggleDisplay = TRUE
            )
    )
    
    output$deathsMap <- renderLeaflet(
        leaflet(df, options = leafletOptions(minZoom = 1, maxZoom = 5, worldCopyJump = TRUE)) %>% 
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Deaths*30, label = ~as.character(paste(Country, "-", 
                                                                            "Deaths: ", Deaths)), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.6, color = "red") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) %>% 
            setMaxBounds(-180, 90, 180, -90) %>% 
            addProviderTiles("CartoDB.DarkMatter") %>% 
            addMiniMap(
                tiles = providers$CartoDB.DarkMatter,
                toggleDisplay = TRUE
            )
    )
    
    output$recoveredMap <- renderLeaflet(
        leaflet(df, options = leafletOptions(minZoom = 1, maxZoom = 5, worldCopyJump = TRUE)) %>% 
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Recovered*20, label = ~as.character(paste(Country, "-", 
                                                                               "Recovered: ", Recovered)), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.6, color = "green") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) %>% 
            setMaxBounds(-180, 90, 180, -90) %>% 
            addProviderTiles("CartoDB.DarkMatter") %>% 
            addMiniMap(
                tiles = providers$CartoDB.DarkMatter,
                toggleDisplay = TRUE
            )
    )
    
    output$activeMap <- renderLeaflet(
        leaflet(df, options = leafletOptions(minZoom = 1, maxZoom = 5, worldCopyJump = TRUE)) %>% 
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Active*4, label = ~as.character(paste(Country, "-", 
                                                                            "Active: ", Active)), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.6, color = "yellow") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) %>% 
            setMaxBounds(-180, 90, 180, -90) %>% 
            addProviderTiles("CartoDB.DarkMatter") %>% 
            addMiniMap(
                tiles = providers$CartoDB.DarkMatter,
                toggleDisplay = TRUE
            )
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

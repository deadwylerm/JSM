library(leaflet)
library(shiny)
library(dplyr)
library(sp)
library(maptools)
library(tidyverse)

load("streams2.rda")
load("df.rda")
load("pts.rda")



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "JSM Habitat Suitability:",
                        min = min(df$JSM_suitab),
                        max = max(df$JSM_suitab),
                        value = mean(df$JSM_suitab))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("MapPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$MapPlot <- renderLeaflet( {

        tmp <- subset( streams2, JSM_suitab >= input$bins )
        
        df %>%
            mutate( Points = COMID %in% pts$COMID ) %>%
            filter( JSM_suitab >= input$bins) -> df1

        leaflet() %>%
            setView(lng = -79, lat = 38, zoom = 8) %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
            addMarkers(data = df1 %>% filter( Points == TRUE), 
                       lng = ~Longitude, lat = ~Latitude) %>%
            addPolylines(data=tmp, color = "#098F92", weight = 1.5, opacity = 1.0 ) %>%
            addCircleMarkers(data=df,label = ~JSM_suitab, opacity = 1, color = "#611708", weight = 2, fillOpacity = 0, popup = ~GNIS_NAME, group = "scores_close") %>%
            groupOptions("scores_close", zoomLevels = 12:18) %>%
            addCircleMarkers(data=df,label = ~JSM_suitab, opacity = 0, fillOpacity = 0, popup = ~GNIS_NAME, group = "scores_far") %>%
            groupOptions("scores_far", zoomLevels = 1:11)

        })

    }

# Run the application 
shinyApp(ui = ui, server = server)


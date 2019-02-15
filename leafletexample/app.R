library(shiny)
library(dplyr)
library(leaflet)
library(gamair) # for mack dataset
#library(RColorBrewer)

# User interface
ui <- fluidPage(
  titlePanel("Showing spatial data with leaflet"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput(inputId = "sst_range",
                  label = "Sea surface temperature range",
                  min = 11.4, max = 19.7, 
                  value = c(12,16) ) # Defaults for slider with a range
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
                 leafletOutput("mymap"),
                 p()), 
        tabPanel("Summary", 
                 verbatimTextOutput("summary"))
      )
      )
    )
  )


server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    data(mack)
    fishpal <- colorFactor(RColorBrewer::brewer.pal(5,name = "Dark2"),mack$country)
    
    leaflet(data = subset(mack,egg.dens>0 & temp.surf > input$sst_range[1] & temp.surf < input$sst_range[2])) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon,~lat,
        radius = ~egg.dens/60,
        color = ~fishpal(country),
        stroke = FALSE, fillOpacity = 0.5
      )
  })
}

shinyApp(ui, server)
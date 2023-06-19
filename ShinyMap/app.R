
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(bslib)
ui <- bootstrapPage(
  
  
)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("test","Bonjour:",c("Salut", "Comment va ?"))
  )
)


server <- function(input, output, session) {
  
  points <- eventReactive(input$xvar, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
}

shinyApp(ui, server)

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(bslib)

shp<-read_sf("data/Sites_presentation_EN.shp") %>% 
  mutate(ID=c(1:nrow(.)))

shp_xy<-data.frame(st_coordinates(st_cast(shp$geometry,"POINT"))) %>% 
  mutate(ID=c(1:nrow(.)))

df_shp<-shp %>% 
  dplyr::select(-geometry) %>% 
  left_join(shp_xy, by="ID") %>% 
  dplyr::select(-c(ID))

Project <- gsub(", ",",",df_shp$Project) 
Project <- c("All",sort(unique(unlist(strsplit(Project,",")))))

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"), 
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("selected_project","Select a project:",Project, selected = "All")
               )

)


server <- function(input, output) {
  
  temp <- reactive({
    
    if(input$selected_project != "All"){
          df_shp %>% 
      dplyr::filter(str_detect(Project,input$selected_project[[1]]))
    }else
    {
      df_shp
    }


  
     })
   output$map <- renderLeaflet({   
  
  temp2 <-temp()
  
  meanX<-mean(temp2$X)
  meanY<-mean(temp2$Y)
if (nrow(temp2)>1) {
  lng1=min(temp2$X)
  lng2=max(temp2$X)
  lat1=min(temp2$Y)
  lat2=max(temp2$Y)
}else{
  lng1=temp2$X-(temp2$X/100)
  lng2=temp2$X+(temp2$X/100)
  lat1=temp2$Y-(temp2$Y/100)
  lat2=temp2$Y+(temp2$Y/100) 
}  


    leaflet(temp2) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      setView(lng = 50, lat = 400 , zoom = 5) %>%
      flyToBounds(lng1 = lng1, lat1 = lat1, lng2 = lng2, lat2 = lat2, options = list()) %>% 
      addMarkers(~X, ~Y, label = temp2$Name, popup = temp2$Descr,clusterOptions = markerClusterOptions()) %>% 
      addProviderTiles(providers$Esri.NatGeoWorldMap)
  
  })  

}

shinyApp(ui, server)
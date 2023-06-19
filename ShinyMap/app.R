
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

Description <- gsub(", ",",",df_shp$Descr) 
Description <- c("All",sort(unique(unlist(strsplit(Description,",")))))

Interest <- gsub(", ",",",df_shp$Interest) 
Interest <- c("All",sort(unique(unlist(strsplit(Interest,",")))))

Tool <- gsub(", ",",",df_shp$Tool) 
Tool <- c("All",sort(unique(unlist(strsplit(Tool,",")))))

shp_OFB<-read_sf("data/TWB_OFB.shp")

ui <- bootstrapPage(
  tags$head(includeCSS("styles.css")),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"), 
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(id = "controls",top = 10, right = 10,
                h3("Select a project:"),
                selectInput("selected_project","",Project, selected = "All"),
                h3("Select a Goal:"),
                selectInput("selected_descr","",Description, selected = "All"),
                h3("Select an interest:"),
                selectInput("selected_interest","",Interest, selected = "All"),
                h3("Select a tool:"),
                selectInput("selected_tool","",Tool, selected = "All")
               )

)


server <- function(input, output) {
  
  
  temp <- reactive({
    
    if(input$selected_project != "All"){
      df_shp<-df_shp %>% 
        dplyr::filter(str_detect(Project,input$selected_project[[1]]))
    }
    
    if(input$selected_descr != "All"){
      df_shp<-df_shp %>% 
        dplyr::filter(str_detect(Descr,input$selected_descr[[1]]))
    }
    
    if(input$selected_interest != "All"){
      df_shp<-df_shp %>% 
        dplyr::filter(str_detect(Interest,input$selected_interest[[1]]))
    }

    if(input$selected_tool != "All"){
      df_shp<-df_shp %>% 
        dplyr::filter(str_detect(Tool,input$selected_tool[[1]]))
    }
    
    df_shp
    
    
    
  })
  
   output$map <- renderLeaflet({   
  
  leaflet() %>%
    addTiles() %>%   # Add default OpenStreetMap map tiles
    
    # flyToBounds(lng1 = lng1, lat1 = lat1, lng2 = lng2, lat2 = lat2, options = list()) %>% 
    # setView(lng = 50, lat = 40 , zoom = 10) %>%
    # addMarkers(~X, ~Y, label = temp2$Name, popup = temp2$Descr,clusterOptions = markerClusterOptions()) %>% 
    addProviderTiles(providers$Esri.WorldImagery)
  
  })  
   
   observe({

     
     temp2 <-temp()
     
     meanX<-mean(temp2$X)
     meanY<-mean(temp2$Y)
     
     if (input$selected_project != "Office Français de la Biodiversité"){
       if (nrow(temp2)>1) {
         lng1=min(temp2$X)
         lng2=max(temp2$X)
         lat1=min(temp2$Y)
         lat2=max(temp2$Y)
       }else{
         lng1=temp2$X-(temp2$X/50)
         lng2=temp2$X+(temp2$X/50)
         lat1=temp2$Y-(temp2$Y/50)
         lat2=temp2$Y+(temp2$Y/50) 
       } 
     }else{
       lng1=-5
       lng2=3
       lat1=43
       lat2=51
     }
     
     awesome_marker <- makeAwesomeIcon(
       icon = "drone",
       iconColor = "black",
       markerColor = "red",
       library = "fa"
       
     )
     
     rendered_map<-leafletProxy("map", data = temp2) %>% 
       clearMarkers() %>%
       clearMarkerClusters() %>% 
       clearShapes() %>% 
       flyToBounds(lng1 = lng1, lat1 = lat1, lng2 = lng2, lat2 = lat2) %>% 
       addAwesomeMarkers(data  = temp2, lng = ~X, lat = ~Y, label = ~Name, popup = ~Descr,icon = awesome_marker, clusterOptions = markerClusterOptions())
       
     if(input$selected_project == "Office Français de la Biodiversité"){
       rendered_map<- rendered_map %>% 
         addPolygons(data = shp_OFB, fillColor = "yellow" , color = "#EABE5F")
         
     }
     
    rendered_map
     
   })
   
   

}

shinyApp(ui, server)
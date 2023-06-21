
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
  absolutePanel(id = "controls", class = "panel panel-default",top = 10, right = 10,
                h3("Select a project:"),
                selectInput("selected_project","",Project, selected = "All"),
                h3("Select a Goal:"),
                selectInput("selected_descr","",Description, selected = "All"),
                h3("Select an interest:"),
                selectInput("selected_interest","",Interest, selected = "All"),
                h3("Select a tool:"),
                selectInput("selected_tool","",Tool, selected = "All"),
                actionButton("buttonReset", "Reset filters")
               ),
  absolutePanel(bottom = 10, left = 10, width = "35%",
                uiOutput("Media")
                ),
  uiOutput("textprj")
)




server <- function(input, output,session) {

  
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
         lng1=min(temp2$X)-(min(temp2$X)/80)
         lng2=max(temp2$X)+(max(temp2$X)/80)
         lat1=min(temp2$Y)-(min(temp2$Y)/80)
         lat2=max(temp2$Y)+(max(temp2$Y)/80)
       }else{
         lng1=temp2$X-(temp2$X/100)
         lng2=temp2$X+(temp2$X/100)
         lat1=temp2$Y-(temp2$Y/100)
         lat2=temp2$Y+(temp2$Y/100) 
       } 
     }else{
       lng1=-6
       lng2=2
       lat1=43
       lat2=51
     }
     
     awesome_marker <- makeAwesomeIcon(
       icon = "drone",
       iconColor = "black",
       markerColor = "red",
       library = "fa"
       
     )
     
     Project_updated <- gsub(", ",",",temp2$Project) 
     Project_updated <- c("All",sort(unique(unlist(strsplit(Project_updated,",")))))
     
     Description_updated <- gsub(", ",",",temp2$Descr) 
     Description_updated <- c("All",sort(unique(unlist(strsplit(Description_updated,",")))))
     
     Interest_updated <- gsub(", ",",",temp2$Interest) 
     Interest_updated <- c("All",sort(unique(unlist(strsplit(Interest_updated,",")))))
     
     Tool_updated <- gsub(", ",",",temp2$Tool) 
     Tool_updated <- c("All",sort(unique(unlist(strsplit(Tool_updated,",")))))
     
     updateSelectizeInput(session, "selected_project","",Project_updated, selected = input$selected_project)
     updateSelectizeInput(session, "selected_descr","",Description_updated, selected = input$selected_descr)
     updateSelectizeInput(session, "selected_interest","",Interest_updated, selected = input$selected_interest)
     updateSelectizeInput(session, "selected_tool","",Tool_updated, selected = input$selected_tool)
     
     
     rendered_map<-leafletProxy("map", data = temp2) %>% 
       clearMarkers() %>%
       clearMarkerClusters() %>% 
       clearShapes() %>% 
       flyToBounds(lng1 = lng1, lat1 = lat1, lng2 = lng2, lat2 = lat2) %>% 
       addAwesomeMarkers(data  = temp2, lng = ~X, lat = ~Y, label = ~Name, popup = ~Descr,icon = awesome_marker, clusterOptions = markerClusterOptions())
       
     if(input$selected_project == "Office Français de la Biodiversité"){
       rendered_map<- rendered_map %>% 
         addPolygons(data = shp_OFB, label = ~nom, fillColor = "yellow" , color = "#EABE5F")
         
     }
     
    rendered_map
    
    })
   
   output$Media <- renderUI({
     
     if (input$selected_project == "BiCOME") {
       tags$video(type = "video/mp4", src = "BiCOME.MP4", controls = TRUE,width="100%", autoplay = TRUE, replay = T)
     }else{
       if (input$selected_project == "CoastOBS") {
         tags$video(type = "video/mp4", src = "CoastObs.mp4", controls = TRUE,width="100%", autoplay = TRUE, replay = T)
       }
     }
   })
   
   

   observeEvent(input$buttonReset, {
     updateSelectizeInput(session, "selected_project","",Project, selected = "All")
     updateSelectizeInput(session, "selected_descr","",Description, selected = "All")
     updateSelectizeInput(session, "selected_interest","",Interest, selected = "All")
     updateSelectizeInput(session, "selected_tool","",Tool, selected = "All")
    
     
   })

   observeEvent(input$selected_project, {  
     if (input$selected_project == "BiCOME") {
     output$textprj <- renderUI({
      list(
        absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                      h1(a(href="https://www.bicome.info", "BiCOME"), align = "center"),
                      h3("    The project is one of three studies that form part of the European Space Agency's ",a(href="https://www.bicome.info/Biodiversity_precursors_en", "Biodiversity+ Precursors"),
                         " on ",a(href="https://www.eo4diversity.info", "Terrestrial (EO4Diversity),"),
                         a(href="https://www.eo4diversity.info", "Freshwater (BIOMONDO)"), " and Coastal ecosystems (BiCOME).", align = "justify"),
                      h1(""),
                      h3("This project aims to develop and demonstrate that Essential Biodiversity Variables",
                         a(href="https://www.science.org/doi/10.1126/science.1229931#:~:text=species%20and%20locations.-,Essential%20Biodiversity%20Variables%20in%20Practice,and%20management%20of%20biodiversity%20change.&text=Dozens%20of%20biodiversity%20variables%20were,sensitivity%2C%20feasibility%2C%20and%20relevance.", "(EBVs, Pereira et al., 2013)"),
                         "relevant for scientific and monitoring applications, can be obtained from state-of-the-art remotely sensed reflectance close to the shoreline,
                         and that they can be scalable globally. By addressing relevant scientific and societal problems.", align = "justify"),
                      h1(""),
                      # h3("As part as the intertidal study case, one of our goal was to develop an algorithm to discriminate accuratly intertidal green macrophytes. 
                      #    The main limitation of intertidal vegetation mapping is that taxonomicaly different vegatation types can share the same pigment compostion and therefore be difficult to distinguish using the spectral informations retrieved from remote sensing.", align = "justify"),
                      # h3(""),
                      h3(a(href="https://www.bicome.info", "Know more about this project"))
                      ),
        absolutePanel(bottom = "5%", left = "88%", width = "10%",
                      a(href="http://bicome.info", img(src="BiCOME_Logo.png", width = "100%"))
                      ),
        absolutePanel(bottom = "9%", left = "75%", width = "10%",
                      a(href="https://www.esa.int", img(src="ESA_logo.png", width = "100%"))
                      ),
        absolutePanel(bottom = "8%", left = "63%", width = "10%",
                      a(href="https://www.dlr.de/de", img(src="DLR_logo.png", width = "90%"))
                      ),
        absolutePanel(bottom = "7%", left = "55%", width = "10%",
                      a(href="https://isomer.univ-nantes.fr", img(src="UN_Logo.png", width = "50%"))
                      ),
        absolutePanel(bottom = "25%", left = "77%", width = "20%",
                      a(href="https://www.pml.ac.uk", img(src="PML_logo.png", width = "100%"))
                      )
        )
     })
     
     }else{
       if (input$selected_project == "Office Français de la Biodiversité") {
         output$textprj <- renderUI({
           list(
         absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                       h1(a(href="https://www.ofb.gouv.fr", "OFB"), align = "center"),
                       h3("In order to establish the ecological status of transitional water bodies, 
                          the European Water Framework Directive (WFD) is based on the evaluation of a certain number of biological quality elements, 
                          as well as physicochemical parameters supporting biology. As phytoplankton monitoring is not considered relevant due to the high turbidity characterizing the large estuaries in mainland France and overseas departments (French Guiana), 
                          the possibility of using microphytobenthos as a biological indicator of the ecological status of estuaries is being explored as a possible alternative.", align = "justify"),
                       h3(""),
                       h3("As part of this project, my goal was to map microphytobenthic biofilms over 42 french estuaries of the Altantic coastlines. 
                          Sentinel-2 data from 2018 to 2020 were used and a random forest classifier was app^lied to discriminate microphytobenthos from other kind of intertidal vegetation.
                          The spatio-temporal variability of biofilm across the 42 estuaries has then being compared to anthropogenic pressures to try to build a bio-indicator."),
                       h3(a(href="https://www.bicome.info", "Know more about this project"))
                      ),
         absolutePanel(bottom = 10, left = 10, width = "35%",
                       img(src="MPB_sampling.jpg", width = "100%")
                      ),
         absolutePanel(bottom = "5%", left = "88%", width = "10%",
                       a(href="https://www.ofb.gouv.fr", img(src="OFB_logo.png", width = "100%"))
                      )
               )
         })
         
       }else{
         if (input$selected_project == "CoastOBS") {
           output$textprj <- renderUI({
             list(
               absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                             h1(a(href="https://coastobs.eu", "CoastObs"), align = "center"),
                             h3("In order to establish the ecological status of transitional water bodies, 
                          the European Water Framework Directive (WFD) is based on the evaluation of a certain number of biological quality elements, 
                          as well as physicochemical parameters supporting biology. As phytoplankton monitoring is not considered relevant due to the high turbidity characterizing the large estuaries in mainland France and overseas departments (French Guiana), 
                          the possibility of using microphytobenthos as a biological indicator of the ecological status of estuaries is being explored as a possible alternative.", align = "justify"),
                             h3(""),
                             h3("As part of this project, my goal was to map microphytobenthic biofilms over 42 french estuaries of the Altantic coastlines. 
                          Sentinel-2 data from 2018 to 2020 were used and a random forest classifier was app^lied to discriminate microphytobenthos from other kind of intertidal vegetation.
                          The spatio-temporal variability of biofilm across the 42 estuaries has then being compared to anthropogenic pressures to try to build a bio-indicator."),
                             h3(a(href="https://www.bicome.info", "Know more about this project"))
               ),
               absolutePanel(bottom = 10, left = 10, width = "35%",
                             img(src="MPB_sampling.jpg", width = "100%")
               ),
               absolutePanel(bottom = "5%", left = "88%", width = "10%",
                             a(href="https://www.ofb.gouv.fr", img(src="OFB_logo.png", width = "100%"))
               )
             )
           })
           
         }else{
         if (input$selected_project == "All") {
         output$textprj <- renderUI({
           absolutePanel(top = 10, left = 10, width = "35%")
         })
       }
       }
 
       
       }
     }
  })


}

shinyApp(ui, server)
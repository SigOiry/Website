
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(bslib)
library(shinyWidgets)

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
Project<-Project[-which(Project=="NA")]

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
                actionButton("buttonReset", "Reset filters"),
                h3("Show text, pictures and videos: "),
                switchInput(inputId = "showOverlay", value = TRUE, width ="100%")
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
    # addTiles() %>%   # Add default OpenStreetMap map tiles
    
    # flyToBounds(lng1 = lng1, lat1 = lat1, lng2 = lng2, lat2 = lat2, options = list()) %>% 
    # setView(lng = 50, lat = 40 , zoom = 10) %>%
    # addMarkers(~X, ~Y, label = temp2$Name, popup = temp2$Descr,clusterOptions = markerClusterOptions()) %>% 
    addProviderTiles(providers$Esri.WorldImagery, options = tileOptions(minZoom = 3, maxZoom = 12))
  
  })  
   
   observe({
     
     temp2 <-temp()
     
     meanX<-mean(temp2$X)
     meanY<-mean(temp2$Y)
     
     if (input$selected_project != "Office Français de la Biodiversité"){
       if (input$selected_project == "All" & input$selected_descr == "Drone survey" & input$showOverlay){
         lng1=-133
         lng2=179
         lat1=43
         lat2=-66
       }else{
         if (nrow(temp2)>1) {
           lng1=min(temp2$X)-(min(temp2$X)/150)
           lng2=max(temp2$X)+(max(temp2$X)/150)
           lat1=min(temp2$Y)-(min(temp2$Y)/150)
           lat2=max(temp2$Y)+(max(temp2$Y)/150)
         }else{
           lng1=temp2$X-(temp2$X/100)
           lng2=temp2$X+(temp2$X/100)
           lat1=temp2$Y-(temp2$Y/100)
           lat2=temp2$Y+(temp2$Y/100) 
         }
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
     Project_updated<-Project_updated[which(Project_updated != "NA")]
     
     
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

   observeEvent(input$buttonReset, {
     updateSelectizeInput(session, "selected_project","",Project, selected = "All")
     updateSelectizeInput(session, "selected_descr","",Description, selected = "All")
     updateSelectizeInput(session, "selected_interest","",Interest, selected = "All")
     updateSelectizeInput(session, "selected_tool","",Tool, selected = "All")
    
     
   })
   observe({  
     if (input$showOverlay) {
     observeEvent(input$selected_project, {  
       if (input$selected_project == "BiCOME" & input$showOverlay) {
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
                        h3(a(href="https://sigoiry.github.io/Website/about.html", "Know more about this project"))
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
       output$Media<-renderUI({
         tags$video(type = "video/mp4", src = "BiCOME.mp4", controls = TRUE,width="100%", autoplay = TRUE, muted = T)
       })
       
       }else{
         if (input$selected_project == "Office Français de la Biodiversité" & input$showOverlay) {
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
                            The spatio-temporal variability of biofilm across the 42 estuaries has then being compared to anthropogenic pressures to try to build a bio-indicator.", align = "justify"),
                         h3(a(href="https://sigoiry.github.io/Website/about.html", "Know more about this project"))
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
           if (input$selected_project == "CoastOBS" & input$showOverlay) {
             output$textprj <- renderUI({
               list(
                 absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                               h1(a(href="https://coastobs.eu", "CoastObs"), align = "center"), 
                               h3("CoastObs uses satellite remote sensing to monitor coastal water environments. ", align = "justify"),
                               h3("CoastObs products and services include algal blooms, 
                                  chlorophyll-a concentration, turbidity, seagrass per cent coverage, phytoplankton size classes, harmful algae, sediment plumes,
                                  and water surface temperature as well as integration with predictive models for products such as shellfish growth potential.", align = "justify"),
                               h3(a(href="https://sigoiry.github.io/Website/about.html", "Know more about this project"))
                 ),
                 absolutePanel(bottom = "5%", left = "88%", width = "10%",
                               a(href="https://coastobs.eu", img(src="coastobs_logo.png", width = "100%"))
                 )
               )
             })
             output$Media<-renderUI({
               tags$video(type = "video/mp4", src = "CoastObs.mp4", controls = TRUE,width="100%", autoplay = TRUE, muted = T)
             })
             
           }else{
             if (input$selected_project == "PHC NUSANTARA" & input$showOverlay) {
               output$textprj <- renderUI({
                 list(
                   absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                                 h1(a(href="https://www.campusfrance.org/en/node/2355", "PHC NUSANTARA"), align = "center"), 
                                 h3("The PHC NUSANTARA is a joint initiative between the governments of France and Indonesia,
                                    aiming to encourage collaboration on research and innovation while strengthening
                                    connections that will lead to greater collaboration in the future. This program is managed by
                                    the French Ministry for Europe and Foreign Affairs, the French Ministry of Higher Education
                                    and Research, and the Indonesian Ministry for Education, Culture, Research and Technology
                                    (KEMDIKBUDRISTEK).", align = "justify"),
                                 h1("", align = "justify"),
                                 h3("", align = "justify"),
                                 h3(a(href="https://sigoiry.github.io/Website/about.html", "Know more about this project"))
                   ),
                   absolutePanel(bottom = "5%", left = "88%", width = "10%",
                                 a(href="https://isomer.univ-nantes.fr", img(src="UN_logo.png", width = "50%"))
                   ),
                   absolutePanel(bottom = "5%", left = "78%", width = "10%",
                                 a(href="https://lp2m.unhas.ac.id", img(src="Hasanuddin_logo.png", width = "60%"))
                   ),
                   absolutePanel(bottom = 10, left = 10, width = "35%",
                                 img(src="Kapaphycus.jpeg", width = "100%")
                   )
                 )
               })
               
             }else{
                  output$textprj <- renderUI({
                   absolutePanel(top = 10, left = 10, width = "35%")
                 })
                  output$Media <- renderUI({
                    absolutePanel(top = 10, left = 10, width = "35%")
                  })
               }
           }
         }
      }
    })
       
       
     
     observeEvent(input$selected_descr, {
       if (input$selected_descr == "Drone survey" & input$showOverlay) {
         output$textprj <- renderUI({
           list(
             absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                           h1(a(href="https://sigoiry.github.io/Website/about.html", "Drone Surveys"), align = "center"), 
                           h3("Having obtained my drone pilot license in 2021, 
                              I have used this tool to perform the mapping of various intertidal habitats along the European coasts", 
                              a(href="https://sigoiry.github.io/Website/about.html", "(BiCOME)"),
                              "or aquaculture activities ",
                              a(href="https://sigoiry.github.io/Website/about.html", "(Bourgneuf Bay, France;"),
                              a(href="https://sigoiry.github.io/Website/about.html", "Punaga, Indonesia)."),
                              "As a result, I have developed expertise in processing drone data, whether it is RGB or multispectral, using photogrammetry software such as Agisoft Metashape or Pix4D. 
                              I have also had the opportunity to work with Lidar data.", align = "justify"),
                           h1(""),
                           h3("I have used :"), 
                           h3("- Micasense MX Dual Camera"),
                           h3("- Lidar DJI Zenmuse L1"),
                           h3("- DJI Matrice RTK 200 and 300"),
                           h3("- DJI Phantom 4 mutlispectral"),
                           h3("- DJI mavic 3 and Parrot Anafi"),
                           h3(a(href="https://sigoiry.github.io/Website/about.html", "Know more about my drone surveys"))
             ),
             absolutePanel(bottom = "1%", left = "0.5%", width = "35%",
                           img(src="AlexSimonOyster.jpeg", width = "100%")
             ),
             absolutePanel(bottom = "1%", left = "36%", width = "35%",
                           img(src="Drone_title.jpg", width = "100%")
             )
             
           )
         })
         
         output$Media <- renderUI({
           absolutePanel(top = 10, left = 10, width = "35%")
         })
         
         
         
       }else{
         if (input$selected_descr == "Teaching" & input$showOverlay) {
           output$textprj <- renderUI({
             list(
               absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                             h1(a(href="https://sigoiry.github.io/Website/about.html", "Teaching"), align = "center"), 
                             h3("As a member of the University of Nantes, I have had numerous opportunities to teach courses to undergraduate and master's students. 
                                In addition to classroom lectures, I have also had the chance to give field-based instruction to students, 
                                teaching them various sampling methods used in ecology and how to extrapolate these observations using satellite data.", align = "justify"),
                              h3("This practical experience has been invaluable in helping students understand the real-world applications of ecological 
                                research and how remote sensing can enhance our understanding of environmental patterns and processes.", align = "justify"),
                             h3(a(href="https://sigoiry.github.io/Website/about.html", "Know more about my teaching experiences"))
               ),
               absolutePanel(bottom = "5%", left = "88%", width = "10%",
                             a(href="https://isomer.univ-nantes.fr", img(src="UN_logo.png", width = "50%"))
               )
             )
           })
           output$Media<-renderUI({
             tags$video(type = "video/mp4", src = "Teaching.mp4", controls = TRUE,width="100%", autoplay = TRUE, muted = T)
           })
         }else{
           if (input$selected_descr == "Vegetation extent assessment" & input$showOverlay) {
             output$textprj <- renderUI({
               list(
                 absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                               h1(a(href="https://sigoiry.github.io/Website/about.html", "Vegetation extent assessment"), align = "center"),
                               h3("Often, when we conduct drone flights, we also perform measurements on the ground, which later allow us to validate the various 
                                  algorithms we develop.", align = "justify"),
                               h3("For this purpose, we have a dGPS (differential Global Positioning System) with positioning accuracy of a few centimeters.
                                  This GPS enables us to precisely geolocate the data we acquire in the field within the drone images and in the spatial context. 
                                  The acquired data can include hyperspectral data obtained using an ASD (Analytical Spectral Device), height data (e.g., height of oyster tables)
                                  , or simply quadrats that help us estimate the percentage of vegetation cover for different types of vegetation.", align = "justify"),
                               h3(a(href="https://sigoiry.github.io/Website/about.html", "Know more about my teaching experiences"))
                 ),
                 absolutePanel(bottom = "1%", left = "0.5%", width = "35%",
                               img(src="LaurentLisboa.jpg", width = "100%")
                 )
               )
             })
             output$Media <- renderUI({
               absolutePanel(top = 10, left = 10, width = "35%")
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
     
     }else{
       output$textprj <- renderUI({
         absolutePanel(top = 10, left = 10, width = "35%")
         
         output$Media <- renderUI({
           absolutePanel(top = 10, left = 10, width = "35%")
         })
       })
     }
   })


}

shinyApp(ui, server)
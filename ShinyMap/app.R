
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(bslib)
library(shinyWidgets)


# rsconnect::deployApp()

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
Interest<-Interest[-which(Interest=="None")]

Tool <- gsub(", ",",",df_shp$Tool) 
Tool <- c("All",sort(unique(unlist(strsplit(Tool,",")))))
Tool<-Tool[-which(Tool=="None")]

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
    setView(lng = -2, lat = 47 , zoom = 10) %>%
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
         if (input$selected_project == "All" & input$selected_tool == "Drone" & input$showOverlay){
           lng1=-133
           lng2=179
           lat1=63
           lat2=-46
         }else{
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
     Interest_updated<-Interest_updated[which(Interest_updated != "None")]
     
     Tool_updated <- gsub(", ",",",temp2$Tool) 
     Tool_updated <- c("All",sort(unique(unlist(strsplit(Tool_updated,",")))))
     Tool_updated<-Tool_updated[which(Tool_updated != "None")]
     
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
                        h1(a(target="_blank", href="https://www.bicome.info", "BiCOME"), align = "center"),
                        h3("    The project is one of three studies that form part of the European Space Agency's ",a(target="_blank", href="https://www.bicome.info/Biodiversity_precursors_en", "Biodiversity+ Precursors"),
                           " on ",a(target="_blank", href="https://www.eo4diversity.info", "Terrestrial (EO4Diversity),"),
                           a(target="_blank", href="https://www.eo4diversity.info", "Freshwater (BIOMONDO)"), " and Coastal ecosystems (BiCOME).", align = "justify"),
                        h1(""),
                        h3("This project aims to develop and demonstrate that Essential Biodiversity Variables",
                           a(target="_blank", href="https://www.science.org/doi/10.1126/science.1229931#:~:text=species%20and%20locations.-,Essential%20Biodiversity%20Variables%20in%20Practice,and%20management%20of%20biodiversity%20change.&text=Dozens%20of%20biodiversity%20variables%20were,sensitivity%2C%20feasibility%2C%20and%20relevance.", "(EBVs, Pereira et al., 2013)"),
                           "relevant for scientific and monitoring applications, can be obtained from state-of-the-art remotely sensed reflectance close to the shoreline,
                           and that they can be scalable globally. By addressing relevant scientific and societal problems.", align = "justify"),
                        h1(""),
                        # h3("As part as the intertidal study case, one of our goal was to develop an algorithm to discriminate accuratly intertidal green macrophytes. 
                        #    The main limitation of intertidal vegetation mapping is that taxonomicaly different vegatation types can share the same pigment compostion and therefore be difficult to distinguish using the spectral informations retrieved from remote sensing.", align = "justify"),
                        # h3(""),
                        h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about this project"))
                        ),
          absolutePanel(bottom = "5%", left = "88%", width = "10%",
                        a(target="_blank", href="http://bicome.info", img(src="BiCOME_Logo.png", width = "100%"))
                        ),
          absolutePanel(bottom = "9%", left = "75%", width = "10%",
                        a(target="_blank", target="_blank", href="https://www.esa.int", img(src="ESA_logo.png", width = "100%"))
                        ),
          absolutePanel(bottom = "8%", left = "63%", width = "10%",
                        a(target="_blank", target="_blank", href="https://www.dlr.de/de", img(src="DLR_logo.png", width = "90%"))
                        ),
          absolutePanel(bottom = "8%", left = "55%", width = "10%",
                        a(target="_blank", href="https://isomer.univ-nantes.fr", img(src="UN_Logo.png", width = "50%"))
                        ),
          absolutePanel(bottom = "25%", left = "77%", width = "20%",
                        a(target="_blank", href="https://www.pml.ac.uk", img(src="PML_logo.png", width = "100%"))
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
                         h1(a(target="_blank", href="https://www.ofb.gouv.fr", "OFB"), align = "center"),
                         h3("In order to establish the ecological status of transitional water bodies, 
                            the European Water Framework Directive (WFD) is based on the evaluation of a certain number of biological quality elements, 
                            as well as physicochemical parameters supporting biology. As phytoplankton monitoring is not considered relevant due to the high turbidity characterizing the large estuaries in mainland France and overseas departments (French Guiana), 
                            the possibility of using microphytobenthos as a biological indicator of the ecological status of estuaries is being explored as a possible alternative.", align = "justify"),
                         h3(""),
                         h3("As part of this project, my goal was to map microphytobenthic biofilms over 42 french estuaries of the Altantic coastlines. 
                            Sentinel-2 data from 2018 to 2020 were used and a random forest classifier was app^lied to discriminate microphytobenthos from other kind of intertidal vegetation.
                            The spatio-temporal variability of biofilm across the 42 estuaries has then being compared to anthropogenic pressures to try to build a bio-indicator.", align = "justify"),
                         h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about this project"))
                        ),
           absolutePanel(bottom = 10, left = 10, width = "35%",
                         img(src="MPB_sampling.jpg", width = "100%")
                        ),
           absolutePanel(bottom = "5%", left = "88%", width = "10%",
                         a(target="_blank", href="https://www.ofb.gouv.fr", img(src="OFB_logo.png", width = "100%"))
                        )
                 )
           })
           
         }else{
           if (input$selected_project == "CoastOBS" & input$showOverlay) {
             output$textprj <- renderUI({
               list(
                 absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                               h1(a(target="_blank", href="https://coastobs.eu", "CoastObs"), align = "center"), 
                               h3("CoastObs uses satellite remote sensing to monitor coastal water environments. ", align = "justify"),
                               h3("CoastObs products and services include algal blooms, 
                                  chlorophyll-a concentration, turbidity, seagrass per cent coverage, phytoplankton size classes, harmful algae, sediment plumes,
                                  and water surface temperature as well as integration with predictive models for products such as shellfish growth potential.", align = "justify"),
                               h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about this project"))
                 ),
                 absolutePanel(bottom = "5%", left = "88%", width = "10%",
                               a(target="_blank", href="https://coastobs.eu", img(src="coastobs_logo.png", width = "100%"))
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
                                 h1(a(target="_blank", href="https://www.campusfrance.org/en/node/2355", "PHC NUSANTARA"), align = "center"), 
                                 h3("The PHC NUSANTARA is a joint initiative between the governments of France and Indonesia,
                                    aiming to encourage collaboration on research and innovation while strengthening
                                    connections that will lead to greater collaboration in the future. This program is managed by
                                    the French Ministry for Europe and Foreign Affairs, the French Ministry of Higher Education
                                    and Research, and the Indonesian Ministry for Education, Culture, Research and Technology
                                    (KEMDIKBUDRISTEK).", align = "justify"),
                                 h1("", align = "justify"),
                                 h3("", align = "justify"),
                                 h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about this project"))
                   ),
                   absolutePanel(bottom = "5%", left = "88%", width = "10%",
                                 a(target="_blank", href="https://isomer.univ-nantes.fr", img(src="UN_logo.png", width = "50%"))
                   ),
                   absolutePanel(bottom = "5%", left = "78%", width = "10%",
                                 a(target="_blank", href="https://lp2m.unhas.ac.id", img(src="Hasanuddin_logo.png", width = "60%"))
                   ),
                   absolutePanel(bottom = 10, left = 10, width = "35%",
                                 img(src="Kapaphycus.png", width = "100%")
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
                           h1(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Drone Surveys"), align = "center"), 
                           h3("Having obtained my drone pilot license in 2021, 
                              I have used this tool to perform the mapping of various intertidal habitats along the European coasts", 
                              a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "(BiCOME)"),
                              "or aquaculture activities ",
                              a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "(Bourgneuf Bay, France;"),
                              a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Punaga, Indonesia)."),
                              "As a result, I have developed expertise in processing drone data, whether it is RGB or multispectral, using photogrammetry software such as Agisoft Metashape or Pix4D. 
                              I have also had the opportunity to work with Lidar data.", align = "justify"),
                           h1(""),
                           h3("I have used :"), 
                           h3("- Micasense MX Dual Camera"),
                           h3("- Lidar DJI Zenmuse L1"),
                           h3("- DJI Matrice RTK 200 and 300"),
                           h3("- DJI Phantom 4 mutlispectral"),
                           h3("- DJI mavic 3 and Parrot Anafi"),
                           h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about my drone surveys"))
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
                             h1(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Teaching"), align = "center"), 
                             h3("As a member of the University of Nantes, I have had numerous opportunities to teach courses to undergraduate and master's students. 
                                In addition to classroom lectures, I have also had the chance to give field-based instruction to students, 
                                teaching them various sampling methods used in ecology and how to extrapolate these observations using satellite data.", align = "justify"),
                              h3("This practical experience has been invaluable in helping students understand the real-world applications of ecological 
                                research and how remote sensing can enhance our understanding of environmental patterns and processes.", align = "justify"),
                             h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about my teaching experiences"))
               ),
               absolutePanel(bottom = "5%", left = "88%", width = "10%",
                             a(target="_blank", href="https://isomer.univ-nantes.fr", img(src="UN_logo.png", width = "50%"))
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
                               h1(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Vegetation extent assessment"), align = "center"),
                               h3("Often, when we conduct drone flights, we also perform measurements on the ground, which later allow us to validate the various 
                                  algorithms we develop.", align = "justify"),
                               h3("For this purpose, we have a dGPS (differential Global Positioning System) with positioning accuracy of a few centimeters.
                                  This GPS enables us to precisely geolocate the data we acquire in the field within the drone images and in the spatial context. 
                                  The acquired data can include hyperspectral data obtained using an ASD (Analytical Spectral Device), height data (e.g., height of oyster tables)
                                  , or simply quadrats that help us estimate the percentage of vegetation cover for different types of vegetation.", align = "justify"),
                               h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about my teaching experiences"))
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
     
     
     
     
     
     observeEvent(input$selected_interest, {
       if (input$selected_interest == "Ulvophyceae" & input$showOverlay) {
         output$textprj <- renderUI({
           list(
             absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                           h1(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Ulvophyceae"), align = "center"),
                           h3("Ulvophyceae, commonly known as green seaweeds or ulvophytes, are a diverse group of photosynthetic organisms belonging to the phylum Chlorophyta. 
                              With their characteristic green pigmentation resulting from chlorophyll-a and chlorophyll-b, 
                              they share a common evolutionary heritage with land plants. Ulvophyceae display a wide range of morphological forms, 
                              including filamentous, sheet-like, and tubular structures, reflecting their ability to adapt to various ecological niches.", align = "justify"),
                           h3(""),
                           h3("Because Ulvophyceae share the same pigment composition has marine magnoliopsida, these to class can be difficult to distinguish using remote sensing.
                              Furthemore, green algae and seagrass are often mixed together especially in places where green tides can occur."),
                           h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about my work on green algae"))
                           ),
           absolutePanel(bottom = "1%", left = "0.5%", width = "35%",
                         img(src="GreenAlgae.png", width = "100%")
           )
             
           ) 
         })
         output$Media <- renderUI({
           
         })
         
       }else{
         if (input$selected_interest == "Gracilaria vermiculophylla" & input$showOverlay) {
           output$textprj <- renderUI({
             list(
               absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                             h1(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Gracilaria vermiculophylla"), align = "center"),
                             h3("Gracilaria vermiculophylla, commonly known as the Asian seaweed or vermicular seaweed, 
                                belongs to the family Gracilariaceae. It is characterized by its delicate, 
                                branching structure and vibrant reddish coloration. Originally native to coastal areas of the Western Pacific, 
                                including Japan and Korea, this seaweed species has spread rapidly to numerous regions worldwide, 
                                facilitated by human activities such as shipping and aquaculture.", align = "justify"),
                             h3(""),
                             h3("It has gained notoriety as a highly invasive species due to its exceptional adaptability and reproductive capabilities. 
                                It can tolerate a wide range of environmental conditions.  
                                These trait enable it to outcompete native species and establish dense populations in invaded areas."),
                             h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about my work on Gracilaria"))
               ),
               absolutePanel(bottom = "1%", left = "0.5%", width = "35%",
                             img(src="Gracilaria.png", width = "100%")
               )
               
             ) 
           })
           output$Media <- renderUI({
             
           })
           
         }else{
           if (input$selected_interest == "Kappaphycus alvarezii" & input$showOverlay) {
             output$textprj <- renderUI({
               list(
                 absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                               h1(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Kappaphycus alvarezii"), align = "center"),
                               h3("Kappaphycus alvarezii, also known as Kappa or Eucheuma seaweed, is cultivated primarily for its carrageenan content. 
                                  Indonesia stands as a major producer of this algae. The cultivation of Kappaphycus alvarezii involves floating raft systems and vertical 
                                  longlines in coastal areas. Water quality, nutrient availability, and disease control are carefully managed during cultivation. 
                                  Indonesia's significant production contributes to the global supply of carrageenan.", align = "justify"),
                               h3("Cultivating Kappaphycus alvarezii provides a reliable source of this valuable ingredient. 
                                  The country's expertise in cultivation methods supports the efficient growth of the algae.
                                  Kappaphycus alvarezii cultivation in Indonesia plays a vital role in meeting industrial demands for carrageenan.", align = "justify"),
                               h3(""),
                               h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about my work on Kappaphycus alvarezii"))
                 ),
                 absolutePanel(bottom = "1%", left = "0.5%", width = "35%",
                               img(src="Kapaphycus.png", width = "100%")
                 )
                 
               ) 
             })
             output$Media <- renderUI({
               
             })
             
           }else{
             if (input$selected_interest == "Microphytobenthos" & input$showOverlay) {
               output$textprj <- renderUI({
                 list(
                   absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                                 h1(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Microphytobenthos"), align = "center"),
                                 h3("Intertidal microphytobenthos are microscopic photosynthetic organisms found in the upper layers of sediment in intertidal zones. 
                                    They form dense biofilms on sediment surfaces, playing a vital role in coastal ecosystems. 
                                    These organisms contribute to primary production and nutrient cycling in the benthic environment. 
                                    Intertidal microphytobenthos serve as a crucial food source for various organisms, supporting the growth and survival of 
                                    invertebrates and higher trophic levels.", align = "justify"),
                                 h3(" Studying their responses to environmental changes helps understand the impacts of human activities and climate change on coastal ecosystems. 
                                    The interactions of intertidal microphytobenthos with light, nutrients, and temperature are areas of interest in ecological research.", align = "justify"),
                                 h3(""),
                                 h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about my work on microphybenthos"))
                   ),
                   absolutePanel(bottom = "1%", left = "0.5%", width = "35%",
                                 img(src="MPB_sampling.jpg", width = "100%")
                   )
                   
                 ) 
               })
               output$Media <- renderUI({
                 
               })
               
             }else{
               if (input$selected_interest == "Oyster reefs and Oyster farms" & input$showOverlay) {
                 output$textprj <- renderUI({
                   list(
                     absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                                   h1(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Oysters"), align = "center"),
                                   h3("Oysters, highly valued bivalve mollusks, are widely cultivated through aquaculture practices. 
                                      Their cultivation in marine and brackish water environments provides both ecological and economic benefits. 
                                      Oyster farms contribute to the creation of essential habitat for diverse marine organisms, enhancing biodiversity in coastal ecosystems. 
                                      As filter feeders, oysters improve water quality by efficiently removing nutrients and particulate matter from the surrounding environment.
                                      Oyster reefs serve as protective nurseries, supporting the growth and survival of various fish and invertebrate species. 
                                      Moreover, oyster reefs act as natural buffers, reducing coastal erosion and mitigating wave energy. ", align = "justify"),
                                   h3("Oysters are highly sought-after in the aquaculture industry, providing a valuable source of food and economic revenue. 
                                      Sustainable management practices are essential to maintain healthy oyster populations and ensure the long-term viability of oyster aquaculture.", align = "justify"),
                                   h3(""),
                                   h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about my work on oysters"))
                     ),
                     absolutePanel(bottom = "1%", left = "0.5%", width = "35%",
                                   img(src="oyster_slide2.jpg", width = "100%")
                     )
                     
                   ) 
                 })
                 output$Media <- renderUI({
                   
                 })
                 
               }else{
                 if (input$selected_interest == "Sabellaria alveolata" & input$showOverlay) {
                   output$textprj <- renderUI({
                     list(
                       absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                                     h1(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Sabellaria alveolata"), align = "center"),
                                     h3("Sabellaria alveolata, known as the honeycomb worm, forms biogenic reefs with sand and shell fragments. These reefs provide vital habitat in coastal areas. 
                                        They create a complex structure, offering shelter for diverse marine organisms. Sabellaria alveolata reefs serve as nursery grounds, supporting the growth of various fish and invertebrates. 
                                        They also play a role in sediment stabilization and reducing coastal erosion. As filter feeders, honeycomb worms improve water quality by filtering plankton and organic particles. 
                                        The reefs contribute to biodiversity and are valued for their aesthetic and recreational opportunities. 
                                       ", align = "justify"),
                                     h3(" Conservation efforts are crucial to protect Sabellaria alveolata populations and preserve these unique reefs. Understanding their ecological 
                                        significance aids in implementing sustainable management practices for coastal ecosystem health.", align = "justify"),
                                     h3(""),
                                     h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about my work on honeycomb reefs"))
                       ),
                       absolutePanel(bottom = "1%", left = "0.5%", width = "35%",
                                     img(src="Honeycomb_reef.jpg", width = "100%")
                       )
                       
                     ) 
                   })
                   output$Media <- renderUI({
                     
                   })
                   
                 }else{
                   if (input$selected_interest == "Xanthophyceae" & input$showOverlay) {
                     output$textprj <- renderUI({
                       list(
                         absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                                       h1(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Xanthophyceae"), align = "center"),
                                       h3("Xanthophyceae, or yellow-green algae, are diverse organisms found in freshwater and marine habitats. 
                                          Their distinct yellow-green pigmentation sets them apart. Xanthophyceae contribute to primary production and nutrient cycling through photosynthesis, 
                                          indicating healthy water quality. They form symbiotic relationships, providing habitats for other organisms and enhancing ecosystem diversity. 
                                          These algae can adapt to various environmental conditions, thriving in freshwater lakes, ponds, and coastal regions. 
                                          Their versatility allows them to occupy diverse ecological niches. Xanthophyceae serve as indicators of water quality,
                                          offering insights into environmental changes.", align = "justify"),
                                       h3("Studying their diversity and ecological interactions aids in understanding and managing aquatic ecosystems. 
                                          Conservation efforts should prioritize preserving their habitats and supporting water quality for the continued well-being of Xanthophyceae and overall
                                          aquatic ecosystem health.", align = "justify"),
                                       h3(""),
                                       h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about my work on Vaucheria sp."))
                         ),
                         absolutePanel(bottom = "1%", left = "0.5%", width = "35%",
                                       img(src="jpg", width = "100%")
                         )
                         
                       ) 
                     })
                     output$Media <- renderUI({
                       
                     })
                     
                   }else{
                     if (input$selected_interest == "Xanthophyceae" & input$showOverlay) {
                       output$textprj <- renderUI({
                         list(
                           absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                                         h1(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Xanthophyceae"), align = "center"),
                                         h3("Xanthophyceae, or yellow-green algae, are diverse organisms found in freshwater and marine habitats. 
                                          Their distinct yellow-green pigmentation sets them apart. Xanthophyceae contribute to primary production and nutrient cycling through photosynthesis, 
                                          indicating healthy water quality. They form symbiotic relationships, providing habitats for other organisms and enhancing ecosystem diversity. 
                                          These algae can adapt to various environmental conditions, thriving in freshwater lakes, ponds, and coastal regions. 
                                          Their versatility allows them to occupy diverse ecological niches. Xanthophyceae serve as indicators of water quality,
                                          offering insights into environmental changes.", align = "justify"),
                                         h3("Studying their diversity and ecological interactions aids in understanding and managing aquatic ecosystems. 
                                          Conservation efforts should prioritize preserving their habitats and supporting water quality for the continued well-being of Xanthophyceae and overall
                                          aquatic ecosystem health.", align = "justify"),
                                         h3(""),
                                         h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about my work on Vaucheria sp."))
                           ),
                           absolutePanel(bottom = "1%", left = "0.5%", width = "35%",
                                         img(src="jpg", width = "100%")
                           )
                           
                         ) 
                       })
                       output$Media <- renderUI({
                         
                       })
                       
                     }else{
                       if (input$selected_interest == "Zostera noltii" & input$showOverlay) {
                         output$textprj <- renderUI({
                           list(
                             absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                                           h1(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Zostera noltii"), align = "center"),
                                           h3("Intertidal Zostera noltii meadows are vital coastal ecosystems found between land and sea. 
                                              They provide important nursery habitats for diverse marine organisms.
                                              These meadows stabilize sediments, reducing erosion and enhancing shoreline protection. 
                                              Zostera noltii's extensive root systems help anchor sediments, ensuring coastal stability. 
                                              They contribute to nutrient cycling and water quality improvement. Intertidal Zostera noltii meadows tolerate tidal fluctuations and exposure to air during low tides. 
                                              Conservation efforts are necessary to protect and preserve these valuable ecosystems.", align = "justify"),
                                          h3("Understanding their significance aids in implementing sustainable management practices for their long-term survival.", align = "justify"),
                                          h3(""),
                                          h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about my work on seagrass meadows "))
                             ),
                             absolutePanel(bottom = "1%", left = "0.5%", width = "35%",
                                           img(src="zostera.jpg", width = "100%")
                             )
                             
                           ) 
                         })
                         output$Media <- renderUI({
                           
                         })
                         
                       }else{
                         if (input$selected_interest == "All") {
                           output$textprj <- renderUI({
                             absolutePanel(top = 10, left = 10, width = "35%")
                           })
                         }
                       }
                     }
                   }
                 }
               }
             }
           }
         }
       }
     })
     
     observeEvent(input$selected_tool, {
       if (input$selected_tool == "Drone" & input$showOverlay) {
         output$textprj <- renderUI({
           list(
             absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                           h1(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Drone"), align = "center"),
                           h3("Drone usage in remote sensing has revolutionized data collection. Equipped with sensors and cameras, drones provide cost-effective, high-resolution imagery. 
                              They access previously challenging areas, expanding remote sensing capabilities. Drones capture aerial imagery, perform surveys, and monitor environmental changes. 
                              Their agility allows for precise and targeted data acquisition. The collected data aids in mapping, monitoring, and analysis for various applications. 
                              Drones democratize geospatial data collection, making it accessible and affordable. They transform environmental observation and decision-making processes. 
                              Drones offer new possibilities for research, conservation, and planning.", align = "justify"),
                           h3(""),
                           h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about my work with drones."))
                           ),
             absolutePanel(bottom = "1%", left = "0.5%", width = "35%",
                           img(src="dronezoom.jpg", width = "100%")
             )
             
           )
         })
         
       }else{
         if (input$selected_tool == "Satellite" & input$showOverlay) {
           output$textprj <- renderUI({
             list(
               absolutePanel(id="text_box", class = "panel panel-default", top = 10, left = 10, width = "35%",
                             h1(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Satellites"), align = "center"),
                             h3("Satellite usage in remote sensing has transformed intertidal ecology research. 
                                Specialized sensors on satellites provide global-scale monitoring of intertidal habitats. 
                                Satellite imagery captures information on coastal vegetation, tidal dynamics, and sediment distribution. 
                                Researchers utilize satellite data to assess biodiversity and study the impacts of coastal development and climate change. 
                                Satellites help map intertidal habitats and track species distribution and migration patterns.
                                Satellite observations aid in monitoring pollution and human impacts on intertidal areas. 
                                They inform conservation efforts and guide restoration projects. 
                                Satellite remote sensing enhances our understanding of the dynamic interactions between land and sea. 
                                It supports sustainable coastal management practices and the preservation of intertidal ecosystem integrity.", align = "justify"),
                             h3(""),
                             h3(a(target="_blank", href="https://sigoiry.github.io/Website/about.html", "Know more about my work with Satellites"))
               ),
               absolutePanel(bottom = "1%", left = "8%", width = "25%",
                             img(src="Sentinel2.png", width = "100%")
               )
               
             )
           })
           
         }else{
           if (input$selected_tool == "All") {
             output$textprj <- renderUI({
               absolutePanel(top = 10, left = 10, width = "35%")
             })
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
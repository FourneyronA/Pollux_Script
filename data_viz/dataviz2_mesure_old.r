# . -------------------------------------------------------------------------- =============
# 0 - Chargement des librairies ---------------------------------------------- =============
# . -------------------------------------------------------------------------- =============

### manipulation et traitement de donnees
library(readxl)
library(dplyr) 
library(tidyr)
library(tidyverse)
library(lubridate) 
library(stringr)
library(geojsonR)
### manipulation application
library(shiny)

### visualisation des donnees 
library(ggplot2) # la visualisation graph stat
library(RColorBrewer) # des palettes
library(rmarkdown)
library(gganimate)
library(gapminder)
library(plotly)
library(maps)
#install.packages("mapdata")
library(mapdata)
library(maptools)

## analyse spatiale / carto
library(sp) # classes et methodes pour donnees spatiales pe dlasspar SF
library(tmap) # carto
library(ggmap)# carto +
library(leaflet) # carto web
library(rgdal) #gdal pour projection, crud et surtout export
library(sf) # nouveau package de classes et methodes spatiales doit "remplacer" rgdal et rgeos (et ofc sp) 
library(mapview)
library(raster)
library(rasterVis)
library(rgdal)
library(viridis)


library(rasterVis)
library(animation)
library(classInt)

# . -------------------------------------------------------------------------- =============
# 1 - Preparation des données ------------------------------------------------ =============
# . -------------------------------------------------------------------------- =============

# requete sur la base de donnee

Donnee_NSB <- st_read(DB_pol_lum, query = "SELECT dc.site_mesure, dc.date, dc.time, sc.geometry, sc.lieu_dit, dc.nsb 
                                      FROM donnee_capteur as dc
                                      INNER JOIN station_capteur as sc 
                                      ON dc.site_mesure = sc.site_mesure_id") 

length(unique(Donnee_NSB$site_mesure))

# remise en forme des donnees temporelle pour ameliorer les rendu visuel
Donnee_exploitable <- Donnee_NSB %>%
  mutate(date_ok = as_date(paste("2020/",str_sub(date, start = 6, end = 10),sep = "")),
         time_ok = hms::as_hms(time)) %>%
  filter(date_ok >= as.Date("2020-04-01") & date_ok <= as.Date("2020-04-30"))%>%
  #filter(time_ok <= hms::as_hms("06:00:00") | time_ok >= hms::as_hms("19:00:00")) %>%
  mutate(daynight = as_date(ifelse(time_ok <= hms::as_hms("06:00:00"), date_ok-1, date_ok)),
         site_mesure = paste(site_mesure, lieu_dit),
         h = as.numeric(str_sub(time_ok, start = 1, end = 2)),
         m = as.numeric(str_sub(time_ok, start = 4, end = 5)),
         s = as.numeric(str_sub(time_ok, start = 7, end = 8)),
         time_V2 = ifelse(daynight == date_ok,
                          as.numeric(h*3600+m*60+s),
                          as.numeric((h+24)*3600+m*60+s)))


### Gestion des variables ==== 
# les sites possibles
list_site <- unique(Donnee_exploitable$site_mesure)

# liste des analyses possible de choisir
list_analyse <- c('Moyenne','Max','Min','Exact');
list_heure <- c('Heure','Minute','Seconde');

# . -------------------------------------------------------------------------- =============
# 3 - Application Shiny  -------------------------------------- =============
# . -------------------------------------------------------------------------- =============

# 1 - Gestion des entrées/sorties ====
ui <- fluidPage(
  # Application titre
  titlePanel("Analyses spatio-temporelles de la pollution lumineuse"),
  
  conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                   HTML("<style>#dashboard_complete{cursor:default;}</style>")
  ),
  
  # DEF des entrées ====
  sidebarLayout(
    sidebarPanel(
      
      dateRangeInput("choix_date", "choix de la plage temporelle", start = '2020-04-01', end ='2020-04-01',  #Sys.Date(),
                     min = NULL, max = NULL, format = "dd/mm/yyyy", startview = "month", weekstart = 0,
                     language = "en", separator = " au ", width = NULL),
      
      #timeInput("choix_time", "Choix de l'heure", value = hms::as_hms("19:00:00")),
      selectInput("choix_analyse","Methode d'analyse", list_analyse, selected = 'Moyenne'),
      selectInput("choix_time","Agregation des donnees", list_heure, selected = 'Heure'),
      sliderInput("choix_heure","Choix des heures", 0,23,1),
      sliderInput("choix_minute","Choix des minutes", 0,60,1),
      #sliderInput("choix_seconde","Choix des secondes", 0,60,1),
      checkboxGroupInput("choix_site","Choix des sites",list_site[], selected = list_site[])
    ),
    
    # DEF des sorties ====
    mainPanel(wellPanel(
      navbarPage("Menu",
                 tabPanel("Tableau des données",tableOutput("table_graph")),
                 tabPanel("Graphique",plotOutput("distPlot_histo")),
                 tabPanel("Cartographie",imageOutput("Plot_GIF") )))
    )
  )
)

# 2 - Gestion du serveur ====
server <- function(input, output){
  

  # gestion reactive des autres entrées ====
  react_input <- reactive({
    Sys.sleep(1)
    
    if(input$choix_time == "Heure"){

      data_graph <- Donnee_exploitable %>% 
        filter(site_mesure %in% input$choix_site) %>%
        filter(h == input$choix_heure) %>%
        filter(date_ok >= input$choix_date[[1]] & date_ok <= input$choix_date[[2]])  %>%
        group_by(site_mesure,h) %>%
        summarise( moy_nsb = mean(nsb), max_nsb = max(nsb),  min_nsb = min(nsb))
      
    }
    
    if(input$choix_time == "Minute"){
      
      data_graph <- Donnee_exploitable %>% 
        filter(site_mesure %in% input$choix_site) %>%
        filter(h == input$choix_heure) %>%
        filter(m == input$choix_minute) %>%
        filter(date_ok >= input$choix_date[[1]] & date_ok <= input$choix_date[[2]])  %>%
        group_by(site_mesure,h,m,s) %>%
        summarise( moy_nsb = mean(nsb), max_nsb = max(nsb),  min_nsb = min(nsb))
      
    }
    
    data_graph
  })
  
  # gestion de la visualisation des données ====
  output$table_graph <- renderTable({
    head(react_input(),50)
  })
  
  # gestion de la vialusation statistiques ====
  output$distPlot_histo <- renderPlot({

    if(input$choix_tempo == "Moyenne"){

      ggplot(react_input(), aes(y=moy_nsb , x = site_mesure, fill = moy_nsb, color = moy_nsb ))+
        geom_boxplot(width = 0.5, alpha = 0.5)+
        geom_jitter(width = 0.25, alpha = 0.4)+
        scale_color_viridis(direction = -1,discrete = FALSE, option="magma")+
        scale_fill_viridis(direction = -1,discrete = FALSE, option="magma")+
        labs(y = "Luminosité en nsb", x = "Mesure", subtitle = "donnée à chauqe minutes") +
        ggtitle(paste("Moyenne de luminosité à",input$choix_heure,"h"))+
        theme_minimal() + 
        coord_flip()
      
    } 
    
    if(input$choix_tempo == "Max"){

      ggplot(react_input(), aes(y=max_nsb , x = site_mesure, fill = max_nsb, color = max_nsb ))+
        geom_boxplot(width = 0.5, alpha = 0.5)+
        geom_jitter(width = 0.25, alpha = 0.4)+
        scale_color_viridis(direction = -1,discrete = FALSE, option="magma")+
        scale_fill_viridis(direction = -1,discrete = FALSE, option="magma")+
        labs(y = "Luminosité en nsb", x = "Mesure", subtitle = "donnée à chauqe minutes") +
        ggtitle(paste("Moyenne de luminosité à",input$choix_heure,"h ",input$choix_minute,"min"))+
        theme_minimal() + 
        coord_flip()
    } 
    
    if(input$choix_tempo == "Min") {

        ggplot(react_input(), aes(y=min_nsb , x = site_mesure, fill = min_nsb, color = min_nsb ))+
        geom_boxplot(width = 0.5, alpha = 0.5)+
        geom_jitter(width = 0.25, alpha = 0.4)+
        scale_color_viridis(direction = -1,discrete = FALSE, option="magma")+
        scale_fill_viridis(direction = -1,discrete = FALSE, option="magma")+
        labs(y = "Luminosité en nsb", x = "Mesure", subtitle = "donnée à chauqe minutes") +
        ggtitle(paste("Moyenne de luminosité à",unique(data_graph$h),"h"))+
        theme_minimal() + 
        coord_flip()
    }
    
    
  
  })
  
}
# . -------------------------------------------------------------------------- =============
# Lancer l'application ------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============
shinyApp(ui = ui, server = server)




# 
# ggplot(data_graph, aes(y=max_nsb , x = site_mesure, fill = max_nsb, color = max_nsb ))+
#   geom_boxplot(width = 0.5, alpha = 0.5)+
#   geom_jitter(width = 0.25, alpha = 0.4)+
#   scale_color_viridis(direction = -1,discrete = FALSE, option="magma")+
#   scale_fill_viridis(direction = -1,discrete = FALSE, option="magma")+
#   labs(y = "Luminosité en nsb", x = "Mesure", subtitle = "donnée à chauqe minutes") +
#   ggtitle(paste("Moyenne de luminosité à",unique(data_graph$h),"h"))+
#   theme_minimal() + 
#   coord_flip()


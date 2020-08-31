
library(tidyverse)

#Load the required packages to run the app
rm(list = ls())
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(htmltools)
library(rgdal)
library(leaflet)
library(plotrix)

#Load the csv files
all_forests <- read_csv("Data/CSVs/all_forests.csv")
dat_molten<- read_csv("Data/CSVs/dat_molten.csv")
dat_sum<- read_csv("Data/CSVs/dat_sum.csv")
dataFinal<- read_csv("Data/CSVs/dataFinal.csv")
mammals<- read_csv("Data/CSVs/mammals.csv")
newData<- read_csv("Data/CSVs/newData.csv")
divFinal<- read_csv("Data/CSVs/divFinal.csv")
Activity <- read_csv("Data/CSVs/Activity.csv")


#Load the shapefiles to prep the map
Forests<-readOGR("Data/Shapefiles/Study Forest Locations.shp", layer = "Study Forest Locations")

#Project shape file
Forests_proj<-spTransform(Forests, CRS("+proj=longlat +datum=WGS84"))

#Add column with unabbreviated forest name
Forests_proj@data$Forest <- with(Forests@data, ifelse(
  ForestCode == "SH", 'South Hammond', ifelse(
    ForestCode == "BC", 'Beaver Creek', ifelse(
      ForestCode == "DON", 'Donnerville', ifelse(
        ForestCode == "WHIP", 'Whippoorwill Corners', ifelse(
          ForestCode == "WF", 'Whiskey Flats', ifelse(
            ForestCode == "DEG", 'Degrasse', 'whoops')))))))


ui <- fluidPage(
  
  # App title ----
  titlePanel("North Country Wild Zooniverse Project"),
  tabsetPanel(
    #first tab = map of study sites
    tabPanel("Map of study sites", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(# Input: Slider for the forest selection ----
                     selectInput("forest", h3("Choose your Forest"),
                                 choices = c("All forests", 
                                             "South Hammond", 
                                             "Donnerville", 
                                             "Beaver Creek", 
                                             "Whippoorwill Corners", 
                                             "Whiskey Flats", 
                                             "Degrasse"), selected = "South Hammond"),
                     checkboxGroupInput("species", 
                                        h3("Choose your Species"), 
                                        choices = list("All Mammals",
                                                       "Black Bear",
                                                       "Bobcat",
                                                       "Chipmunk",
                                                       "Coyote",
                                                       "Fisher",
                                                       "Flying Squirrel",
                                                       "Gray Squirrel",
                                                       "Mink",
                                                       "Opossum",
                                                       "Other Small Mammal",
                                                       "Porcupine",
                                                       "Raccoon",
                                                       "Red Fox",
                                                       "Red Squirrel",
                                                       "River Otter",
                                                       "Snowshoe Hare",
                                                       "Striped Skunk",
                                                       "Weasel",
                                                       "White-tailed Deer"),
                                        selected = "Black Bear"),
                     
                     img(src = "NatureUpNorth.png", height = 100, width = 300)
        ),
        mainPanel(leafletOutput("speciesmap"))   
      )
    ),
    #Second tab = Number of detections 
    tabPanel("Detections", fluid = TRUE,
      sidebarLayout(
      sidebarPanel(# Input: Slider for the forest selection ----
                   selectInput("forest", h3("Choose your Forest"),
                               choices = c("All forests", 
                                           "South Hammond", 
                                           "Donnerville", 
                                           "Beaver Creek", 
                                           "Whippoorwill Corners", 
                                           "Whiskey Flats", 
                                           "Degrasse"), selected = "South Hammond"),
                   checkboxGroupInput("species",
                                      h3("Choose your Species"),
                                      choices = list("All Mammals",
                                                     "Black Bear",
                                                     "Bobcat",
                                                     "Chipmunk",
                                                     "Coyote",
                                                     "Fisher",
                                                     "Flying Squirrel",
                                                     "Gray Squirrel",
                                                     "Mink",
                                                     "Opossum",
                                                     "Other Small Mammal",
                                                     "Porcupine",
                                                     "Raccoon",
                                                     "Red Fox",
                                                     "Red Squirrel",
                                                     "River Otter",
                                                     "Snowshoe Hare",
                                                     "Striped Skunk",
                                                     "Weasel",
                                                     "White-tailed Deer"),
                                      selected = "Black Bear"),
                   
                   img(src = "NatureUpNorth.png", height = 100, width = 300)
      ),
      mainPanel(plotOutput(outputId = "foresthist"))   
    )
    )
      
    )
  )



  
server <- function(input, output){
  #Map of Study Sites
  output$speciesmap<-renderLeaflet({
    
    if("All Mammals" %in% input$species){
      data<-dat_sum
      
    } else{
        choices<-c(input$species)
        data<-dat_sum %>% filter(Species %in% choices)
    }
    #Join data to shape file
    Forests_proj@data <- left_join(Forests_proj@data, data, by = c("Forest"= "Forest"))
    pal <- colorNumeric("Blues", domain= data$number_det) 
    labels<-sprintf( "%s, Number of Detections %s", 
                         Forests_proj$Forest, data$number_det) %>% lapply(htmltools::HTML)
    leaflet() %>% addTiles() %>% 
      setView(lng = -75.169395, lat = 44.595466, zoom = 8) %>% 
      addPolygons(
        data = Forests_proj, 
        fillColor = ~pal(data$number_det),
        fillOpacity = 0.5,
        weight = 1, 
        col = 'red',
        highlight = highlightOptions(#highlight lets you mouse over a site and have it change color
          weight = 5,
          color = "orange", 
          bringToFront = T),
        label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) 
    })
  
  output$foresthist <- renderPlot({
    
    choices<-c(input$species)
    data<-newData %>% filter(Species %in% choices)
    study<-reactive(switch(input$forest,
                           if("All Mammals" %in% input$species){
                             choices<-c(unique(newData$Species))
                             data<-newData %>% filter(Species %in% choices)
                             study<-reactive(switch(input$forest,
                                                    "All Forests" = data,
                                                    "South Hammond" = data %>% filter(Forest=="South Hammond"),
                                                    "Beaver Creek" = data %>% filter(Forest=="Beaver Creek"),
                                                    "Donnerville" = data %>% filter(Forest == "Donnerville"),
                                                    "Whippoorwill Corners" = data %>% filter(Forest == "Whippoorwill Corners"),
                                                    "Whiskey Flats" = data %>% filter(Forest == "Whiskey Flats"),
                                                    "Degrasse" = data %>% filter(Forest == "Degrasse"))
                             )
                             ggplot(study(), aes(Species)) +
                               geom_histogram(stat = "count", position = "dodge") +
                               theme_bw() +
                               xlab("Species") +
                               ylab("Number of Detections") +
                               theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5))
                             
                           }
                           else{
                             choices<-c(input$species)
                             data<-newData %>% filter(Species %in% choices)
                             study<-reactive(switch(input$forest,
                                                    "All Forests" = data,
                                                    "South Hammond" = data %>% filter(Forest=="South Hammond"),
                                                    "Beaver Creek" = data %>% filter(Forest=="Beaver Creek"),
                                                    "Donnerville" = data %>% filter(Forest == "Donnerville"),
                                                    "Whippoorwill Corners" = data %>% filter(Forest == "Whippoorwill Corners"),
                                                    "Whiskey Flats" = data %>% filter(Forest == "Whiskey Flats"),
                                                    "Degrasse" = data %>% filter(Forest == "Degrasse"))
                             )
                             ggplot(study(), aes(Species)) +
                               geom_histogram(stat = "count", position = "dodge") +
                               theme_bw() +
                               xlab("Species") +
                               ylab("Number of Detections") +
                               theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5))
                             
                           }))
  })
}
shinyApp(ui = ui, server = server)
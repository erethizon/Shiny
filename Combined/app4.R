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
Activity <- read_csv("~/Documents/Shiny/Activity/Data/Activity.csv")


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
    #Mammal Activity Patterns 
    tabPanel("Mammal Activity Patterns", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("mammals", h3("Choose your Species"),
                                        choices = c("Black Bear",
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
                            
                            img(src = "NatureUpNorth.png", height = 100, width = 240)
               ),
               mainPanel(h5("Mammals are not all active at the same time! Select a mammal using the tab in the 
               panel to the left and learn what times of day that mammal was detected by our cameras.
               The graph is shaped like a clock with 0-23 symbolizing the hour we detected the mammal.
               The x-axis lets you know how many detections we had during that hour."),
                         plotOutput(outputId = "activity"), imageOutput(outputId = "image"))   
             )
    )
    
  )
)




server <- function(input, output){

  output$image <- renderImage({
    list(src = "Deer.jpg", height = 240, width = 300, align = "left")
  if(input$mammals == "White-tailed Deer"){
    list(src = "Deer.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Chipmunk"){
    list(src = "Chipmunk.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Coyote"){
    list(src = "Coyote.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Fisher"){
    list(src = "Fisher.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Raccoon"){
    list(src = "Raccoon.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Red Squirrel"){
    list(src = "RedSquirrel.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Other Small Mammal"){
    list(src = "Mole.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Gray Squirrel"){
    list(src = "GraySquirrel.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Black Bear"){
    list(src = "blackbear.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Red Fox"){
    list(src = "RedFox.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Porcupine"){
    list(src = "Porcupine.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Bobcat"){
    list(src = "Bobcat.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Opossum"){
    list(src = "Opossum.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Weasel"){
    list(src = "Weasel.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Striped Skunk"){
    list(src = "StripedSkunk.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Flying Squirrel"){
    list(src = "FlyingSquirrel.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Snowshoe Hare"){
    list(src = "SnowshoeHare.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "River Otter"){
    list(src = "RiverOtter.jpg", height = 240, width = 300, align = "left")
  }
  else if(input$mammals == "Mink"){
    list(src = "Mink.jpg", height = 240, width = 300, align = "left")
  }
  })
}

shinyApp(ui = ui, server = server)
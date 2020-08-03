#Load the required packages to run the app
rm(list = ls())
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(htmltools)

#Load the csv files
all_forests <- read_csv("Data/CSVs/all_forests.csv")
dat_molten<- read_csv("Data/CSVs/dat_molten.csv")
dat_sum<- read_csv("Data/CSVs/dat_sum.csv")
dataFinal<- read_csv("Data/CSVs/dataFinal.csv")
mammals<- read_csv("Data/CSVs/mammals.csv")
newData<- read_csv("Data/CSVs/newData.csv")
divFinal<- read_csv("Data/CSVs/divFinal.csv")


#Load the shapefiles to prep the map
Forests<-readOGR("Data/Shapefiles/Study Forest Locations.shp", layer = "Study Forest Locations")

#Project shape file
Forests_proj<-spTransform(Forests, CRS("+proj=longlat +datum=WGS84"))

#Add column with unabbreviated forest name
Forests@data$Forest <- with(Forests@data, ifelse(
  ForestCode == "SH", 'South Hammond', ifelse(
    ForestCode == "BC", 'Beaver Creek', ifelse(
      ForestCode == "DON", 'Donnerville', ifelse(
        ForestCode == "WHIP", 'Whippoorwill Corners', ifelse(
          ForestCode == "WF", 'Whiskey Flats', ifelse(
            ForestCode == "DEG", 'Degrasse', 'whoops')))))))


labels<-sprintf( "%s", 
                 Forests$Forest) %>% lapply(htmltools::HTML)

#Run the shiny app!
ui <- fluidPage(
  
  # App title ----
  titlePanel("North Country Wild Zooniverse Project"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput("forest", h3("Choose your Forest"),
                  choices = c("All Forests","South Hammond", "Donnerville", "Beaver Creek", "Whippoorwill Corners", "Whiskey Flats", "Degrasse"), selected = "All Forests"),
      
      conditionalPanel( condition = "NOTHINGHERE",
                        checkboxInput("remove", "Remove Nothing Here")),
      h6("Powered by:"),
      img(src = "nun_SLU.jpg", height = 100, width = 150),
      
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(  tabsetPanel(
      tabPanel("Map of Study Sites", leafletOutput(outputId = "speciesmap")),
      tabPanel("Number of Detections per Species",
               plotOutput(outputId = "foresthist")),
      tabPanel("Species Trophic Levels", plotOutput(outputId = "trophic")),
      tabPanel("Forest Composition", plotOutput(outputId = "covariates")),
      tabPanel("Forest Diversity", plotOutput(outputId = "diversity"))
    )
    )
  )
)


server <- function(input, output) {
  
  #Map of Study Sites
  output$speciesmap <- renderLeaflet({ 
    
    
    leaflet() %>% addTiles() %>% 
      setView(lng = -75.169395, lat = 44.595466, zoom = 9) %>%  
      addPolygons(
        data = Forests_proj, 
        weight = 2, 
        col = '#39541e',
        highlight = highlightOptions(#highlight lets you mouse over a county and have it change color
          weight = 5,
          color = "#C6ABE1", 
          bringToFront = T),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) 
    
  })
  #Number of Detections per Species
  output$foresthist <- renderPlot({
    
    
    if(input$remove){
      newdata<-switch(input$forest, 
                      "All Forests" = newData,
                      "South Hammond" = newData %>% filter(ForestName=="SH"),
                      "Beaver Creek" = newData %>% filter(ForestName=="BC"),
                      "Donnerville" = newData %>% filter(ForestName == "DON"),
                      "Whippoorwill Corners" = newData %>% filter(ForestName == "WHIP"),
                      "Whiskey Flats" = newData %>% filter(ForestName == "WF"),
                      "Degrasse" = newData %>% filter(ForestName == "DEG"))
      
      ggplot(newdata, aes(Species)) + 
        geom_histogram(stat = "count", position = "dodge", fill = '#165970', colour = '#543b1f') + 
        labs(title = "Species per Forest", x="Species", y="Number of Detections") +
        theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5))
    }    
    else {
      data<- switch(input$forest, 
                    "All Forests" = dataFinal,
                    "South Hammond" = dataFinal %>% filter(ForestName=="SH"),
                    "Beaver Creek" = dataFinal %>% filter(ForestName=="BC"),
                    "Donnerville" = dataFinal %>% filter(ForestName == "DON"),
                    "Whippoorwill Corners" = dataFinal %>% filter(ForestName == "WHIP"),
                    "Whiskey Flats" = dataFinal %>% filter(ForestName == "WF"),
                    "Degrasse" = dataFinal %>% filter(ForestName == "DEG"))
      
      ggplot(data, aes(Species)) + 
        geom_histogram(stat = "count", position = "dodge", fill = '#165970', colour = '#543b1f') + 
        labs(title = "Species per Forest", x="Species", y="Number of Detections")+
        theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5))
    }
  })
  #Species Trophic Levels
  output$trophic <- renderPlot({
    
    data<-switch(input$forest, 
                 "All Forests" = mammals,
                 "Whiskey Flats" = mammals %>% filter(ForestName=="WF"),
                 "South Hammond" = mammals %>% filter(ForestName=="SH"),
                 "Donnerville" = mammals %>% filter(ForestName == "DON"),
                 "Beaver Creek" = mammals %>% filter(ForestName == "BC"),
                 "Degrasse" = mammals %>% filter(ForestName == "DEG"),
                 "Whippoorwill Corners" = mammals %>% filter(ForestName == "WHIP"),)
    
    ggplot(data, aes(Trophic)) + 
      geom_histogram(stat = "count", position = "dodge", fill = '#165970', colour = '#543b1f') + 
      labs(title = "Trophic Levels per Forest", x="Trophic Level", y="Number of Detections") +
      theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5))
    
    
  })
  #Forest Composition
  output$covariates <- renderPlot({
    
    data<-switch(input$forest, 
                 "All Forests" = all_forests,
                 "South Hammond" = dat_molten %>% filter(Forest=="SH"),
                 "Beaver Creek" = dat_molten %>% filter(Forest=="BC"),
                 "Donnerville" = dat_molten %>% filter(Forest=="DON"),
                 "Degrasse" = dat_molten %>% filter(Forest=="DEG"),
                 "Whippoorwill Corners" = dat_molten %>% filter(Forest=="WHIP"),
                 "Whiskey Flats" = dat_molten %>% filter(Forest=="WF"),)
    
    
    ggplot(data, aes(x="", y=value, fill= variable)) + 
      geom_bar(stat="identity") + 
      coord_polar("y", start = 0) + 
      geom_text(aes(label = paste0(round(value), "%")), color = "white", size=2)+
      theme_void() +
      theme(axis.line = element_blank(),
            plot.title = element_text(hjust=0.5)) + 
      labs(x = NULL,
           y = NULL,
           title = "Forest Composition")+
      scale_fill_manual(values = c("Water" = "#165970",
                                   "Mixed" = "#543b1f",
                                   "Evergreen" = "#C6ABE1",
                                   "Deciduous" = "#39541e",
                                   "Development" = "#ABC4E0",
                                   "Agriculture" = "#E9A68F",
                                   "Barren" = "#b9da97",
                                   "Shrub" = "#d4b18a",
                                   "Wetland" = "#69c3e1",
                                   "Herbaceous" = "Gray"))
    
    
  })
  #Forest Diversity 
  output$diversity <- renderPlot({
    
    data<-switch(input$forest, 
                 "All Forests" = divFinal,
                 "South Hammond" = divFinal %>% filter(Forest == "SH"),
                 "Beaver Creek" = divFinal %>% filter(Forest == "BC"),
                 "Donnerville" = divFinal %>% filter(Forest == "DON"),
                 "Degrasse" = divFinal %>% filter(Forest == "DEG"),
                 "Whippoorwill Corners" = divFinal %>% filter(Forest == "WHIP"),
                 "Whiskey Flats" = divFinal %>% filter(Forest == "WF"),)
    
    ggplot(data, aes(x= Forest, y = Index, fill = Diversity_Index)) + 
      geom_bar(stat = "identity",position= position_dodge(), width = 0.7) +
      labs(title = "Diversity Indices per Forest", x= "Forest", y= "Diversity Index") + theme (plot.title =element_text(hjust = 0.5)) + 
      scale_fill_manual(values = c("Shannon Index" = "#165970",
                                   "Inverse Simpson Index" = "#543b1f",
                                   "Species Richness" = "#C6ABE1"))
    
  })
  
  
  
  
}



shinyApp(ui = ui, server = server)
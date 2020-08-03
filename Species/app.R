ui <- fluidPage(
  
  # App title ----
  titlePanel("Mammal Distribution by Forset Type"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput("species", h3("Choose your Species"),
                  choices = c("White-tailed Deer", "Chipmunk", "Coyote", "Fisher", "Raccoon", "Red Squirrel", "Other Small Mammal", "Gray Squirrel", "Black Bear", "Red Fox", "Porcupine", "Bobcat", "Opossum", "Weasel", "Striped Skunk", "Flying Squirrel", "Snowshoe Hare", "River Otter", "Mink"), selected = "White-tailed Deer"),
      
      
      img(src = "nun_SLU.jpg", height = 100, width = 200)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "foresthist"),
      imageOutput(outputId = "image")
      
      
    )
  )
)

server <- function(input, output) {
  
  # Histogram of which forest types each species is found at ----
  
  output$foresthist <- renderPlot({
    
    data<-switch(input$species, 
                 "White-tailed Deer" = newData %>% filter(choice=="DEERWHITETAILED"),
                 "Chipmunk" = newData %>% filter(choice=="CHIPMUNK"),
                 "Coyote" = newData %>% filter(choice == "COYOTE"),
                 "Fisher" = newData %>% filter(choice == "FISHER"),
                 "Raccoon" = newData %>% filter(choice == "RACCOON"),
                 "Red Squirrel" = newData %>% filter(choice == "SQUIRRELRED"),
                 "Gray Squirrel" = newData %>% filter(choice == "SQUIRRELGRAY"),
                 "Black Bear" = newData %>% filter(choice == "BLACKBEAR"),
                 "Red Fox" = newData %>% filter(choice == "FOXRED"),
                 "Porcupine" = newData %>% filter(choice == "PORCUPINE"),
                 "Bobcat" = newData %>% filter(choice == "BOBCAT"),
                 "Weasel" = newData %>% filter(choice == "WEASEL"),
                 "Striped Skunk" = newData %>% filter(choice == "SKUNKSTRIPED"),
                 "Flying Squirrel" = newData %>% filter(choice == "SQUIRRELFLYING"),
                 "Snowshoe Hare" = newData %>% filter(choice == "SNOWSHOEHARE"),
                 "River Otter" = newData %>% filter(choice == "RIVEROTTER"),
                 "Mink" = newData %>% filter(choice == "MINK"),
                 "Other Small Mammal" = newData %>% filter(choice == "OTHERSMALLMAMMAL"),
                 "Opossum" = newData %>% filter(choice == "OPOSSUM"),)
    
    ggplot(data, aes(ForestName)) + 
      geom_histogram(stat = "count", position = "dodge") + 
      theme_bw() + 
      xlab("Forest Type") +
      ylab("Number of Detections") +
      theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5))
    
    
  })
  
  output$image <- renderImage({    
    if(input$species == "White-tailed Deer"){            
      list(src = "Deer.jpg", height = 240, width = 300)
    }                                        
    else if(input$species == "Chipmunk"){
      list(src = "Chipmunk.jpg", height = 240, width = 300)
    }
    else if(input$species == "Coyote"){
      list(src = "Coyote.jpg", height = 240, width = 300)
    }
    else if(input$species == "Fisher"){
      list(src = "Fisher.jpg", height = 240, width = 300)
    }
    else if(input$species == "Raccoon"){
      list(src = "Raccoon.jpg", height = 240, width = 300)
    }
    else if(input$species == "Red Squirrel"){
      list(src = "RedSquirrel.jpg", height = 240, width = 300)
    }
    else if(input$species == "Other Small Mammal"){
      list(src = "Mole.jpg", height = 240, width = 300)
    }
    else if(input$species == "Gray Squirrel"){
      list(src = "GraySquirrel.jpg", height = 240, width = 300)
    }
    else if(input$species == "Black Bear"){
      list(src = "blackbear.jpg", height = 240, width = 300)
    }
    else if(input$species == "Red Fox"){
      list(src = "RedFox.jpg", height = 240, width = 300)
    }
    else if(input$species == "Porcupine"){
      list(src = "Porcupine.jpg", height = 240, width = 300)
    }
    else if(input$species == "Bobcat"){
      list(src = "Bobcat.jpg", height = 240, width = 300)
    }
    else if(input$species == "Opossum"){
      list(src = "Opossum.jpg", height = 240, width = 300)
    }
    else if(input$species == "Weasel"){
      list(src = "Weasel.jpg", height = 240, width = 300)
    }
    else if(input$species == "Striped Skunk"){
      list(src = "StripedSkunk.jpg", height = 240, width = 300)
    }
    else if(input$species == "Flying Squirrel"){
      list(src = "FlyingSquirrel.jpg", height = 240, width = 300)
    }
    else if(input$species == "Snowshoe Hare"){
      list(src = "SnowshoeHare.jpg", height = 240, width = 300)
    }
    else if(input$species == "River Otter"){
      list(src = "RiverOtter.jpg", height = 240, width = 300)
    }
    else if(input$species == "Mink"){
      list(src = "Mink.jpg", height = 240, width = 300)
    }
  })
  
}



shinyApp(ui = ui, server = server)

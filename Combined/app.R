library(tidyverse)
ui <- fluidPage(
  
  # App title ----
  titlePanel("North Country Wild Zooniverse Project"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput("forest", h3("Choose your Forest"),
                  choices = c("South Hammond", "Donnerville", "Beaver Creek", "Whippoorwill Corners", "Whiskey Flats", "Degrasse"), selected = "South Hammond"),
      
      checkboxGroupInput("species", 
                         h3("Choose your Species"), 
                         choices = list("White-tailed Deer",
                                        "Chipmunk",
                                        "Coyote",
                                        "Fisher",
                                        "Raccoon",
                                        "Red Squirrel",
                                        "Other Small Mammal",
                                        "Gray Squirrel",
                                        "Black Bear",
                                        "Red Fox",
                                        "Porcupine",
                                        "Bobcat",
                                        "Opossum",
                                        "Weasel",
                                        "Striped Skunk",
                                        "Flying Squirrel",
                                        "Snowshoe Hare",
                                        "River Otter",
                                        "Mink"),
                         selected = "White-tailed Deer"),
    
    img(src = "NatureUpNorth.png", height = 100, width = 300),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = "foresthist")
    )
  )
)

server <- function(input, output) {
  
  
  
  
  output$foresthist <- renderPlot({
    choices<-c(input$species)
    data<-newData %>% filter(Species %in% choices)
    study<-reactive(switch(input$forest,
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
        })
    
  
}
shinyApp(ui = ui, server = server)
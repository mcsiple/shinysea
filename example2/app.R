# Rladies Shiny workshop
# Example 2

library(shiny)
library(tidyverse)
library(xtable)
library(kableExtra)
load("ZurichDogs.RData")

# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("Dogs of Zurich"),
   
   # Sidebar with a dropdown menu for breed
   sidebarLayout(
      sidebarPanel(
         selectInput("breed",
                     "Breed:",
                     choices=unique(dogs$BREED))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h3("City-wide summary"),
        plotOutput("gridPlot"),
         p(),
         p(),
         column(6, # column() modifies the layout (# is the column width)
         h4("Most popular breeds in district")),
         column(6,
        plotOutput("distPlot"))
         
      )
   )
)

# Server logic
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # count of chosen breed x by district
      dogs %>%
       filter(BREED==input$breed) %>%
       ggplot(aes(x=factor(DISTRICT))) +
       xlab("District") +
       ylab("Number of dogs") +
       ggtitle(paste("Count of",input$breed, "\n in each district",sep=" ")) +
       geom_bar(fill="#74CEB7") +  #
       theme_classic(base_size=14)
   })
   
   # use inputs here to subset the data to the user's district of choice:
   output$breedTable <- renderTable({
    df <- dogs %>%
       # filter(DISTRICT == _____ ) %>%
       group_by(BREED) %>%
       summarize(total.dogs = length(HALTER_ID)) %>% 
       arrange(desc(total.dogs)) %>%
       as.data.frame() 
       df[1:10,]
   })
   
   output$gridPlot <- renderPlot({
     
     most.popular.breeds <- dogs %>% 
       group_by(BREED) %>% 
       summarize(total=length(HALTER_ID)) %>% 
       top_n(10) %>% as.data.frame()
     
     dogs %>% filter(BREED %in% most.popular.breeds$BREED) %>%
       group_by(BREED,DISTRICT) %>% 
       summarize(dist.total = (length(HALTER_ID))) %>%
       ggplot(aes(x = BREED, y = factor(DISTRICT),size=dist.total))+
       geom_point() + 
       theme_classic(base_size=16) + 
       theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
       ylab("District") +
       xlab("Dog breed")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


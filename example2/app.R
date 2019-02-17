# Rladies Shiny workshop
# Example 2

# If you need these packages:
# install.packages(c("tidyverse","xtable","kableExtra"))

# Make sure your working directory is set to the location of this file
# From RStudio: Session --> Set Working Directory -->To Source File Location
# If you get a string error at some point, run the following before running the app: Sys.setlocale('LC_ALL','C') 
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
         selectInput(inputId = "breed",
                     label = "Breed:",
                     choices = unique(dogs$BREED),
                     selected = "Shih Tzu") # sets default selection
      ),
      
      # Show a plot of the city-wide distribution
      mainPanel(
         column(6, # column() modifies the layout (# is the column width)
         h4("Most popular breeds in district")
         # Add breedTable here
         ), 
         p(), # a line break
         p(),
         column(6,
        plotOutput("distPlot")),
        h3("City-wide summary"),
        plotOutput("gridPlot"))
         
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
     
     # Create a table of the top 10 most popular breeds in the city
     most.popular.breeds <- dogs %>% 
       group_by(BREED) %>% 
       summarize(total=length(HALTER_ID)) %>% 
       top_n(10) %>% as.data.frame()
     
     # Subset to data for the most popular breeds
     data <- dogs %>% filter(BREED %in% most.popular.breeds$BREED) %>%
       group_by(BREED,DISTRICT) %>% 
       summarize(dist.total = (length(HALTER_ID))) %>%
       ggplot(aes(x = BREED, y = factor(DISTRICT),size=dist.total,colour=dist.total))+
       geom_point(alpha=0.5) +
       theme_classic(base_size=16) + 
       theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
       ylab("District") +
       xlab("Dog breed")
     data
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


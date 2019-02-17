# Rladies Shiny workshop
# Example 1 - 'solutions'

# This is a Shiny web application. In Rstudio you can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(scales)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("A simple app"),
  # The following text will appear below the title. h1 is big text, h6 is small.
  h5("This app plots orange tree data from three different trees."),
  # Sidebar with checkbox input 
  sidebarLayout( # This defines the layout of the whole page.
    # Other options include tabsetPanel(), navlistPanel()
    sidebarPanel(  # Everything in sidebarPanel() describes the contents of the sidebar in the UI
      checkboxGroupInput(inputId = "treeID", # inputID links ui to the server logic
                         label = "Tree number:", # The label is what the user sees
                         c("1" = 1, 
                           "2" = 2,
                           "3" = 3),
                         selected="1" # Default selection
      )
    ),
    
    # Show plots
    mainPanel(
      h6("Age-circumference relationships for three orange trees"),
      plotOutput("orangePlot"),
      h6("A histogram of circumference"),
      plotOutput("orangeHist")
    )
  ) # end of sidebarLayout
)

# Define server logic required to plot
server <- function(input, output) {
  
  # Scatterplot
  output$orangePlot <- renderPlot({
    # subset to the trees that the user selected:
    which.trees <- input$treeID
    tree <- subset(Orange,Tree %in% which.trees)
    
    # Plot data for the selected trees:
    x <- tree$age
    y <- tree$circumference
    plot(x,y,
         xlab="Age (days)",ylab="Circumference (mm)",
         pch=19, cex=2,col=alpha("orange",0.5))
  })
  
  # Histogram
  output$orangeHist <- renderPlot({
    which.trees <- input$treeID
    tree <- subset(Orange,Tree %in% which.trees) 
    # Notice, you have to define tree again in this function
    
    hist(tree$circumference,
         col=alpha("orange",0.5),
         main="",
         xlab="Circumference (mm)")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


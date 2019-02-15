# Example 3: This app allows users to
# - upload a csv
# - download a report after performing some analysis

library(shiny)

ui <- fluidPage(
titlePanel("Uploading Files"),
# Sidebar layout with input and output definitions ----
sidebarLayout(
  # Sidebar panel for inputs ----
  sidebarPanel(
    # Input: Select a file ----
    fileInput("file1", "Choose CSV File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
              tags$hr(),
              checkboxInput("header", "Header", TRUE)),
   mainPanel(
    tableOutput("contents"),
    plotOutput("sizescatter"))
  ) 
)

server <- function(input, output) {
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    dat <- read.csv(inFile$datapath, header = input$header)
    head(dat)
  })
  
  # If you want the object dat to be available in multiple functions, you have to define it as a reactive object
  # This is particularly helpful if you are:
  # - retrieving a large amt of data from a web source
  # - performing some kind of analysis and want the results of that analysis to be available to all your objects (e.g., model fits)
  inFile2 <- reactive({read.csv(input$file1$datapath,header = input$header)})

  output$contents <- renderTable({
    head(inFile2())
  })

  output$sizescatter <- renderPlot({
    dat <- inFile2()
    ggplot(dat, aes(x=weight.w,y=rings,colour=sex)) +
      geom_point()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


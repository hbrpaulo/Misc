library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
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
      
      # Horizontal line ----
      tags$hr(),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      sliderInput("disp", "Display",
                  min = 2, max = 25, value = 6)
    ),
    # Main panel for displaying outputs ----
    mainPanel = mainPanel(
      
      # Output: Data file ----
      DT::DTOutput("contents")
      
    )
  )
)

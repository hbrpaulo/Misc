server <- function(input, output) {
  
  output$contents <- DT::renderDT({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file
    
    req(input$file1)
    
    tryCatch(
      {
        df <- readxl::read_excel(input$file1$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    return(DT::datatable(df[, c(3, 4, 34)]))
    
  })
  
}


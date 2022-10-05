server <- function(input, output) {
  
  output$contents <- renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
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
    
    return(DT::datatable(df))
    
  })
  
}

# suspended ----

# Lógica do servidor
server <- function(input, output, session) {
  
  alpha <- .05
  metrics <- indicators <- list()
  #source('scripts/Misc/diagrama.R', local = TRUE)
  source('scripts/Misc/example_creation.R')
  source('scripts/ReferenceInterval.R', local = TRUE)
  source('scripts/tests/trends_test.R', local = TRUE)
  source('scripts/tests/seasonality_test.R', local = TRUE)
  
  output$preview <- renderTable({
    head(resi_cs)
  })
  
}
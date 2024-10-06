
# Lógica do servidor
server <- function(input, output, session) {
  
  divisor = .2
  source('scripts/Misc/diagrama.R', local = TRUE)
  source('scripts/Misc/example_creation.R')
  source('scripts/ReferenceInterval.R', local = TRUE)
  
  output$preview <- renderTable({
    head(resi_cs)
  })
  
}
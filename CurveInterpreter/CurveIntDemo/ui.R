library(shiny)
library(tibble)
library(ggplot2)

# Interface do usuário
ui <- fluidPage(
  titlePanel("Upload e Seleção de Colunas"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Carregue o arquivo CSV", accept = ".csv"),
      
      # Selecionar a variável 'res' obrigatoriamente
      uiOutput("curve_column"),
      
      # Opção A: Condicional de seleção da variável 'ref'
      checkboxInput("optionA", "Selecionar a coluna referência?", FALSE),
      uiOutput("ref_column"),
      
      # Opção B: Se Opção A for selecionada, escolher opcionalmente ref_sup e ref_inf
      conditionalPanel(
        condition = "input.optionA == true",
        checkboxInput("optionB", "Selecionar ref_sup e ref_inf?", FALSE),
        uiOutput("ref_sup_column"),
        uiOutput("ref_inf_column")
      )
    ),
    
    mainPanel(
      tableOutput("preview"),
      plotOutput("ggplotf")
    )
  )
)


#shinyApp(ui = ui, server = server)

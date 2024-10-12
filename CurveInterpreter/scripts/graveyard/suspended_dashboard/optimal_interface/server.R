
# Lógica do servidor
server <- function(input, output, session) {
  
  default_data <- reactive({
    url <- "https://raw.githubusercontent.com/hbrpaulo/Misc/refs/heads/main/CurveInterpreter/example_data.csv"
    read.csv(url)
  })
  
  # Carregar o arquivo ou usar o dataset padrão
  data <- reactive({
    if (is.null(input$file)) {
      # Se o arquivo não foi carregado, use o dataset padrão
      default_data()
    } else {
      # Caso contrário, use o arquivo carregado
      read.csv(input$file$datapath)
    }
  })
  
  # Criar seletor de colunas para 'res'
  output$curve_column <- renderUI({
    req(data())
    selectInput("res", "Selecione a coluna da curva:", choices = names(data()), selected = names(data())[1])
  })
  
  # Criar seletor de colunas para 'ref' (mostrado se optionA for TRUE)
  output$ref_column <- renderUI({
    req(data(), input$optionA)
    if (input$optionA) {
      selectInput("ref", "Selecione a coluna ref:", choices = names(data()))
    }
  })
  
  # Criar selectores de colunas para 'ref_sup' e 'ref_inf' (mostrado se optionB for TRUE)
  output$ref_sup_column <- renderUI({
    req(data(), input$optionA, input$optionB)
    if (input$optionB) {
      selectInput("ref_sup", "Selecione a coluna ref_sup:", choices = names(data()))
    }
  })
  
  output$ref_inf_column <- renderUI({
    req(data(), input$optionA, input$optionB)
    if (input$optionB) {
      selectInput("ref_inf", "Selecione a coluna ref_inf:", choices = names(data()))
    }
  })
  
  # Mostrar preview dos dados
  output$preview <- renderTable({
    req(data())
    head(data())
  })
  
  output$ggplotf <- renderPlot({
    req(data())
    
    # Criando um tibble a partir dos dados
    plot_data <- tibble(data())
    
    #if (!is.null(input$ref){
    p <- ggplot(plot_data, aes(x = 1:nrow(plot_data), y = get(input$res))) +
      geom_line() +# Gráfico da coluna res
      ggtitle('Grafico sem referencia')
    #}

    # Adicionar a linha ref apenas se o usuário selecionou a Opção A
    if (input$optionA && !is.null(input$ref)) {
      p <- p + geom_line(aes_string(y = input$ref), col = 'red') +# Linha da ref
        ggtitle('Gráfico com referência')
    }

    # Adicionar ref_sup e ref_inf apenas se o usuário selecionou a Opção B
    if (input$optionB && !is.null(input$ref_sup) && !is.null(input$ref_inf)) {
      p <- p +
        geom_line(aes_string(y = input$ref_inf), col = 'red', lty = 2) + # Linha de ref_inf
        geom_line(aes_string(y = input$ref_sup), col = 'red', lty = 2) + # Linha de ref_sup
        ggtitle('Gráfico com referência intervalar')
    }
    
    # Renderizar o gráfico
    print(p)
  })
}
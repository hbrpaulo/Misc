opcoes <- c("Option 1", "Option 2", "Option 3")

# Usando a função menu para escolher uma das opções
reference <- askYesNo("Ha referencia?", default = TRUE)

if(reference==TRUE){
has_interval <- askYesNo("Ha referencia intervalar?", default = TRUE)
}

rmarkdown::render('Vignette.Rmd')

# choose between those 3 options
opcoes[menu(opcoes, title = "Choose an option")]

# Definir as opções do menu
opcoes <- c("Categoria 1", "Categoria 2", "Categoria 3")

# Criar uma janela para o usuário escolher uma opção
escolha <- select.list(opcoes, title = "Escolha uma categoria:")

# Mostrar a categoria selecionada
cat("Você escolheu:", escolha, "\n")

library(tcltk)
tkmessageBox(title = "Greetings from R TclTk",
             message = "Hello, world!", icon = "info", type = "ok")
# for the one displayed by q() :
tkmessageBox(message = "Do you want to save before quitting?",
             icon = "question", type = "yesnocancel", default = "yes")
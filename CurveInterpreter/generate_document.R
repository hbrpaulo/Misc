opcoes <- c("Option 1", "Option 2", "Option 3")

# Usando a função menu para escolher uma das opções
escolha <- menu(opcoes, title = "Please choose one option:")

rmarkdown::render('Vignette.Rmd')

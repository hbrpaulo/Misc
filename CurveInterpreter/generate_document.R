# Usando a função askYesNo para escolher qual tipo de documento será gerado
# análogo a utilização de shiny

reference <- askYesNo("Há referência?", default = TRUE)

if(reference==TRUE){
has_interval <- askYesNo("A referência é intervalar?", default = TRUE)
}

DocumentType <- reference+has_interval

rmarkdown::render('Vignette.Rmd')

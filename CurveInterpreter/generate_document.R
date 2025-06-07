path_one <- stringr::str_split(rstudioapi::getActiveDocumentContext()$path, '/', simplify = TRUE)
setwd(paste0(path_one[-length(path_one)], collapse = '/'))

# Usando a função askYesNo para escolher qual tipo de documento será gerado
# análogo a utilização de shiny

reference <- askYesNo("Há referência?", default = TRUE)

if(reference==TRUE){
has_interval <- askYesNo("A referência é intervalar?", default = TRUE)
}

DocumentType <- reference+has_interval

if(!exists('DocumentType')){
  DocumentType <- 2
}

rmarkdown::render('Vignette.Rmd')
# ui
source('https://raw.githubusercontent.com/hbrpaulo/Misc/main/finance_app/scripts/ui.R')
#source('scripts/ui.R')
# server
source('https://raw.githubusercontent.com/hbrpaulo/Misc/main/finance_app/scripts/server.R')
#source('scripts/server.R')
shinyApp(ui, server)

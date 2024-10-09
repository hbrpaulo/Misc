{library(zoo)
library(shiny)
library(bslib)
library(tidyverse)
library(DiagrammeR)}

# Interface do usuário
ui <- page_fluid(title = 'v0.2 CurveInterpreter',
                 navset_tab(
                   nav_panel("Referência intervalar",
                             fluidPage(
                               verbatimTextOutput('outside'),
                               plotOutput("ggplotff"),
                               tableOutput("preview"),
                               verbatimTextOutput('trends')
                             )
                   ),
                   nav_panel("Sumário",
                             DiagrammeROutput('diagram')
                   ),
                   nav_panel("Referência pontual",
                             fluidPage(
                               mainPanel(
                               )
                             )
                   ),
                   nav_panel("Sem referência",
                             fluidPage(
                               mainPanel(
                               )
                             )
                   )
                 ), 
                 id = "tab")

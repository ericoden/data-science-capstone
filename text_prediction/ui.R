
library(shiny)

shinyUI(fluidPage(
    tags$head(
        tags$link(rel = 'stylesheet', type = 'text/css', href = 'dark_mode.css')
    ),
    tags$h2("Text Prediction"),
    tags$div(class='box',
        textAreaInput('user_text', label = NULL, width = '400px', placeholder = 'Enter text here'),
        tags$div(id='buttons', 
                 uiOutput("predictionButtons"))
    )
))

library(shiny)
source("predict.R")
#library('KeyboardSimulator')
shinyServer(function(input, output, session) {
    output$predictionButtons <- renderUI({
        predictions <- ngram_predict(input$user_text)
        if(length(predictions) == 3){
            list(
            actionButton('prediction_a', label = predictions[1]),
            actionButton('prediction_b', label = predictions[2]),
            actionButton('prediction_c', label = predictions[3])
            )
        }
        else if(length(predictions) == 2){
            list(
            actionButton('prediction_a', label = predictions[1]),
            actionButton('prediction_b', label = predictions[2])
            )
        }
        else if(length(predictions) == 1){
            list(
            actionButton('prediction_a', label = predictions[1])
            )
        }
    })
    
    updateTextField <- function(i){
        prediction = ngram_predict(input$user_text)[i]
        if (startsWith(prediction, ' ')){
            updateTextInput(session, "user_text", value=paste0(input$user_text, prediction))
        }
        else
        {   
            new_words <- c(words(input$user_text)[1:length(words(input$user_text))-1], prediction)
            new_text <- paste(new_words, sep=' ', collapse=' ')
            updateTextInput(session, "user_text", value=new_text)
        }
    }
    observeEvent(input$prediction_a, {
        updateTextField(1)        
        #keybd.press('Shift', hold=TRUE)
        #keybd.press('Tab')
        #keybd.release('Shift')
        })
    observeEvent(input$prediction_b, {
        updateTextField(2)
       # keybd.press('Shift', hold=TRUE)
      #  keybd.press('Tab')
     #   keybd.press('Tab')
    #    keybd.release('Shift')
        })
    observeEvent(input$prediction_c, {
        updateTextField(3)
       # keybd.press('Shift', hold=TRUE)
    #    keybd.press('Tab')
     #   keybd.press('Tab')
      #  keybd.press('Tab')
       # keybd.release('Shift')
        })
    })
                                    
library(shiny)
load("unigram_nostop.Rda")
load("bigram_nostop.Rda")
load("trigram_nostop.Rda")
load("quadgram_nostop.Rda")
library(stringr)
library(stringi)
library(tm)
library(NLP)
source("prediction_model.R")

shinyServer(function(input, output,session) {
  prediction = reactive({
    wordproc(input$inputString)
  })
  
  output$message = renderText(prediction()[length(prediction())])
  
  #Reactive prediction
  
  value = reactiveValues(buttons = list(actionButton(inputId = "button1",
                                                     label = 1)))
  #observe when input changes
  observeEvent(eventExpr = input$inputString,
               handlerExpr = {
                 len <- length(prediction()[-length(prediction())])
                 value$buttons=list()
                 for (i in 1:len){
                   value$buttons[[i]] = actionButton(inputId = paste0("button",i),
                                                     label = prediction()[-length(prediction())][i])
                 }
               })
  #render buttons
  output$suggestions = renderUI({
    value$buttons
  })
  #add observeEvent to buttons
  for(ii in 1:10){
    local({
      i = ii
      observeEvent(eventExpr = input[[paste0("button",i)]],
                   handlerExpr = {updateTextInput(session,'inputString',
                                                  value=paste(input$inputString,prediction()[-length(prediction())][i]))})
    })
  }
})

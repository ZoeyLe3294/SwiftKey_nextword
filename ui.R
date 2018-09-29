library(shiny)


shinyUI(fluidPage(
  
  #Application title
  titlePanel(strong("Coursera Capstone Project - SwiftKey Next Word Prediction")),
  sidebarLayout(
    sidebarPanel(
      h3(strong("INSTRUCTION"),style = "color:blue"),
      p("- Feel free type in the box or click on suggestions below"),
      p("- The prediction algorithm is used back-off model. There will be a note of from what n-gram tokenizer which has successfully sent out the results."),
      p('- For the references, click on the "Codebook" tab to see data overview and modeling coding details')
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Prediction",
                           h3(textAreaInput("inputString", "Enter here:",width = '300%',rows = 4)),
                           uiOutput('suggestions'),
                           br(),
                           h3("Note:"),
                           tags$span(style="color:darkred",
                                     tags$strong(textOutput("message")))),
                  tabPanel("Codebook", includeMarkdown("Codebook.Rmd")))
      
    )
  )
  
))

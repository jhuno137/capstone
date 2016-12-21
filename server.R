library(shiny)
library(htmltools)

# prediction algorithm
source("stupid_backoff.R")

# Helper functions
getPrediction <- function(input){
    input <- trimws(input)
    input <- tolower(input)
    input <- gsub("[^a-z' ]","",input)
    nxtWord$predict(input)$prediction
}

history <- ""

# Define server logic
shinyServer(function(input, output){
    predictions <- reactive({
        getPrediction(input$userInput)
    })
    
    addHistoryLine <- eventReactive(input$sendButton, {
        if(input$userInput != "" && !identical(input$userInput,character(0))){
            lineTxt <- htmlEscape(input$userInput)
            lineTxt <- paste("<div class='line'>",lineTxt,'</div>')
            history <<- paste(history,lineTxt)
        }
        history
    })
    
    observeEvent(input$clearButton,{ history <<- "" })
    
    output$predOutput1= renderText({
        predictions()[3]
    })
    output$predOutput2= renderText({ 
        predictions()[1]
    })
    output$predOutput3= renderText({ 
        predictions()[2]
    })
    output$history = renderText({
        addHistoryLine()
    })
})

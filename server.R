library(shiny)
library(htmltools)

# prediction algorithm
source("stupid_backoff.R")

# Helper function to get predictions
getPrediction <- function(input){
    input <- trimws(input)
    input <- tolower(input)
    input <- gsub("[^a-z' ]","",input)
    nxtWord$predict(input)$prediction
}

# Define server logic
shinyServer(function(input, output){
    # Use reactiveValues() to maintain an object for multiple action buttons 
    # to interact with
    history <- reactiveValues(data=NULL)
    # A reactive expression is an R expression that uses widget input and 
    # returns a value. The reactive expression will update this value whenever 
    # the original widget changes.
    predictions <- reactive({
        getPrediction(input$userInput)
    })
    # observeEvent(), eventReactive() isolates the block of code in its second 
    # argument with isolate()
    observeEvent(input$sendButton,{
        if(input$userInput != "" && !identical(input$userInput,character(0))){
            lineTxt <- htmlEscape(input$userInput)
            lineTxt <- paste("<div class='line'>",lineTxt,'</div>')
            history$data <- paste(history$data,lineTxt)
        }
    })
    observeEvent(input$clearButton,{ history$data <- "" })
    # predictions
    output$predOutput1= renderText({ predictions()[3] })
    output$predOutput2= renderText({ predictions()[1] }) # highest probbability!
    output$predOutput3= renderText({ predictions()[2] })
    # html output with user history
    output$history = renderText({ history$data })
})

#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    answer <- reactive({
        combined_predict(input$starterInput, model20, dict20, TRUE)
    })
    
    output$preds <- renderTable({
        data.frame(Rank=c("1","2","3"), Prediction=answer()[[1]])
    })
    
    output$path <- renderPrint({
        data <- answer()
        print(data[[2]])
    })
    

})

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("SwiftPredict"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("starterInput", "Type your text here")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Predictions",
                         h3("Predictions"),
                         tableOutput("preds"),
                         h3("Matching ngrams"),
                         verbatimTextOutput("path")
                         ),
                tabPanel("How it works",
                         includeHTML("how-it-works.html"))
            )
        )
    )
))

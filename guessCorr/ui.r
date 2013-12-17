# Shiny app to guess correlations.

library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Guessing Correlations"),
  
  sidebarPanel(
    numericInput('guess', 'Estimated Correlation:', 0),
    actionButton('submit', 'Submit Guess'),
    br(),
    checkboxInput('bestfit', 'Show line of best fit', value = FALSE),
    br(),
    br()
    ),
  
  mainPanel(
    verbatimTextOutput('corrvalue'),
    plotOutput('plot', height = '600px')
    )  
  )  
  )
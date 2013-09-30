# This example converts the simulating confidence intervals seen here:
# http://www.rossmanchance.com/applets/NewConfsim/Confsim.html
# to a shiny app.

library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Sampling Distributions"),
  
  sidebarPanel(
    
    numericInput("popmean", "Parameter Mean:", 0),
    numericInput("popsd", "Parameter SD:", 1),
    numericInput("sampsize", "Sample Size:", 25),
    numericInput("numsamp", "Number of Samples:", 100),
    br(),
    sliderInput("conflevel", "Confidence Level", min = 0, max = 1, 
                value = .95, step = .01), 
    
    actionButton("goButton", "Go!")),
  
  mainPanel(
    plotOutput("plot", height="600px")
  )  
))
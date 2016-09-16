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
    br(),
    checkboxInput("restrict", "Restriction of Range"),
    conditionalPanel(condition = "input.restrict == true",
                     sliderInput("x_range", "Range:",
                                 min = -3, max = 3, step = .25, value = c(-1, 2)),
                     checkboxInput('fitrestrict', 'Restricted range best fit', value = FALSE)),
    br(),
    helpText('Guess the correlation from the scatterplot.  Add the line of best fit to the scatterplot
             to help aid in guessing the correlation.')
    ),
  
  mainPanel(
    verbatimTextOutput('corrvalue'),
    verbatimTextOutput('data'),
    plotOutput('plot', height = '600px')
    )  
  )  
  )
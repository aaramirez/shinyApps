# Histogram application

hist_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  div(
    h2('Exploring Histograms'),
    fluidRow(
      box(title = 'Data Options', status = 'info',
          width = 4, collapsible = TRUE, collapsed = FALSE,
          selectInput(ns("dataset"), "Choose a dataset:", 
                      choices = c("rock", "pressure", "diamonds", "pokemon"))
      ),
      box(title = 'Plot Output', status = 'primary',
        width = 8,
        dataTableOutput(ns('table'))
      )
    )
  )
  
}

hist_module <- function(input, output, session) {
  
  # Return the requested dataset
  data_input <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "diamonds" = diamonds,
           "pokemon" = pokemon)
  })
  
  output$table <- renderDataTable(data_input())
  
}

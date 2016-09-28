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
                      choices = c("rock", "pressure", "diamonds", "pokemon")),
          sliderInput('binwidth', label = 'Select Binwidth')
      ),
      column(width = 8,
             box(title = 'Plot Output', status = 'info',
                 collapsible = TRUE, collapsed = FALSE,
                 dataTableOutput(ns('table'))
             ),
             box(title = 'Plot Output', status = 'primary',
                 collapsible = TRUE, collapsed = FALSE,
                 plotOutput(ns('plot'), height = '600px')
             )
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
  
  output$table <- renderDataTable({
    data_input()
  })
  
  output$plot <- renderPlot({
    ggplot(input$dataset, aes(x = input$variable)) + theme_bw(base_size = 20) + 
      geom_histogram()
  })
  
}

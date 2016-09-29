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
                             choices = c("rock", "pressure", "diamonds", "pokemon"),
                             selected = 'pokemon'),
                 hr(),
                 uiOutput(ns('vars')),
                 hr(),
                 sliderInput(ns('binwidth'), label = 'Select Binwidth',
                             min = 1, max = 100, value = 5, step = 1),
                 hr(),
                 checkboxInput(ns('interact'), 'Interactive Plot?', 
                               value = FALSE)
             ),
             box(title = 'Plot Output', status = 'primary',
                 width = 8, collapsible = TRUE, collapsed = FALSE,
                 conditionalPanel(
                   condition = sprintf("input['%s'] == ''", ns("interact")),
                   plotOutput(ns('plot'), height = '600px')
                 ),
                 conditionalPanel(
                   condition = sprintf("input['%s'] != ''", ns("interact")),
                   highchartOutput(ns('highbar'))
                 )
             )
      ),
      fluidRow(
        box(title = 'Data Table', status = 'info', solidHeader = TRUE,
            width = 12, collapsible = TRUE, collapsed = FALSE,
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
  
  output$vars <- renderUI({
    selectInput(session$ns('x_vars'), 'Select X Variable',
                choices = names(data_input()))
  })
  
  output$table <- renderDataTable({
    data_input()
  })
  
  output$plot <- renderPlot({
    ggplot(data_input(), aes_string(input$x_vars)) + 
      theme_bw(base_size = 20) + 
      geom_histogram(binwidth = input$binwidth)
  })
  
  data_count <- reactive({
    tmp <- data_input()[input$x_var]
    tmp_min <- min(tmp)
    tmp_max <- max(tmp)
    bins <- seq(tmp_min:tmp_max, by = input$binwidth)
    if(!any(bins != tmp_max)) {
      bins <- c(bins, tmp_max)
    }
    
    h <- hist(tmp, breaks = bins, plot = FALSE, right = FALSE)
    
    as.data.frame(breaks = h$breaks[-1], n = h$counts)
  })
  
  output$highbar <- renderHighchart({
    highchart() %>%
      hc_add_series_df(data = data_count(), type = 'column',
                       y = n, x = breaks) %>%
      hc_plotOptions(
        column = list(groupPadding = 0, pointPadding = 0, 
                   borderWidth = 0, shadow = FALSE)
      )
  })
  
}

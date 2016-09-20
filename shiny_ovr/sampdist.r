
sampdist_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  div(
    h2("Sampling Distributions"),
    
    fluidRow(
      box(title = 'Options', status = 'info',
          width = 4, 
          numericInput(ns("sampsize"), "Sample Size:", 25),
          numericInput(ns("numsamp"), "Number of Samples:", 100),
          numericInput(ns("popvalue"), "Parameter:", .45),
          hr(),
          
          checkboxInput(ns("normcurve"), "Draw Normal Curve", FALSE),
          
          actionButton(ns("goButton"), "Go!")
        ),
      box(title = 'Sampling Distribution', status = 'primary',
          width = 8,
          plotOutput(ns("plot"), height="600px")
      )
    )
  )
  
}

sampdist_module <- function(input, output, session) {
  # Computing proportion of orange candies
  dataset <- eventReactive(input$goButton, {
      data.frame(prob = rbinom(n = input$numsamp, size = input$sampsize, 
                               prob = input$popvalue)/input$sampsize)
  })
  
  output$plot <- renderPlot({
      rangeC <- max(dataset()$prob) - min(dataset()$prob)
      
      if(input$normcurve) {
        
        dataNorm <- data.frame(norm = rnorm(10000, mean = input$popvalue,
                                            sd = sqrt(((input$popvalue)*1-input$popvalue)/input$numsamp)))
        # Building histogram of sampling distribution with normal curve
        p <- ggplot(dataset(), aes(x = prob)) 
        p <- p + geom_histogram(aes(y = ..density..), fill = "steelblue", 
                                color = "black", binwidth = rangeC/20) +
          geom_density(data = dataNorm, aes(x = norm), color = "darkgreen", 
                       size = 1.25, alpha = 0) +
          theme_bw(base_size = 24)+ labs(title = paste("Mean = ", round(mean(dataset()$prob), 3), 
                                                       "; SE = ", 
                                                       round(sqrt(mean(dataset()$prob)*
                                                                    (1-mean(dataset()$prob))/input$sampsize), 3)))
        
        print(p)
      } else {
        # Building histogram of sampling distribution
        p <- ggplot(dataset(), aes(x = prob)) 
        p <- p + geom_histogram(fill = "steelblue", color = "black", 
                                binwidth = rangeC/20) +
          theme_bw(base_size = 24) + labs(title = paste("Mean = ", round(mean(dataset()$prob), 3), 
                                                        "; SE = ", 
                                                        round(sqrt(mean(dataset()$prob)*
                                                                     (1-mean(dataset()$prob))/input$sampsize), 3)))
        
        print(p)
      }  
  })
}

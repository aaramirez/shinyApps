# This example converts the Reeses Pieces Applet seen here:
# http://www.rossmanchance.com/applets/Reeses/ReesesPieces.html
# to a shiny app.

library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  # Computing proportion of orange candies
  dataset <- reactive({
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
        theme_bw()+ labs(title = paste("Mean = ", round(mean(dataset()$prob), 3), 
                 "; SE = ", 
                 round(sqrt(mean(dataset()$prob)*
                              (1-mean(dataset()$prob))/input$sampsize), 3)))
      
      print(p)
    } else {
      # Building histogram of sampling distribution
      p <- ggplot(dataset(), aes(x = prob)) 
      p <- p + geom_histogram(fill = "steelblue", color = "black", 
                              binwidth = rangeC/20) +
        theme_bw() + labs(title = paste("Mean = ", round(mean(dataset()$prob), 3), 
                  "; SE = ", 
                  round(sqrt(mean(dataset()$prob)*
                               (1-mean(dataset()$prob))/input$sampsize), 3)))
      
      print(p)
    }
        
  })
  
})
                            
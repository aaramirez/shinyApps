# This example converts the simulating confidence intervals seen here:
# http://www.rossmanchance.com/applets/NewConfsim/Confsim.html
# to a shiny app.

library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  # Computing proportion of orange candies
  dataset <- reactive({
    if(input$goButton > 0 | TRUE) {
    replicate(input$numsamp, 
              rnorm(n = input$sampsize, mean = input$popmean, sd = input$popsd))
    }
  })
  
  output$plot <- renderPlot({
    
    # compute means and sd for data
    confnums <- data.frame(means = apply(dataset(), 2, mean),
                           se = apply(dataset(), 2, sd)/sqrt(input$sampsize))
    
    # compute upper/lower limits of confidence interval
    confnums$lb <- with(confnums, means - qnorm(1-(1-input$conflevel)/2)*se)
    confnums$ub <- with(confnums, means + qnorm(1-(1-input$conflevel)/2)*se)
    
    # indicator whether interval contains mean
    confnums$containMean <- ifelse(confnums$lb > input$popmean | 
                                     confnums$ub < input$popmean, 0, 1)
    confnums$x <- 1:nrow(confnums)
    
    # Building histogram of sampling distribution
    p <- ggplot(confnums, aes(y = means, x = x, ymax = ub, ymin = lb, 
                              color = factor(containMean))) 
    p <- p + geom_pointrange(size = .75) + theme_bw(base_size = 24) + 
      scale_color_manual("", values = c("red", "forestgreen")) + 
      theme(legend.position = "none") +
      geom_hline(yintercept = input$popmean, size = 1) + 
      theme(axis.text.x = element_blank(), axis.title.x = element_blank()) + 
      #scale_y_continuous("", limits = c(min))
      labs(title = paste("Intervals Containing Mean = ", sum(confnums$containMean),
                         "/", input$numsamp, " = ",
                         round(mean(confnums$containMean)*100, 0), "%"))
    
    print(p)
    
  })
  
})
                            
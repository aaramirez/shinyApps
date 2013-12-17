# Shiny app to guess correlations.

library(shiny)
library(ggplot2)
#library(MASS)


shinyServer(function(input, output){
  
  
  # Generate correlated data
  dataMat <- cbind(X = rnorm(500), Y = rnorm(500))
  
  genCorr <- runif(1, -1, 1)
  
  mat <- diag(1, 2, 2)
  delta <- row(mat) - col(mat)
  mat[delta != 0] <- genCorr
  
  # Cholesky decomposition
  dataMat2 <- data.frame(dataMat %*% chol(mat))
  names(dataMat2) <- c("X", "Y")
  corData <- cor(dataMat2)[1,2]

  # outputs
  output$plot <- renderPlot({
      p <- ggplot(dataMat2, aes(x = X, y = Y))
      p <- p + theme_bw(base_size = 24) + geom_point()
      
      if(input$bestfit == TRUE){
        p <- p + stat_smooth(method = lm, se = FALSE, size = 1)
      }
      
      print(p)      

    })
  output$corrvalue <- renderPrint({
    
    if(input$submit == 0){
        cat('Input Guess')
        }
    isolate({
         if(((input$guess > corData - .05 & input$guess < corData + .05))) {
        cat('Correct! \n', 'Correlation = ', corData)
      } else{ if (input$guess < corData - .05 & input$submit != 0) {
        cat('Try Again, Underestimating the Correlation')
      }  else {  if(input$guess > corData + .05 & input$guess != 0) { 
        cat('Try Again, Overestimating the Correlation')
      }}}
    })
  })
})
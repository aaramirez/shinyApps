# Shiny app to guess correlations.

library(shiny)
library(ggplot2)


shinyServer(function(input, output){
  
  dataMat2 <- reactive({  
  # Generate correlated data
  dataMat <- cbind(X = rnorm(500), Y = rnorm(500))
  
  genCorr <- runif(1, -1, 1)
  
  mat <- diag(1, 2, 2)
  delta <- row(mat) - col(mat)
  mat[delta != 0] <- genCorr
  
  # Cholesky decomposition
  dataMat2 <- data.frame(dataMat %*% chol(mat))
  names(dataMat2) <- c("X", "Y")
  
  
    if(input$restrict == TRUE){ 
    dataMat2$outside <- ifelse(dataMat2$X > max(input$x_range) | dataMat2$X < min(input$x_range), 
                               0, 1)
    }
  return(dataMat2)
  })
  
  corData <- reactive({
    corData <- cor(dataMat2())[1,2]
    return(as.numeric(corData))
  })
  
  corDataRest <- reactive({
    if(input$restrict == TRUE){
      corDataRest <- cor(subset(dataMat2(), outside == 1))[1,2]
      return(as.numeric(corDataRest))
    }
  })
  
  # outputs
  output$plot <- renderPlot({
    p <- ggplot(dataMat2(), aes(x = X, y = Y))
      p <- p + theme_bw(base_size = 24) + geom_point()
    
    if(input$bestfit == TRUE){
      p <- p + stat_smooth(data = dataMat2(), method = lm, se = FALSE, size = 1)
    }
      
      if(input$restrict == TRUE){
        #p <- ggplot(dataMat2(), aes(x = X, y = Y))
        p <- p + geom_vline(xintercept = input$x_range, linetype = "dashed") +
          geom_point(aes(color = factor(outside))) + 
          scale_color_manual(values = c("gray80", "black")) + theme(legend.position = "none") 
        if(corData() > 0){
          p <- p + annotate("text", x = -3, y = 2, size = 8, fontface = 3,
                            label = paste("r = ", as.character(round(as.numeric(corData()), 3)), sep =""))
        } else {
          p <- p + annotate("text", x = 3, y = 2, size = 8, fontface = 3,
                            label = paste("r = ", as.character(round(as.numeric(corData()), 3)), sep =""))
        }
        if(input$fitrestrict == TRUE){
          p <- p + stat_smooth(data = subset(dataMat2(), outside == 1), 
                               method = lm, se = FALSE, size = 1.5, color = "darkgreen")
        }
      }
      
   
      
      print(p)      

    })
  output$corrvalue <- renderPrint({
    
    if(input$submit == 0){
        cat('Input Guess')
        }
    isolate({
      if(input$restrict == FALSE){
         if(((input$guess > corData() - .05 & input$guess < corData() + .05))) {
        cat('Correct! \n', 'Correlation = ', round(corData(), 3))
      } else{ if((corData() > 0 & input$guess < corData() - .05 & input$submit != 0) |
                   (corData() < 0 & input$guess > corData() - .05 & input$submit != 0)){
        cat('Try Again, Underestimating the Correlation')
      }  else {  if((corData() > 0 & input$guess > corData() + .05 & input$guess != 0) |
                      (corData() < 0 & input$guess < corData() + .05 & input$guess != 0)){ 
        cat('Try Again, Overestimating the Correlation')
      }}}
      } else{
        if(((input$guess > corDataRest() - .05 & input$guess < corDataRest() + .05))) {
          cat('Correct! \n', 'Correlation = ', round(corDataRest(), 3))
        } else{ if((corDataRest() > 0 & input$guess < corDataRest() - .05 & input$submit != 0) |
                     (corDataRest() < 0 & input$guess > corDataRest() - .05 & input$submit != 0)){
          cat('Try Again, Underestimating the Correlation')
        }  else {  if((corDataRest() > 0 & input$guess > corDataRest() + .05 & input$guess != 0) |
                        (corDataRest() < 0 & input$guess < corDataRest() + .05 & input$guess != 0)){ 
          cat('Try Again, Overestimating the Correlation')
      }}}
      }        
    })
  })
  output$data <- renderPrint({
    c(corData(), corDataRest())
  })
})
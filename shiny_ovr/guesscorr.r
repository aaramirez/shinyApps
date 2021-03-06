# Guessing correlations modules

guesscorr_ui <- function(id){
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  div(
    h2("Guessing Correlations"),
    fluidRow(
      box(title = 'Data Options', status = 'info',
          width = 4, collapsible = TRUE, collapsed = FALSE,
          actionButton(ns('sim'), 'Generate Data'),
          hr(),
          numericInput(ns('guess'), 'Estimated Correlation:', 0),
          actionButton(ns('submit'), 'Submit Guess'),
          hr(),
          checkboxInput(ns('bestfit'), 'Show line of best fit', value = FALSE),
          hr(),
          checkboxInput(ns("restrict"), "Restriction of Range"),
          conditionalPanel(condition = sprintf("input['%s'] != ''", ns("restrict")),
                           sliderInput(ns("x_range"), "Range:",
                                       min = -3, max = 3, step = .25, value = c(-1, 2)),
                           checkboxInput(ns('fitrestrict'), 'Restricted range best fit', value = FALSE)),
          h4('Guess the correlation from the scatterplot.  Add the line of best fit to the scatterplot
             to help aid in guessing the correlation.')
          ),
      box(title = 'Plot Output', status = 'primary',
          width = 8,
          verbatimTextOutput(ns('corrvalue')),
          # verbatimTextOutput(ns('data')),
          plotOutput(ns('plot'), height = '600px')
      )
      )
    )
}

guesscorr_module <- function(input, output, session) {
  dataMat2 <- eventReactive(input$sim, {  
    # Generate correlated data
    dataMat <- cbind(X = rnorm(500), Y = rnorm(500))
    
    genCorr <- runif(1, -1, 1)
    
    mat <- diag(1, 2, 2)
    delta <- row(mat) - col(mat)
    mat[delta != 0] <- genCorr
    
    # Cholesky decomposition
    dataMat2 <- data.frame(dataMat %*% chol(mat))
    names(dataMat2) <- c("X", "Y")
    
    dataMat2$outside <- 0
    
    return(dataMat2)
  })
  
  corData <- reactive({
    corData <- cor(dataMat2())[1,2]
    return(as.numeric(corData))
  })
  
  dat3 <- reactive({
    dataMat2() %>%
      mutate(outside = ifelse(X < max(input$x_range) & X > min(input$x_range), 1, 0))
  })
  
  corDataRest <- reactive({
    if(input$restrict == TRUE){
      corDataRest <- cor(filter(dat3(), outside == 1))[1,2]
      return(as.numeric(corDataRest))
    }
  })
  
  # outputs
  output$plot <- renderPlot({
    p <- ggplot(dataMat2(), aes(x = X, y = Y))
    p <- p + theme_bw(base_size = 24) + geom_point() + coord_fixed()
    
    if(input$bestfit == TRUE){
      p <- p + stat_smooth(data = dataMat2(), method = lm, se = FALSE, size = 1)
    }
    
    if(input$restrict == TRUE){
      p <- ggplot(dat3(), aes(x = X, y = Y))
      p <- p + theme_bw(base_size = 24) + geom_point() + coord_fixed()
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
        p <- p + stat_smooth(data = subset(dat3(), outside == 1), 
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
  # output$data <- renderPrint({
  #   c(corData(), corDataRest())
  # })
}



ci_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  div(
    h2("Sampling Distributions"),
    
    fluidRow(
      box(title = 'Options', status = 'info',
          width = 4,
          numericInput(ns("popmean"), "Parameter Mean:", 0),
          numericInput(ns("popsd"), "Parameter SD:", 1),
          numericInput(ns("sampsize"), "Sample Size:", 25),
          numericInput(ns("numsamp"), "Number of Samples:", 100),
          hr(),
          sliderInput(ns("conflevel"), "Confidence Level", min = 0, max = 1, 
                      value = .95, step = .01), 
          
          actionButton(ns("goButton"), "Go!")
        ),
      box(title = 'Confidence Intervals', status = 'primary',
          width = 8,
          plotOutput(ns("plot"), height="600px")
        )
      )
    )
}

ci_module <- function(input, output, session) {
  # Computing proportion of orange candies
  dataset <- reactive({
    if(input$goButton > 0 | TRUE) {
      replicate(input$numsamp, 
                rnorm(n = input$sampsize, mean = input$popmean, sd = input$popsd))
    }
  })
  
  output$plot <- renderPlot({
    input$goButton
    # compute means and sd for data
    isolate({
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
}

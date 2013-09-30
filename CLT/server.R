# Derived from https://github.com/tgouhier/climit

library(shiny)

shinyServer(function(input, output) {
  
  data <- reactive({
    if(input$goButton > 0 | TRUE) {
    if (input$dist == "rbeta") {
      vals <-  do.call(input$dist, list(n=input$n, shape1=5, shape2=1))
    } else {
      vals <-  do.call(input$dist, list(n=input$n))
    }
    
    
    return (list(fun=input$dist, vals=vals))
    }
  })
  
  output$plot <- renderPlot({
    distname <- switch(input$dist,
                       rnorm = "Normal population",
                       rexp = "Right skewed population",
                       rbeta = "Left skewed population",
                       runif = "Uniform population")   
    n <- input$n
    k <- input$k
    pdist <- data()$vals

    
    
    if (input$dist == "rbeta") {
      x = replicate(k, do.call(input$dist, list(n=n,shape1=5,shape2=1)))
      pop = do.call(input$dist, list(n=1e5,shape1=5,shape2=1))
    } else {
      x = replicate(k, do.call(input$dist, list(n=n)))
      pop = do.call(input$dist, list(n=1e5))
    }
    
    m_pop =  round(mean(pop),2)
    sd_pop = round(sd(pop),2)
    
    ndist <- colMeans(x)
    
    m_samp =  round(mean(ndist),2)
    sd_samp = round(sd(ndist),2)
    
    obs <- data.frame(pdist=c(mean(pdist), var(pdist)), ndist=c(mean(ndist), var(ndist)))
    
    if(input$dist == "rnorm"){
      limx <- c(-3, 3)
    } else {
      if(input$dist == "rexp"){
        limx <- c(0, 5)
      } else {
        if(input$dist == "runif"){
          limx <- c(0, 1)
        } else {
          limx <- c(0, 1)
        }
      }
    }
    
    par(mfrow=c(2,1))  
  
    pdens=density(pop)
    phist=hist(pop, plot=FALSE)
    hist(pop, main=paste(distname, " (mean = ", m_pop, ", SD = ", sd_pop, ")", sep=""), 
         xlab="", freq=FALSE, ylim=c(0, max(pdens$y, phist$density)),
         col="steelblue", xlim = limx)
    lines(pdens, col="darkgreen", lwd=3)
    box()
    
    ndens=density(ndist)
    nhist=hist(ndist, plot=FALSE)
    hist(ndist, main=paste("Distribution of means of ", k, 
                           " random samples, each\nconsisting of ", n, 
                           " observations from a ", distname,
                           "\n(mean = ", m_samp, ", SE = ", sd_samp, ")", sep=""), 
         xlab="Sample means", freq=FALSE, ylim=c(0, max(ndens$y, nhist$density)),
         col="steelblue", xlim = limx)
    lines(ndens, col="darkgreen", lwd=3)
    box()
  
  })
})
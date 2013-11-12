library(shiny)
library(ggplot2)
library(gridExtra)

rbimod <- function(n, mean = c(-2, 2), var = c(1, 1), num.dist = 2){
  if(length(mean) != num.dist) stop("length of mean must equal num.dist")
  if(length(var) != num.dist) stop("length of var must equal num.dist")
  
  if(length(n) > 1) {
    unlist(lapply(1:num.dist, function(xx) rnorm(n[xx], mean = mean[xx], sd = var[xx])))
  } else {
    unlist(lapply(1:num.dist, function(xx) rnorm(n/num.dist, mean = mean[xx], sd = var[xx])))
  }
  
}

shinyServer(function(input, output) {
  
  data <- reactive({
    if(input$goButton > 0 | TRUE) {
      vals <-  do.call(input$dist, list(n=input$n))
    
    return (list(fun=input$dist, vals=vals))
    }
  })
  
  output$plot <- renderPlot({
    input$goButton
    isolate({
    distname <- switch(input$dist,
                       rnorm = "Normal population",
                       rexp = "Right skewed population",
                       runif = "Uniform population",
                       rbimod = "Bimodal population")
    n <- input$n
    k <- input$k
    pdist <- data()$vals

    
    
    x <- replicate(k, do.call(input$dist, list(n=n)))
    pop <- data.frame(pop = do.call(input$dist, list(n=10000)))
  
    m_pop <-  round(mean(pop$pop),2)
    sd_pop <- round(sd(pop$pop),2)
    
    ndist <- data.frame(cbind(means = colMeans(x)))
    
    m_samp <-  round(mean(ndist$means),2)
    sd_samp <- round(sd(ndist$means),2)
    
    obs <- data.frame(pdist=c(mean(pdist), var(pdist)), ndist=c(mean(ndist$means), var(ndist$means)))
    
    if(input$dist == "rnorm"){
      limx <- c(-3, 3)
    } else {
      if(input$dist == "rexp"){
        limx <- c(0, 5)
      } else {
        if(input$dist == "runif"){
          limx <- c(0, 1)
        } else{
          limx <- c(-3, 3)
        }
      }
    }
    
    #par(mfrow=c(2,1))  
  
    pdens<-density(pop$pop)
    phist<-hist(pop$pop, plot=FALSE)
    #hist(pop$pop, main=paste(distname, " (mean = ", m_pop, ", SD = ", sd_pop, ")", sep=""), 
    #     xlab="", freq=FALSE, ylim=c(0, max(pdens$y, phist$density)),
    #     col="steelblue", xlim = limx, cex.lab=1.5, cex.axis=1.5, cex.main=1.5,
    #     cex.sub=1.5)
    #lines(pdens, col="darkgreen", lwd=3)
    #box()
    
    p1 <- ggplot(pop, aes(x = pop)) + theme_bw(base_size = 18)
    p1 <- p1 + geom_histogram(aes(y=..density..),binwidth = .2, fill = "steelblue") +
      geom_density(fill = NA, color = "darkgreen", size = 1.5) +
      labs(title = paste(distname, " (mean = ", m_pop, ", SD = ", sd_pop, ")", sep="")) +
      xlab("") + coord_cartesian(xlim = limx, ylim = c(0,max(pdens$y, phist$density)))
    
    ndens<-density(ndist$means)
    nhist<-hist(ndist$means, plot=FALSE)
    #hist(ndist$means, main=paste("Sampling Distribution with ", k, 
    #                       "\nsamples of size ", n,
    #                       "\n(mean = ", m_samp, ", SE = ", sd_samp, ")", sep=""), 
    #     xlab="Sample means", freq=FALSE, ylim=c(0, max(ndens$y, nhist$density)),
    #     col="steelblue", xlim = limx, cex.lab=1.5, cex.axis=1.5, cex.main=1.5,
    #     cex.sub=1.5)
    #lines(ndens, col="darkgreen", lwd=3)
    #box()
    
    p2 <- ggplot(ndist, aes(x = means)) + theme_bw(base_size = 18)
    p2 <- p2 + geom_histogram(aes(y=..density..), 
                      binwidth = (max(ndist$means)-min(ndist$means))/30,
                               fill = "steelblue") +
      geom_density(fill = NA, color = "darkgreen", size = 1.5) +
      labs(title = paste("Sampling Distribution with ", k, 
                         "\nsamples of size ", n,
                         "\n(mean = ", m_samp, ", SE = ", sd_samp, ")", sep="")) +
      xlab("Sample Means") + coord_cartesian(xlim = limx, 
                              ylim = c(0, max(ndens$y, nhist$density)))
    
    grid.arrange(p1, p2, ncol = 1)
  
    })
  })
})
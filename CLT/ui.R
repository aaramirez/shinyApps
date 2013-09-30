library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Central Limit Theorem"),
  
  sidebarPanel(
    radioButtons("dist", "Parent distribution (population):",
                 list("Normal" = "rnorm",
                      "Right skewed" = "rexp",
                      "Uniform" = "runif")),
        
    sliderInput("n", 
                "Sample size:", 
                value = 50,
                min = 2, 
                max = 1000),
       
    sliderInput("k", 
                "Number of samples:", 
                value = 10,
                min = 10, 
                max = 1000),
    br(), 
    actionButton("goButton", "Go!")),

  mainPanel(
    plotOutput("plot", height="800px")
  )
))
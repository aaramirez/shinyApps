library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Central Limit Theorem"),
  
  sidebarPanel(
    radioButtons("dist", "Parent distribution (population):",
                 list("Normal" = "rnorm",
                      "Right skewed" = "rexp",
                      "Uniform" = "runif",
                      "Bimodal" = "rbimod")),
        
    numericInput("n", 
                "Sample size:", 
                value = 50),
       
    numericInput("k", 
                "Number of samples:", 
                value = 100),
    br(), 
    actionButton("goButton", "Go!")),

  mainPanel(
    plotOutput("plot", height="800px")
  )
))
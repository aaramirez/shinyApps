library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(skin = 'yellow',
        dashboardHeader(
          dashboardSidebar(
            sidebarMenu(
              menuItem('Confidence Interval', tabName = 'ci',
                       icon = icon('bar-chart')),
              menuItem('Sampling Distribution', tabName = 'sampdist',
                       icon = icon('bar-chart')),
              menuItem('Guessing Correlations', tabName = 'guesscorr',
                       icon = icon('line-chart')),
              menuItem('Central Limit Theorem', tabName = 'clt',
                       icon = icon('bar-chart'))
            )
          )
        ),
        dashboardBody(
          tabItems(
            tabItem(tabName = 'ci',
              ci_ui('ci_input')
            ),
            tabItem(tabName = 'sampdist',
              sampdist_ui('sampdist_input')
            ),
            tabItem(tabName = 'guesscorr',
              guesscorr_ui('guesscorr_input')
            ),
            tabItem(tabName = 'clt',
              clt_ui('clt_input')
            )
          )
        )
  
)

server <- function(input, output, session) {
  callModule(ci_module, 'ci_input')
  callModule(sampdist_module, 'sampdist_input')
  callModule(guesscorr_module, 'guesscorr_input')
  callModule(clt_module, 'clt_input')
}

shinyApp(ui, server)

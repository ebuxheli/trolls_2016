# Russian Trolls, App
# 
# By: Enxhi Buxheli 
# Date: December 4th, 2018
# 
# --- Credit to FiveThirtyEight and ClemsonU for the data ---
#   
#   This is a shiny app that presents Russian Troll data from the timeframe relevant 
# for the 2016 US Presidential Election which I processed to be displayed here. This
# app displays plots that help tell some of the interesting stories this data holds.
# 
# mru: Enxhi Buxheli 12/5/2018 
#   + tabs to be filled in

# Attaching libraries
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
                

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
   
   # Application title
   navbarPage("2016 US Presidential Election: Russian Trolls on Twitter",
              
              # Tab with the initial histogram
              tabPanel("Trolls by Day",
                       
                       # Sidebar with a slider input for number of bins 
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput("bins",
                                       "Number of bins:",
                                       min = 1,
                                       max = 50,
                                       value = 30)
                           ),
                         
                           # Show a plot of the generated distribution
                           mainPanel(
                            plotOutput("distPlot")
                         )
                       )
              ),
              # Tab with the initial histogram
              tabPanel("Twitter Stories",
                       
                       # Sidebar with a slider input for number of bins 
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput("bins",
                                       "Number of bins:",
                                       min = 1,
                                       max = 50,
                                       value = 30)
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                           #plotOutput("distPlot")
                         )
                       )
              ),
              
              # Tab with the initial histogram
              tabPanel("Remarks",
                       
                       # Sidebar with a slider input for number of bins 
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput("bins",
                                       "Number of bins:",
                                       min = 1,
                                       max = 50,
                                       value = 30)
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                           #plotOutput("distPlot")
                         )
                       )
              )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(ggplot2)
library(dplyr)

# reading in tax data
taxes <- read_rds("taxes.rds")

ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(inputId = "country", 
                  label = "Country:",
                  choices = taxes$country_name, 
                  selected = "United States")
    ),
    
    # Outputs
    mainPanel(
      plotOutput(outputId = "lineplot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$lineplot <- renderPlot({
    # filtering the country's data shown by user input
    tax_filtered <- taxes %>% filter(country_name == input$country) 
    
    # plotting the user's inputted country
    ggplot(data = tax_filtered, aes(x = year, y = tax_rate)) + 
      geom_smooth() +
      scale_x_continuous(limits = c(1960, 2017)) + 
      labs(title = paste0("Changes in ", input$country, "'s Tax Rate Over Time"),
           subtitle = "Data from the World Bank",
           x = "Year",
           y = "Tax Rate")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


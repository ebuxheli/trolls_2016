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
library(tidyverse)
library(dplyr)
library(fs)
library(stringr)
library(ggplot2)
library(numform)
library(lubridate)
library(RColorBrewer)

# Importing the daily tweet data to be used in the tab "Russian Troll Tweets by Day"
tweets_daily <- read_rds("tweets_daily.rds")


# Function that creates a pretty theme for the histogram I will be displaying
# Credit to Max Woolf: https://minimaxir.com/2015/02/ggplot-tutorial/
# Made some minor modifications to his original function in coloring and text.

custom_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = "#F0F0F0"
  color.grid.major = palette[3]
  color.axis.text  = palette[6]
  color.axis.title = palette[7]
  color.title      = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size = 9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background = element_rect(fill  = color.background, color = color.background)) +
    theme(plot.background  = element_rect(fill  = color.background, color = color.background)) +
    theme(panel.border     = element_rect(color = color.background)) +
    
    # Format the grid
    theme(panel.grid.major = element_line(color = color.grid.major,size = 0.25)) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.ticks = element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position = "none") +
    theme(legend.background = element_rect(fill = color.background)) +
    theme(legend.text = element_text(size = 7, color = color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title   = element_text(color = color.title, size = 14, vjust = 1.25, face = "bold")) +
    theme(plot.subtitle= element_text(color = color.title, size = 12, vjust = 1.25, face = "italic")) +
    theme(axis.text.x  = element_text(size  = 10,color = color.axis.text)) +
    theme(axis.text.y  = element_text(size  = 10,color = color.axis.text)) +
    theme(axis.title.x = element_text(size  = 10,color = color.axis.title, vjust = 0)) +
    theme(axis.title.y = element_text(size  = 10,color = color.axis.title, vjust = 1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
                

# Define UI for the application that will display the histogram for the 
# twitter data.
ui <- fluidPage(theme = shinytheme("flatly"),
   
   # Application title
   navbarPage("2016 US Presidential Election: Russian Trolls on Twitter",
              
              # Tab with the initial histogram
              tabPanel("Trolls by Day",
                       
                       # Sidebar with a checkbox input for the year selection
                       sidebarLayout(
                         sidebarPanel(
                           checkboxGroupInput("year", "Select a Year:",
                                              choices = c("2018" = 2018, 
                                                          "2017" = 2017, 
                                                          "2016" = 2016, 
                                                          "2015" = 2015),
                                              selected = c(2015, 2016, 2017, 2018))
                         ),
                         
                         # Show the daily twitter data and a small description of the data
                         # and structuring of it.
                         mainPanel(
                           plotOutput("daily_hist"),
                           br(),
                           p("I chose the date range for this histogram to be from June 16th, 2015 to 
                              January 20th, 2018. June 16th, 2015 is when Trump first announced his 
                              candidacy for the US 2016 Presidential election and I felt that it'd be 
                              best to show that as the start of the Russian tweets from this point in 
                              time until 1 year after he took the oath for office (on January 20th, 2017).
                              This helps provide a clearer picture of how the Russian Twitter trolls 
                              worked before and after Trump's election.")
                         )
                       )
              ),
              
              # Tab with the twitter stories
              tabPanel("Twitter Stories",
                       
                       # Sidebar with a dropdown input for the story selection 
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "phrase", 
                                       label = "Select a Trump Phrase:",
                                       choices = c("Fake News" = "fake_news",
                                                   "Crooked Hillary" = "hillary",
                                                   "Make America Great Again" = "maga",
                                                   "Failing New York Times" = "nytimes",
                                                   "Trump" = "trump"), 
                                       selected = "Fake News")
                         ),
                         
                         # Show the twitter images
                         mainPanel(
                           imageOutput("plot3")
                         )
                       )
              ),
              
              # Tab with a final description of this project and a link to the
              # new and improved site.
              tabPanel("Russian Trolls Website",
                       
                       # unnecessary sidebar
                       sidebarLayout(
                         sidebarPanel(),
                         
                         # Describes my conclusions on the data in an abbreviated way and directs
                         # viewers to my website (which is better)
                         mainPanel(
                           p("What I've found is that the Tweets put out by accounts associated with 
                             the Russian trolls in the Internet Research Agency are reflective of some 
                             of the events that were going on at the time of their publishing."),
                           br(),
                           p("Please check out my website to find a more in depth analysis of the data.
                             Here you will find a refined data analysis for this project but without
                             the same level of interactivity found here."),
                           br(),
                           a("Link to my website", href = "https://ebuxheli-russiantrolls.netlify.com/")
                         )
                       )
              )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     # Looking at some cool phrases
     tweets_daily %>% 
       filter(str_detect(tolower(content), (input$phrase))) %>% 
       ggplot(aes(day_of)) + 
       geom_histogram(binwidth = 1, color = "#DD7848") +
       labs(title = "Trump Tweets by Day",
            subtitle = "Usage of different phrases coined or made popular by Trump during the 2016 Election",
            x = "Date",
            y = "# of Tweets") +
       custom_theme() +
       scale_y_continuous(label = ff_denom())
   })
   
   # Table displaying number of tweets daily by trolls
   output$daily_hist <- renderPlot({
     
     ## Replicating some of the graphs found in the FiveThirtyEight article.
     ## https://fivethirtyeight.com/features/why-were-sharing-3-million-russian-troll-tweets/
     #
     # I chose the date range for this histogram to be from June 16th, 2015 to January 20th, 2018. 
     # June 16th, 2015 is when Trump first announced his candidacy for the US 2016 Presidential
     # election and I felt that it'd be best to show that as the start of the Russian tweets from
     # this point in time until 1 year after he took the oath for office (on January 20th, 2017).
     # This helps provide a clearer picture of how the Russian Twitter trolls worked before and
     # after Trump's election. 

     tweets_daily %>% 
       filter(year(day_of) %in% input$year) %>% 
       ggplot(aes(day_of)) + 
       geom_histogram(binwidth = 1, color = "#DD7848") +
       labs(title = "Russian Troll Tweets by Day",
            subtitle = "Nearly 3 million tweets sent by trolls associated with the Internet Research Agency",
            x = "Date",
            y = "# of Tweets") +
       custom_theme() +
       scale_y_continuous(label = ff_denom())
   })
   
   # getting twitter images to show
   output$plot3 <- renderImage({
     # When input$n is 1, filename is ./images/image1.jpeg
     filename <- normalizePath(file.path(paste0(input$phrase, '.png')))
     
     # Return a list containing the filename
     list(src = filename)
   }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)


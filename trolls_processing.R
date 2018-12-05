# Russian Trolls, pre-processing and beta
# 
# By: Enxhi Buxheli 
# Date: November 28th, 2018
# 
# --- Credit to FiveThirtyEight and ClemsonU for the data ---
#   
#   This is an R file that prepares Russian Troll data from the timeframe relevant 
# for the 2016 US Presidential Election for analysis in R. The primary objective
# of this project is to create a visualization of the Twitter data that we have
# and see if we can make some sense of why the Russians acted the way they did on
# Twitter. I will look to see if there is any indication of trends amongst the 
# different types of trolls on Twitter created for this Presidential Election.
# 
# mru: Enxhi Buxheli 12/5/2018 
#   + playing with twitter stories


# Attaching libraries
library(tidyverse)
library(dplyr)
library(fs)
library(stringr)
library(ggplot2)
library(numform)
library(lubridate)
library(RColorBrewer)

# THE FOLLOWING IS FOR THE INITIAL FILE DOWNLOAD OF MY RUSSIAN 
# TROLL DATA. ALL OF THE DATA IS PRESENT ON MY LOCAL MACHINE.
# I'VE RUN THESE COMMANDS ONCE AND SAVED THEIR OUTPUT AS
# AN RDS FILE TO SAVE ON TIME WHEN REINITIALIZING MY R
# ENVIRONMENT. YOU WILL NOT BE ABLE TO REPLICATE THESE DATA
# OUTPUT RESULTS BECAUSE OF THE SHEER SIZE OF THE DATA AND RDS
# FILES. I'VE COMMENTED OUT MY PROCESS TO ENSURE THAT THIS
# TIME CONSUMING PROCESS DOESN'T RUN EVERY TIME I MISTAKENLY
# TRY TO RUN ALL OF MY CODE. 
#
# NOTE: Even advanced compression techniques couldn't bring the
# rds file size below 100MB for GitHub uploading.
#
# ### Consolidating troll data 
# # Listing all of the file names for troll data
# files_names <- dir_ls(path = "troll_data/")
# 
# # Reading in all the csv files as listed by their file names
# troll_data <- map_dfr(files_names, read_csv, .id = "source")
# 
# # Making it easier to read in the data rather than running all of the
# # joining prompts again by storing output into an rds file
# write_rds(troll_data, "trolls.rds")
#
#
# Reading in the cleaned up data to avoid crazy run times for the above
# lines of code that process the data for analytical use. 
# UNUSED NOW WITH troll_clean BELOW
# 
# trolls <- read_rds("trolls.rds")
#
# Here I have selected the columns that I thought were relevant for this project.
# I wanted to do a deeper dive into who the authors were, the content of their 
# tweets to see if I could find any interesting trends in some specific phrases
# or words, along with the impact that they have shown by the followers and following
# metrics, publish date to see the timestamp of these tweets and do more a sequential
# analysis. The rest of the variables I kept just in case I wanted to take a look at 
# these variables in the future (I have not used them thus far).
#
# I took a deeper look at variables from trolls to select and analyze and found some 
# to be unreliable. Found that however language was determined seems to be a pretty 
# unreliable source of data. I checked out results from "Albanian" and "LANGUAGE 
# UNDEFINED" to reach this conclusion and found that many of these tweets were in 
# English. Also looked to see if there were any repeat tweets from those associated 
# with the agency (IRA) and found that post_type was also not a reliable metric 
# because it seems that it didn't count many of the quoted and retweeted tweets.
#
# Considering to create an rds file that is hopefully smaller than the trolls.rds
# file to upload to Github for replicability. Otherwise will have a download.file
# statement here for the file linked from the Google Drive.
# 
# troll_clean <- trolls %>%
#   select(author, content, followers, following,
#          account_type, account_category, publish_date, updates,
#          external_author_id, region) %>%
#   mutate(day_of = as.Date(mdy_hm(publish_date), format = "%d"))
#
## TESTING GOOGLE DRIVE FILE DOWNLOAD
# write_rds(troll_clean, "troll_clean.rds")
#
# Link to download file is here, but does not execute properly.
# download.file(url = "http://goo.gl/1DJi4G",
#               destfile = "troll_cleaned1.rds",
#               mode = "wb")
# troll_clean <- read_rds("troll_clean1.rds")


# None of the above storage methods worked. Will continue to check out 
# different options.
troll_clean <- read_rds("troll_clean.rds")


# Plotting the use of the term fake news. First tweeted by Trump in January 2017.
# This is my first example and use of telling a story with the data using phrases.

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

## Plotting some of the Trump administration's top catchphrases as the
## Russian trolls tweeted out during the election process. Need them in
## picture format because the Shiny app can't handle the huge data size
## because of the content of the tweets in troll_clean.
plot_phrase <- function(name, phrase){
  troll_clean %>% 
    filter(str_detect(tolower(content), (paste(phrase)))) %>% 
    ggplot(aes(day_of)) + 
    geom_histogram(binwidth = 1, color = "#DD7848") +
    labs(title = "Trump Tweets by Day",
         subtitle = "Usage of different phrases coined or made popular by Trump during the 2016 Election",
         x = "Date",
         y = "# of Tweets") +
    custom_theme() +
    scale_y_continuous(label = ff_denom())
  
  ggsave(paste0("trolls_2016/", name, ".png"), width = 9, height = 4.5, plot = last_plot(), device = "png", dpi = "screen")
}
# creating output of pictures into the shiny app directory
plot_phrase("fake_news", "fakenews|fake news")
plot_phrase("hillary", "crooked hillary|crookedhillary")
plot_phrase("maga", "maga|make america great again")
plot_phrase("nytimes", "failing new york times|nytimes")
plot_phrase("trump", "trump")


# I chose the date range for this dataset to be from June 16th, 2015 to January 20th, 2018. 
# June 16th, 2015 is when Trump first announced his candidacy for the US 2016 Presidential
# election and I felt that it'd be best to show that as the start of the Russian tweets from
# this point in time until 1 year after he took the oath for office (on January 20th, 2017).
# This helps provide a clearer picture of how the Russian Twitter trolls worked before and
# after Trump's election. 
# 
# Exporting the only data needed for the histogram to be able to export it to GitHub.
tweets_daily <- troll_clean %>% 
  select(day_of) %>% 
  filter(day_of >= as.Date("2015-06-16") & day_of <= as.Date("2018-01-20"))

write_rds(tweets_daily, "trolls_2016/tweets_daily.rds")  


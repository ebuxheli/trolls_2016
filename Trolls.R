# GOV 1005: Russian Trolls
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
# mru: Enxhi Buxheli 12/3/2018 
#   + added comments to data
#   + narrowed down selected variables
#   + plotted daily troll postings


# Attaching libraries
library(tidyverse)
library(dplyr)
library(fs)
library(stringr)
library(ggplot2)
library(lubridate)


# Building a "not in" function to poke around the data.
# You may or may not see it in use in this version of the project
# as I deleted some ad hoc data dives.

`%!in%` = Negate(`%in%`)


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


# Reading in the cleaned up data to avoid crazy run times for the above
# lines of code that process the data for analytical use.
trolls <- read_rds("trolls.rds")


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

troll_clean <- trolls %>% 
  select(author, content, followers, following,
         account_type, account_category, publish_date, updates,  
         external_author_id, region) %>% 
  mutate(publish_date = mdy_hm(publish_date))


# Plotting the number of active Twitter accounts associated with trolls
# over time to observe some of the trends in number of active accounts
# and times when Twitter was removing some of the accounts.

troll_clean %>% 
  count(quarter_date = quarter(publish_date, with_year = TRUE), author) %>% 
  count(quarter_date) %>% 
  ggplot(aes(x = quarter_date, y = nn)) + geom_point() + geom_smooth() + scale_y_log10()

# Plotting the use of the term fake news. First tweeted by TRUMP in January 2017.
troll_clean %>% 
  filter(str_detect(tolower(content), ("fake news|fakenews"))) %>% 
  count(quarter_date = quarter(publish_date, with_year = TRUE)) %>% 
  ggplot(aes(x = quarter_date, y = n)) + geom_point() + geom_smooth() + scale_y_log10() 

## Replicating some of the graphs found in the FiveThirtyEight article.
## https://fivethirtyeight.com/features/why-were-sharing-3-million-russian-troll-tweets/
troll_clean %>%
  count(day_of = as.Date(publish_date, format = "%d")) %>% 
  filter(day_of >= as.Date("2015-01-01") & day_of <= as.Date("2017-12-31")) %>% 
  ggplot(aes(x = day_of, y = n)) + 
    geom_col() 
  
  

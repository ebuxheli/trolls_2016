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
# mru: Enxhi Buxheli 11/29/2018 - initial data dump


# Attaching libraries
library(tidyverse)
library(dplyr)
library(fs)
library(stringr)

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

# Reading in the data
trolls <- read_rds("trolls.rds")

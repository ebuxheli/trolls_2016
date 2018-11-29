# attaching libraries
library(tidyverse)
library(dplyr)
library(fs)
library(stringr)

### consolidating troll data 
# listing all of the file names for troll data
files_names <- dir_ls(path = "troll_data/")

# reading in all the csv files to create 1 data frame
troll_data <- map_dfr(files_names, read_csv, .id = "source")

# making it easier to read in the data rather than running all of the
# joining prompts again by storing output into an rds file
write_rds(troll_data, "trolls.rds")
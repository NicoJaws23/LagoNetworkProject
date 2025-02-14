#Step 1: Getting data into R from GoogleDocs

library(googledrive)
library(googlesheets4)

drive_auth() #login access to Google Drive
gs4_auth(toekn = drive_token()) #apply login credentials
#f <- gs4_find() %>% #pull in the subgroup sheet
  #filter(name == "Aug2014-Dec2015wGPS_SubgroupsFinal-RECOVERED")
#gs4_get(f) #get info for the google sheet
df <- read_sheet("https://docs.google.com/spreadsheets/d/1MPXNVFS7ZSwvdfeuEhEZgGfodv9vFEJntWRW8LiKrrg/edit?gid=1462960726#gid=1462960726") #read in google sheet from drive
names(df)
#Step 2: Filter down the data
library(tidyverse)
library(stringr)
library(dplyr)

dfSelect <- df |>
  select(Date, Time, Activity,Immediate.Spread, Immediate.Subgroup.Composition)

max_elements <- max(str_count(dfSelect$Immediate.Subgroup.Composition, "/")) + 1
print(max_elements)

# Generate dynamic column names: "Focal", "A", "B", ..., "AN"
column_names <- c("Focal", LETTERS)  # First 26 letters
if (max_elements > 27) {
  column_names <- c(column_names, paste0("A", LETTERS[1:(max_elements - 27)]))
}

# Ensure the correct number of column names
column_names <- column_names[1:max_elements]

# Use separate_wider_delim to split the column dynamically
subgroupsSplit <- dfSelect %>%
  separate_wider_delim(Immediate.Subgroup.Composition, delim = "/", names = column_names, too_few = "align_start", too_many = "drop")
write.csv(subgroupsSplit, "C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\subgroupsSplit.csv")
#Making the pivottables

# Load necessary packages
library(dplyr)
library(tidyr)
getwd()
subgroupCSV <- read.csv("subgroupsSplit.csv", header = TRUE)
names(subgroupCSV)

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
  select("dt-time", "Activity", "Immediate.Spread", "Immediate.Subgroup.Composition")


# Generate dynamic column names: "Focal", "Subgroup"
column_names <- c("Focal", "Subgroup")  # First 26 letters

# Use separate_wider_delim to split the column dynamically
subgroupsSplit <- dfSelect %>%
  separate_wider_delim(Immediate.Subgroup.Composition, delim = "/", names = column_names, too_few = "align_start", too_many = "merge")
write.csv(subgroupsSplit, "C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\subgroupsSplit.csv")

names(subgroupsSplit)

# Load necessary packages
library(dplyr)
library(tidyr)
library(stringr)
#Spliting By Group
groupSplit <- function(df, groupID, delim){
  # Step 1: Filter for focal individuals from group D
  fill <- filter(df, str_starts(Focal, groupID))
  # Step 2: Keep only subgroup members whose names start with "D"
  mutate(fill, Subgroup = str_split(Subgroup, delim)) # Convert to list
  rowwise(fill)
  mutate(fill, Subgroup = list(Subgroup[str_starts(Subgroup, groupID)]))
  ungroup(fill)
  # Step 3: Convert list back to string format
  mutate(fill, Subgroup = sapply(Subgroup, function(x) paste(x, collapse = delim)))
  return(fill)
}
groupD <- groupSplit(subgroupsSplit, "D", "/")
groupC <- groupSplit(subgroupsSplit, "C", "/")
groupG <- groupSplit(subgroupsSplit, "G", "/")
groupP <- groupSplit(subgroupsSplit, "P", "/")

#Count number of focals
dFocals <- groupD |>
  group_by(Focal) |>
  summarise(count = n())



#Number of scans
library(dplyr)
library(tidyr)

format_focal_data <- function(df) {
  # Step 1: Expand subgroup into separate rows
  df_long <- df |> 
    separate_rows(Subgroup, sep = "/") |>  
    filter(!is.na(Subgroup) & Subgroup != "")  # Remove empty values
  
  # Step 2: Count the total number of times each focal appears in the dataset
  focal_counts <- df |> 
    count(Focal, name = "NumberOfFocalScans")
  
  # Step 3: Merge with subgroup and keep only relevant columns
  final_df <- df_long |> 
    left_join(focal_counts, by = "Focal") |> 
    select(Focal, Subgroup, NumberOfFocalScans) |> 
    arrange(Focal, Subgroup)  # Sort for better readability
  
  return(final_df)
}

groupDfocals <- format_focal_data(groupD)

#Get scan/cooccurance ratios
together <- function(df) {
  # Step 1: Expand Subgroup column into separate rows
  df_long <- df |>
    separate_rows(Subgroup, sep = "/") |>  # Split subgroup into separate rows
    filter(Subgroup != "")  # Remove empty values from split
  
  # Step 2: Count total scans for each individual (based on unique occurrences in the dataset)
  total_scans <- df_long |>
    select(Focal) |>
    distinct() |>
    count(Focal, name = "Total_Scans")
  
  subgroup_scans <- df_long |>
    select(Subgroup) |>
    rename(Focal = Subgroup) |>
    distinct() |>
    count(Focal, name = "Total_Scans")
  
  # Combine total scans correctly (avoiding double counting)
  total_scans <- bind_rows(total_scans, subgroup_scans) |>
    group_by(Focal) |>
    summarise(Total_Scans = sum(Total_Scans, na.rm = TRUE))  # Sum distinct occurrences
  
  # Step 3: Count co-occurrences (ensuring symmetry)
  co_occurrences <- df_long |>
    count(Focal, Subgroup, name = "Cooccurrence")
  
  co_occurrences_sym <- co_occurrences |>
    rename(Partner = Subgroup) |>
    bind_rows(co_occurrences |> rename(Focal = Subgroup, Partner = Focal)) |>
    group_by(Focal, Partner) |>
    summarise(Cooccurrence = sum(Cooccurrence), .groups = "drop")  # Summing to ensure symmetry
  
  # Step 4: Merge total scans count with co-occurrence count
  final_df <- co_occurrences_sym |>
    left_join(total_scans, by = "Focal") |>
    arrange(Focal, Partner)  # Sort for readability
  
  return(final_df)
}

# print(sri_result)

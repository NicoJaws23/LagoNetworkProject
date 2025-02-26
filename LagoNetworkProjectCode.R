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

max_elements <- max(str_count(dfSelect$Immediate.Subgroup.Composition, "/")) + 1
print(max_elements)

# Generate dynamic column names: "Focal", "Subgroup"
column_names <- c("Focal", "Subgroup")  # First 26 letters

# Use separate_wider_delim to split the column dynamically
subgroupsSplit <- dfSelect %>%
  separate_wider_delim(Immediate.Subgroup.Composition, delim = "/", names = column_names, too_few = "align_start", too_many = "merge")
write.csv(subgroupsSplit, "C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\subgroupsSplit.csv")
#Making the pivottables
names(subgroupsSplit)
# Load necessary packages
library(dplyr)
library(tidyr)
library(stringr)
#Spliting By Group

# Load data (assuming df is your dataframe)
groupD <- subgroupsSplit %>%
  # Step 1: Filter for focal individuals from group D
  filter(str_starts(Focal, "D")) %>%
  # Step 2: Keep only subgroup members whose names start with "D"
  mutate(Subgroup = str_split(Subgroup, "/")) %>% # Convert to list
  rowwise() %>%
  mutate(Subgroup = list(Subgroup[str_starts(Subgroup, "D")])) %>%
  ungroup() %>%
  # Step 3: Convert list back to string format
  mutate(Subgroup = sapply(Subgroup, function(x) paste(x, collapse = "/")))

groupC <- subgroupsSplit %>%
  # Step 1: Filter for focal individuals from group D
  filter(str_starts(Focal, "C")) %>%
  # Step 2: Keep only subgroup members whose names start with "D"
  mutate(Subgroup = str_split(Subgroup, "/")) %>% # Convert to list
  rowwise() %>%
  mutate(Subgroup = list(Subgroup[str_starts(Subgroup, "C")])) %>%
  ungroup() %>%
  # Step 3: Convert list back to string format
  mutate(Subgroup = sapply(Subgroup, function(x) paste(x, collapse = "/")))

groupG <- subgroupsSplit %>%
  # Step 1: Filter for focal individuals from group D
  filter(str_starts(Focal, "G")) %>%
  # Step 2: Keep only subgroup members whose names start with "D"
  mutate(Subgroup = str_split(Subgroup, "/")) %>% # Convert to list
  rowwise() %>%
  mutate(Subgroup = list(Subgroup[str_starts(Subgroup, "G")])) %>%
  ungroup() %>%
  # Step 3: Convert list back to string format
  mutate(Subgroup = sapply(Subgroup, function(x) paste(x, collapse = "/")))

groupP <- subgroupsSplit %>%
  # Step 1: Filter for focal individuals from group D
  filter(str_starts(Focal, "P")) %>%
  # Step 2: Keep only subgroup members whose names start with "D"
  mutate(Subgroup = str_split(Subgroup, "/")) %>% # Convert to list
  rowwise() %>%
  mutate(Subgroup = list(Subgroup[str_starts(Subgroup, "P")])) %>%
  ungroup() %>%
  # Step 3: Convert list back to string format
  mutate(Subgroup = sapply(Subgroup, function(x) paste(x, collapse = "/")))


# Filter out these individuals from both Focal and Individual columns
groupD <- groupD %>%
  filter(!Focal %in% exclude_individuals,    # Remove excluded individuals from the Focal column
         !Individual %in% exclude_individuals)  # Remove excluded individuals from the Individual column

# View the filtered result
print(df_filtered)
NamesFocal <- df_long |>
  group_by(Focal) |>
  summarise(n_case = n())

NamesIndiv <- df_long |>
  group_by(Individual) |>
  summarize(n_cases = n())

#Association MAtrix


# Ensure dataset is symmetrical by duplicating rows with reversed pairs
groupD <- groupD %>%
  rename(Individual1 = Focal, Individual2 = Individual) %>%
  bind_rows(groupD %>% rename(Individual1 = Individual, Individual2 = Focal)) %>%
  group_by(Individual1, Individual2) %>% 
  summarise(Count = sum(Count), .groups = "drop")  # Sum counts to avoid discrepancies

groupD <- groupD %>%
  filter(!is.na(Individual1), Individual1 != "", !is.na(Individual2), Individual2 != "")

# Convert to a wide association matrix
groupD_matrix <- groupD %>%
  pivot_wider(names_from = Individual2, values_from = Count, values_fill = list(Count = 0)) %>%
  column_to_rownames("Individual1")  # Convert first column into row names

# View the matrix
print(association_matrix)

library(pheatmap)

# Create a heatmap of the association matrix
pheatmap(groupD_matrix,
         cluster_rows = TRUE,  # Cluster individuals based on similarity
         cluster_cols = TRUE,
         display_numbers = TRUE,  # Show actual counts
         color = colorRampPalette(c("white", "blue"))(100))  # Gradient from white to blue

library(igraph)

# Convert association matrix to an adjacency matrix
network_graph <- graph_from_adjacency_matrix(as.matrix(groupD_matrix), 
                                             mode = "undirected", 
                                             weighted = TRUE, 
                                             diag = FALSE)

# Plot the network
plot(network_graph,
     vertex.size = 10,  # Node size
     vertex.label.color = "black",
     edge.width = E(network_graph)$weight / max(E(network_graph)$weight) * 10,  # Scale edge width by association strength
     edge.color = "gray50")


#Just group D to mess with code
library(tidyverse)
df <- read_csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Aug2014-Dec2015wGPS_SubgroupsFinal-RECOVERED.csv")
IDs <- read_csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\IDs.csv")
#Calculate the SRI
library(dplyr)
library(tidyr)
names(IDs)
#Group D indivs based on IDs
d <- IDs |>
  filter(Group == "D")

dfSelect <- df |>
  select("dt-time", "Activity", "Immediate.Spread", "Immediate.Subgroup.Composition")

#dfSelect <- dfSelect %>%
  #mutate(Immediate.Subgroup.Composition = gsub("/$", "", Immediate.Subgroup.Composition)) |>
  #separate_rows(Immediate.Subgroup.Composition, sep = "/") |>
  #filter(Immediate.Subgroup.Composition %in% d$ID) #Remove anyone not in group D or with ID

max_elements <- max(str_count(dfSelect$Immediate.Subgroup.Composition, "/"))
print(max_elements)
# Generate dynamic column names: "Focal", "A", "B", ..., "AN"column_names <- c("Focal", LETTERS)  
#First 26 letters
if (max_elements > 27) {  
  column_names <- c(column_names, paste0("A", LETTERS[1:(max_elements - 27)]))
  }
# Ensure the correct number of column names
column_names <- column_names[1:max_elements]
# Use separate_wider_delim to split the column dynamically
subgroupsSplit <- dfSelect %>%  
  separate_wider_delim(Immediate.Subgroup.Composition, delim = "/", names = column_names, too_few = "align_start", too_many = "drop")

# Separate subgroup members into individual rows
df_expanded <- df %>%
  separate_rows(Subgroup, sep = "/") %>%
  filter(Focal != Subgroup) %>% # Remove self-pairings
  filter(Focal %in% d$ID & Subgroup %in% d$ID) #Remove anyone not in group D or with ID

# Generate dyadic counts (X)
df_dyads <- df_expanded %>%
  rename(individual1 = Focal, individual2 = Subgroup) %>%
  bind_rows(df_expanded %>% rename(individual1 = Subgroup, individual2 = Focal)) %>%
  distinct()  # Remove duplicates

co_occurrences <- df_dyads %>%
  count(individual1, individual2, name = "X")

#Calc total number of individual occurances
total_occurrences <- df_expanded %>%
  pivot_longer(cols = c(Focal, Subgroup), names_to = "role", values_to = "individual") %>%
  count(individual, name = "total_obs")

#Calc SRI
dyad_counts <- co_occurrences %>%
  left_join(total_occurrences, by = c("individual1" = "individual")) %>%
  left_join(total_occurrences, by = c("individual2" = "individual"), suffix = c("_1", "_2")) %>%
  mutate(
    Y = total_obs_1 - X,  # Times individual1 was seen without individual2
    Z = total_obs_2 - X,  # Times individual2 was seen without individual1
    SRI = X / (X + Y + Z)  # Compute Simple Ratio Index
  ) %>%
  select(individual1, individual2, X, Y, Z, SRI)

print(dyad_counts)

#Calc eigenvector centrality
library(igraph)
# Create graph from the SRI data
network <- graph_from_data_frame(dyad_counts, directed = FALSE)

# Compute eigenvector centrality
eigen_centrality_scores <- eigen_centrality(network, weights = E(network)$SRI)

# Extract centrality scores
centrality_df <- data.frame(
  individual = names(eigen_centrality_scores$vector),
  eigenvector_centrality = eigen_centrality_scores$vector
)

# Print results
print(centrality_df)

plot(network, vertex.size = eigen_centrality_scores$vector * 50, edge.width = E(network)$SRI * 5)
#THIS WORKS!
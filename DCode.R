#Trying again with group D
library(tidyverse)
library(stringr)
library(igraph)
# Example data
df <- read_csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Aug2014-Dec2015wGPS_SubgroupsFinal-RECOVERED.csv")
IDs <- read_csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\IDs.csv")

d <- IDs |>
  filter(Group == "D")

df <- df |>
  select("dt-time", "Date", "Month", "Year", "Immediate.Subgroup.Composition")

# Remove trailing slashes (just in case)
df <- df %>%
  mutate(Immediate.Subgroup.Composition = gsub("/$", "", Immediate.Subgroup.Composition)) |>
  #mutate(dt-time = as.Date(dt-time), Date = as.Date(Date)) |>
  mutate(`dt-time` = format(mdy_hm(`dt-time`), "%Y-%m-%d %H:%M"), Date = format(mdy(Date), "%Y-%m-%d"))

# Count max subgroup size (i.e., max number of individuals in any row)
max_cols <- max(str_count(df$Immediate.Subgroup.Composition, "/")) + 1  # +1 for the focal

# Create column names dynamically (Focal + as many as needed)
column_names <- c("Focal", paste0("A", seq_len(max_cols - 1)))  # A1, A2, A3, ...

# Split into separate columns dynamically
df_expanded <- df %>%
  separate(Immediate.Subgroup.Composition, into = column_names, sep = "/", fill = "right", extra = "drop") %>%
  mutate(
    Date = as.Date(Date),  # Ensure Date is properly formatted
    timePeriod = case_when(
      (Date >= as.Date("2014-08-06") & Date <= as.Date("2014-12-06")) ~ 1,
      (Date >= as.Date("2014-12-07") & Date <= as.Date("2015-04-07")) ~ 2,
      (Date >= as.Date("2015-04-08") & Date <= as.Date("2015-08-08")) ~ 3,
      (Date >= as.Date("2015-08-09") & Date <= as.Date("2015-12-14")) ~ 4,
      TRUE ~ NA_real_  # Assign NA to dates outside the given ranges
    )
  ) |>
  relocate(timePeriod, .before = 1)  # Moves 'timePeriod' to the first position

  

#monthlyFocals <- df_filtered |>
  #group_by(Month, Year) |>
  #summarise(Count = n())

#Filter for group D individuals only
df_filtered <- df_expanded %>%
  filter(Focal %in% d$ID) %>%
  mutate(across(starts_with("A"), ~ ifelse(. %in% d$ID, ., NA))) 

write_csv(df_filtered, "C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\GroupDFilt.csv")


#Begin of SRI and adjaceny matric
# Sample data (Replace with actual dataset)
library(tidyverse)
library(lubridate)

# Load dataset
df <- read.csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\GroupDFilt.csv", stringsAsFactors = FALSE)

# Convert dt-time to datetime format
df <- df %>%
  mutate(dt.time = mdy_hm(dt.time))  # Use mdy_hm() for "8/6/2014 9:30" format

# Select relevant columns
individuals_df <- df %>% select(dt.time, Focal, starts_with("A"))

# Convert to long format and remove NAs
long_df <- individuals_df %>%
  pivot_longer(cols = -c(dt.time, Focal), values_to = "Subgroup", names_to = "Position") %>%
  filter(!is.na(Subgroup))

library(dplyr)
library(tidyr)
library(purrr)

# Group by dt-time and create dyads only within the same time
dyads <- long_df %>%
  group_by(dt.time) %>%
  summarise(
    DyadList = if (n_distinct(Subgroup) > 1) list(combn(unique(Subgroup), 2, simplify = FALSE)) else list(NULL),
    .groups = "drop"
  ) %>%
  unnest(DyadList) %>%
  filter(!is.null(DyadList)) %>%  # Remove empty dyad lists
  mutate(
    Ind1 = map_chr(DyadList, 1),
    Ind2 = map_chr(DyadList, 2)
  ) %>%
  select(Ind1, Ind2) %>%
  mutate(
    # Ensure symmetry: always store the pair in alphabetical order
    DyadID = pmap_chr(list(Ind1, Ind2), ~ paste(sort(c(..1, ..2)), collapse = "-"))
  ) %>%
  group_by(DyadID) %>%
  summarise(n = n(), .groups = "drop") %>%
  separate(DyadID, into = c("Ind1", "Ind2"), sep = "-")  # Split back into two columns

# View the result
dyads

# Count individual occurrences
individual_counts <- long_df %>%
  group_by(Subgroup) %>%
  summarise(count = n(), .groups = "drop")

# Merge counts for SRI calculation
dyads <- dyads %>%
  left_join(individual_counts, by = c("Ind1" = "Subgroup")) %>%
  rename(CountA = count) %>%
  left_join(individual_counts, by = c("Ind2" = "Subgroup")) %>%
  rename(CountB = count)

# Compute SRI
dyads <- dyads %>%
  mutate(SRI = n / (CountA + CountB - n))

# Create adjacency matrix
individuals <- unique(c(dyads$Ind1, dyads$Ind2))
adj_matrix <- matrix(0, nrow = length(individuals), ncol = length(individuals), dimnames = list(individuals, individuals))

# Populate adjacency matrix
for (i in 1:nrow(dyads)) {
  adj_matrix[dyads$Ind1[i], dyads$Ind2[i]] <- dyads$SRI[i]
  adj_matrix[dyads$Ind2[i], dyads$Ind1[i]] <- dyads$SRI[i]  # Ensure symmetry
}

# Convert to data frame and set diagonals to 0
adj_matrix_df <- as.data.frame(adj_matrix)
diag(adj_matrix_df) <- 0  # No self-interactions

# Save adjacency matrix
write.csv(adj_matrix_df, "C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\GroupD_sri_adjacency_matrix.csv")

# Display matrix
print(adj_matrix_df)

#Calc cetrality
# Assuming 'adj_matrix' is your adjacency matrix
adj_matrix <- as.matrix(adj_matrix_df)

g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
centrality_scores <- eigen_centrality(g, directed = FALSE)$vector
# Assuming you have a vector of individual names or IDs
individuals <- d$ID # Replace with actual names or IDs
names(g)
# Combine centrality scores with individual names
centrality_df <- data.frame(Individual = individuals, Centrality = centrality_scores, row.names = NULL)
min_sri <- min(E(g)$weight)
max_sri <- max(E(g)$weight)
normalized_weights <- (E(g)$weight - min_sri) / (max_sri - min_sri)
edge_colors <- colorRampPalette(c("lightblue", "royalblue", "darkblue"))(100)
edge_color_indices <- round(normalized_weights * 99) + 1
# Map sex to shape - IMPORTANT: use vertex "shape" attribute directly
V(g)$shape <- sapply(V(g)$name, function(name) {
  row <- which(d$ID == name)
  if(d$Sex[row] == "F") {
    "square"
  } else {
    "circle"
  }
})

plot(g,
     # Set edge width based on SRI values
     edge.width = E(g)$weight * 5,
     
     # Set edge color based on SRI values
     edge.color = edge_colors[edge_color_indices],
     edge.curved = 0.2,
     # Node styling
     vertex.size = 30,
     vertex.color = "lightgrey",
     vertex.label = V(g)$name,
     vertex.label.cex = 1,
     
     # Overall layout
     layout = layout_with_fr(g, niter = 1000, area = vcount(g)^3.5, repulserad = vcount(g)^6),
     main = "Social Network Based on SRI Values"
)
write.csv(centrality_df, "C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\GroupD_centrality.csv")

# Get the eigenvector centrality scores
centrality_scores <- eigen_centrality(g, directed = FALSE)$vector

# Create a vector for node sizes (scaled by centrality)
node_size <- centrality_scores * 15  # Adjust the multiplier for desired size

# Create a vector for node colors (scaled by centrality)
node_color <- heat.colors(length(centrality_scores))[rank(centrality_scores)]

# Plot the graph with customized node size and color
plot(g,
     vertex.size = node_size,      # Node size based on centrality
     vertex.color = node_color,    # Node color based on centrality
     vertex.label = NA,            # No labels
     edge.width = 1,               # Edge width
     main = "Network Visualization with Eigenvector Centrality"
)

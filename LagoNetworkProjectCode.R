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

install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan()
install.packages("remotes")
remotes::install_github("JHart96/bisonR")

library(bisonR)
library(dplyr)
sim_data <- simulate_bison_model("binary", aggregated = TRUE)
df <- sim_data$df_sim
priors <- get_default_priors("binary")
fit_edge <- bison_model(
  (event | duration) ~ dyad(node_1_id, node_2_id), 
  data = df, 
  model_type = "binary",
  priors = priors
)
summary(fit_edge)
plot(fit_edge)

install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages("devtools")
library(devtools)
#remotes::install_github("JHart96/bisonR")



#check cmdstan

#install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
install_cmdstan()
set_cmdstan_path(path = NULL)
set_cmdstan_path(path = "C:/Users/Jawor/.cmdstan/cmdstan-2.36.0")

library(bisonR)
library(dplyr)
library(brms)
library(Rcpp)
library(cmdstanr)
library(tidyr)
library(readxl)
library(igraph)
library(ggplot2)

sim_data <- simulate_bison_model("binary", aggregated = TRUE)
df <- sim_data$df_sim
priors <- get_default_priors("binary")
fit_edge <- bison_model(
  (event | duration) ~ dyad(node_1_id, node_2_id), 
  data = df, 
  model_type = "binary",
  priors = priors
)
summary(fit_edge)
plot(fit_edge)

library(ggplot2)
library(tidyr)

df_effects <- as.data.frame(fit_edge$edge_samples)
colnames(df_effects) <- fit_edge$dyad_names  # Assign dyad names to columns

# Compute summary statistics (median, 5%, 95%)
df_summary <- data.frame(
  Dyad = colnames(df_effects),
  Median = apply(df_effects, 2, median),
  Lower = apply(df_effects, 2, quantile, 0.05),
  Upper = apply(df_effects, 2, quantile, 0.95)
)

print(df_summary)

library(ggplot2)
library(tidyr)
library(dplyr)

df_plot <- df_summary %>%
  separate(Dyad, into = c("node_1", "node_2"), sep = " <-> ", convert = TRUE)

ggplot(df_plot, aes(x = as.factor(node_1), y = as.factor(node_2), fill = Median)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0.5) +
  theme_minimal() +
  labs(title = "Estimated Dyadic Interaction Probabilities",
       x = "Node 1",
       y = "Node 2",
       fill = "Probability")

library(igraph)
library(ggraph)

# Convert summary data to edge list format
edges <- df_summary %>%
  separate(Dyad, into = c("from", "to"), sep = " <-> ", convert = TRUE) 

# Create graph object
g <- graph_from_data_frame(edges, directed = FALSE)

# Plot network
ggraph(g, layout = "nicely") +
  geom_edge_link(aes(edge_alpha = Median), show.legend = TRUE) +
  geom_node_point(size = 5, color = "darkred") +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  labs(title = "Social Network with Edge Strengths")


#Messing with group D data
# Load your adjacency matrix from CSV file
adjacency_matrix <- read.csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\GroupD_sri_adjacency_matrix.csv", row.names = 1)
head(adjacency_matrix)

adjacency_matrix <- as.matrix(adjacency_matrix)  # Ensure it's a matrix

# Convert the adjacency matrix to long format (with row and column names)
adj_long <- as.data.frame(as.table(adjacency_matrix))

# Rename columns for clarity
colnames(adj_long) <- c("Focal", "Subgroup", "Interaction")

# Filter for non-zero interactions
adj_long <- adj_long %>%
  filter(Interaction > 0)

# View the resulting long format data
head(adj_long)

# Get the default priors for the model
priors1 <- c(
  set_prior("normal(0, 1)", class = "b"),   # Adjust this prior as needed
  set_prior("normal(0, 1)", class = "Intercept")
)
print(priors)

write.csv(adj_long, "C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\GroupD_adjLong.csv")

str(adj_long)  # Check the structure of the data
adj_long$Focal <- as.factor(adj_long$Focal)
adj_long$Subgroup <- as.factor(adj_long$Subgroup)
adj_long$Interaction <- as.numeric(adj_long$Interaction)  # Ensure Interaction is numeric
colnames(adj_long) <- gsub(" ", "_", colnames(adj_long))  # Replace spaces with underscores

fit_edge <- bison_model(
  Interaction ~ Focal + Subgroup,  # A simple model with just the variables
  data = adj_long, 
  model_type = "binary", 
  priors = priors1
)
install.packages("bisonR")  # Reinstall the package


#G Code
#Group D Code pt 2
library(tidyverse)
library(purrr)
library(igraph)
library(rptR)
library(lme4)

df <- read_csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Aug2014-Dec2015wGPS_SubgroupsFinal-RECOVERED.csv")
IDs <- read_csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\IDs.csv")

d <- IDs |>
  filter(Group == "G")

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

#Filter for group D individuals only
df_filtered <- df_expanded %>%
  filter(Focal %in% d$ID) %>%
  mutate(across(starts_with("A"), ~ ifelse(. %in% d$ID, ., NA))) 

# Convert wide format to long format
df_long <- df_filtered %>%
  pivot_longer(cols = starts_with("A"), values_to = "Individual", names_to = "ID") %>%
  select(-ID) %>%  # Remove unnecessary column
  filter(!is.na(Individual) & Individual != "NA")

# Group by Date-Time to generate all possible individual pairs per observation
df_pairs <- df_long %>%
  group_by(`dt-time`) %>%
  summarise(Pairs = list(combn(unique(c(Focal, Individual)), 2, simplify = FALSE))) %>%
  unnest(Pairs) %>%
  mutate(Pair = map_chr(Pairs, ~ paste(sort(.x), collapse = "-"))) %>%
  count(Pair, name = "X")  # Count how many times each dyad appears

# Extract individual names
df_pairs <- df_pairs %>%
  separate(Pair, into = c("Ind_A", "Ind_B"), sep = "-")

# Count total occurrences of each individual
individual_counts <- df_long %>%
  select(Focal, Individual) %>%
  pivot_longer(cols = everything(), values_to = "Individual") %>%
  filter(!is.na(Individual) & Individual != "NA") %>%
  count(Individual, name = "Total_Seen")

# Merge counts for proper SRI calculation
pair_data <- df_pairs %>%
  left_join(individual_counts, by = c("Ind_A" = "Individual")) %>%
  rename(Total_A = Total_Seen) %>%
  left_join(individual_counts, by = c("Ind_B" = "Individual")) %>%
  rename(Total_B = Total_Seen) %>%
  mutate(
    Y = Total_A - X,
    Z = Total_B - X,
    SRI = X / (X + Y + Z)
  ) %>%
  replace_na(list(SRI = 0)) %>%  # Replace NA values with 0
  select(Ind_A, Ind_B, SRI)

print(pair_data)

# Duplicate the data to ensure symmetry
pair_data_symmetric <- pair_data %>%
  bind_rows(pair_data %>% rename(Ind_A = Ind_B, Ind_B = Ind_A)) %>%
  group_by(Ind_A, Ind_B) %>%
  summarize(SRI = mean(SRI, na.rm = TRUE), .groups = "drop")  # Ensure unique pairs

# Pivot to create adjacency matrix
adj_matrix <- pair_data_symmetric %>%
  pivot_wider(names_from = Ind_B, values_from = SRI, values_fill = list(SRI = 0)) %>%
  column_to_rownames("Ind_A") %>%
  as.matrix()

# Ensure it's properly ordered
individuals <- sort(unique(c(pair_data_symmetric$Ind_A, pair_data_symmetric$Ind_B)))
adj_matrix <- adj_matrix[individuals, individuals]

# Remove self-associations
diag(adj_matrix) <- 0

print(adj_matrix)



# Create graph from adjacency matrix
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)

# Calculate Centrality Measures
eigen_centrality <- eigen_centrality(g)$vector

# Store results
centrality_results <- data.frame(
  Individual = names(eigen_centrality),
  Eigenvector = eigen_centrality
)

# Print centrality results
print(centrality_results)

# Plot the social network
plot(g, vertex.size = 10, vertex.label = V(g)$name, edge.width = E(g)$weight * 5)


# Merge centrality results with individual info
centrality_results <- centrality_results %>%
  left_join(d, by = join_by("Individual" == "ID"))
write.csv(centrality_results, "C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Exports\\groupG_allTimeCent.csv")
#Splitting data by time period
d1 <- df_filtered |>
  filter(timePeriod == 1)
d2 <- df_filtered |>
  filter(timePeriod == 2)
d3 <- df_filtered |>
  filter(timePeriod == 3)
d4 <- df_filtered |>
  filter(timePeriod == 4)

#Redoing analysis by time period

#d1
# Convert wide format to long format
d1_long <- d1 %>%
  pivot_longer(cols = starts_with("A"), values_to = "Individual", names_to = "ID") %>%
  select(-ID) %>%  # Remove unnecessary column
  filter(!is.na(Individual) & Individual != "NA")

# Group by Date-Time to generate all possible individual pairs per observation
d1_pairs <- d1_long %>%
  group_by(`dt-time`) %>%
  summarise(Pairs = list(combn(unique(c(Focal, Individual)), 2, simplify = FALSE))) %>%
  unnest(Pairs) %>%
  mutate(Pair = map_chr(Pairs, ~ paste(sort(.x), collapse = "-"))) %>%
  count(Pair, name = "X")  # Count how many times each dyad appears

# Extract individual names
d1_pairs <- d1_pairs %>%
  separate(Pair, into = c("Ind_A", "Ind_B"), sep = "-")

# Count total occurrences of each individual
d1individual_counts <- d1_long %>%
  select(Focal, Individual) %>%
  pivot_longer(cols = everything(), values_to = "Individual") %>%
  filter(!is.na(Individual) & Individual != "NA") %>%
  count(Individual, name = "Total_Seen")

# Merge counts for proper SRI calculation
d1pair_data <- d1_pairs %>%
  left_join(d1individual_counts, by = c("Ind_A" = "Individual")) %>%
  rename(Total_A = Total_Seen) %>%
  left_join(d1individual_counts, by = c("Ind_B" = "Individual")) %>%
  rename(Total_B = Total_Seen) %>%
  mutate(
    Y = Total_A - X,
    Z = Total_B - X,
    SRI = X / (X + Y + Z)
  ) %>%
  replace_na(list(SRI = 0)) %>%  # Replace NA values with 0
  select(Ind_A, Ind_B, SRI)

print(d1pair_data)

# Duplicate the data to ensure symmetry
pair_data_symmetric <- d1pair_data %>%
  bind_rows(d1pair_data %>% rename(Ind_A = Ind_B, Ind_B = Ind_A)) %>%
  group_by(Ind_A, Ind_B) %>%
  summarize(SRI = mean(SRI, na.rm = TRUE), .groups = "drop")  # Ensure unique pairs

# Pivot to create adjacency matrix
d1adj_matrix <- pair_data_symmetric %>%
  pivot_wider(names_from = Ind_B, values_from = SRI, values_fill = list(SRI = 0)) %>%
  column_to_rownames("Ind_A") %>%
  as.matrix()

# Ensure it's properly ordered
individuals <- sort(unique(c(pair_data_symmetric$Ind_A, pair_data_symmetric$Ind_B)))
d1adj_matrix <- d1adj_matrix[individuals, individuals]

# Remove self-associations
diag(d1adj_matrix) <- 0

print(d1adj_matrix)

# Create graph from adjacency matrix
d1g <- graph_from_adjacency_matrix(d1adj_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)

# Calculate Centrality Measures
d1eigen_centrality <- eigen_centrality(d1g)$vector

# Store results
d1centrality_results <- data.frame(
  Individual = names(d1eigen_centrality),
  Eigenvector = d1eigen_centrality,
  timePeriod = 1
)
# Print centrality results
print(d1centrality_results)

# Plot the social network
plot(d1g, vertex.size = 10, vertex.label = V(d1g)$name, edge.width = E(d1g)$weight * 5)


# Merge centrality results with individual info
d1centrality_results <- d1centrality_results %>%
  left_join(d, by = join_by("Individual" == "ID"))
write.csv(d1centrality_results, "C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Exports\\groupG_p1Cent.csv")

#######################################################################################
#d2
# Convert wide format to long format
d2_long <- d2 %>%
  pivot_longer(cols = starts_with("A"), values_to = "Individual", names_to = "ID") %>%
  select(-ID) %>%  # Remove unnecessary column
  filter(!is.na(Individual) & Individual != "NA")

# Group by Date-Time to generate all possible individual pairs per observation
d2_pairs <- d2_long %>%
  group_by(`dt-time`) %>%
  summarise(Pairs = list(combn(unique(c(Focal, Individual)), 2, simplify = FALSE))) %>%
  unnest(Pairs) %>%
  mutate(Pair = map_chr(Pairs, ~ paste(sort(.x), collapse = "-"))) %>%
  count(Pair, name = "X")  # Count how many times each dyad appears

# Extract individual names
d2_pairs <- d2_pairs %>%
  separate(Pair, into = c("Ind_A", "Ind_B"), sep = "-")

# Count total occurrences of each individual
d2individual_counts <- d2_long %>%
  select(Focal, Individual) %>%
  pivot_longer(cols = everything(), values_to = "Individual") %>%
  filter(!is.na(Individual) & Individual != "NA") %>%
  count(Individual, name = "Total_Seen")

# Merge counts for proper SRI calculation
d2pair_data <- d2_pairs %>%
  left_join(d2individual_counts, by = c("Ind_A" = "Individual")) %>%
  rename(Total_A = Total_Seen) %>%
  left_join(d2individual_counts, by = c("Ind_B" = "Individual")) %>%
  rename(Total_B = Total_Seen) %>%
  mutate(
    Y = Total_A - X,
    Z = Total_B - X,
    SRI = X / (X + Y + Z)
  ) %>%
  replace_na(list(SRI = 0)) %>%  # Replace NA values with 0
  select(Ind_A, Ind_B, SRI)

print(d2pair_data)

# Duplicate the data to ensure symmetry
pair_data_symmetric <- d2pair_data %>%
  bind_rows(d2pair_data %>% rename(Ind_A = Ind_B, Ind_B = Ind_A)) %>%
  group_by(Ind_A, Ind_B) %>%
  summarize(SRI = mean(SRI, na.rm = TRUE), .groups = "drop")  # Ensure unique pairs

# Pivot to create adjacency matrix
d2adj_matrix <- pair_data_symmetric %>%
  pivot_wider(names_from = Ind_B, values_from = SRI, values_fill = list(SRI = 0)) %>%
  column_to_rownames("Ind_A") %>%
  as.matrix()

# Ensure it's properly ordered
individuals <- sort(unique(c(pair_data_symmetric$Ind_A, pair_data_symmetric$Ind_B)))
d2adj_matrix <- d2adj_matrix[individuals, individuals]

# Remove self-associations
diag(d2adj_matrix) <- 0

print(d2adj_matrix)

# Create graph from adjacency matrix
d2g <- graph_from_adjacency_matrix(d2adj_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)

# Calculate Centrality Measures
d2eigen_centrality <- eigen_centrality(d2g)$vector

# Store results
d2centrality_results <- data.frame(
  Individual = names(d2eigen_centrality),
  Eigenvector = d2eigen_centrality,
  timePeriod = 2
)

# Print centrality results
print(d2centrality_results)

# Plot the social network
plot(d2g, vertex.size = 10, vertex.label = V(g)$name, edge.width = E(g)$weight * 5)


# Merge centrality results with individual info
d2centrality_results <- d2centrality_results %>%
  left_join(d, by = join_by("Individual" == "ID"))
write.csv(d2centrality_results, "C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Exports\\groupG_p2Cent.csv")

####################################################################################################
#d3
# Convert wide format to long format
d3_long <- d3 %>%
  pivot_longer(cols = starts_with("A"), values_to = "Individual", names_to = "ID") %>%
  select(-ID) %>%  # Remove unnecessary column
  filter(!is.na(Individual) & Individual != "NA")

# Group by Date-Time to generate all possible individual pairs per observation
d3_pairs <- d3_long %>%
  group_by(`dt-time`) %>%
  summarise(Pairs = list(combn(unique(c(Focal, Individual)), 2, simplify = FALSE))) %>%
  unnest(Pairs) %>%
  mutate(Pair = map_chr(Pairs, ~ paste(sort(.x), collapse = "-"))) %>%
  count(Pair, name = "X")  # Count how many times each dyad appears

# Extract individual names
d3_pairs <- d3_pairs %>%
  separate(Pair, into = c("Ind_A", "Ind_B"), sep = "-")

# Count total occurrences of each individual
d3individual_counts <- d3_long %>%
  select(Focal, Individual) %>%
  pivot_longer(cols = everything(), values_to = "Individual") %>%
  filter(!is.na(Individual) & Individual != "NA") %>%
  count(Individual, name = "Total_Seen")

# Merge counts for proper SRI calculation
d3pair_data <- d3_pairs %>%
  left_join(d3individual_counts, by = c("Ind_A" = "Individual")) %>%
  rename(Total_A = Total_Seen) %>%
  left_join(d3individual_counts, by = c("Ind_B" = "Individual")) %>%
  rename(Total_B = Total_Seen) %>%
  mutate(
    Y = Total_A - X,
    Z = Total_B - X,
    SRI = X / (X + Y + Z)
  ) %>%
  replace_na(list(SRI = 0)) %>%  # Replace NA values with 0
  select(Ind_A, Ind_B, SRI)

print(d3pair_data)

# Duplicate the data to ensure symmetry
pair_data_symmetric <- d3pair_data %>%
  bind_rows(d3pair_data %>% rename(Ind_A = Ind_B, Ind_B = Ind_A)) %>%
  group_by(Ind_A, Ind_B) %>%
  summarize(SRI = mean(SRI, na.rm = TRUE), .groups = "drop")  # Ensure unique pairs

# Pivot to create adjacency matrix
d3adj_matrix <- pair_data_symmetric %>%
  pivot_wider(names_from = Ind_B, values_from = SRI, values_fill = list(SRI = 0)) %>%
  column_to_rownames("Ind_A") %>%
  as.matrix()

# Ensure it's properly ordered
individuals <- sort(unique(c(pair_data_symmetric$Ind_A, pair_data_symmetric$Ind_B)))
d3adj_matrix <- d3adj_matrix[individuals, individuals]

# Remove self-associations
diag(d3adj_matrix) <- 0

print(d3adj_matrix)

# Create graph from adjacency matrix
d3g <- graph_from_adjacency_matrix(d3adj_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)

# Calculate Centrality Measures
d3eigen_centrality <- eigen_centrality(d3g)$vector

# Store results
d3centrality_results <- data.frame(
  Individual = names(d3eigen_centrality),
  Eigenvector = d3eigen_centrality,
  timePeriod = 3
)

# Print centrality results
print(d3centrality_results)

# Plot the social network
plot(d3g, vertex.size = 10, vertex.label = V(g)$name, edge.width = E(g)$weight * 5)


# Merge centrality results with individual info
d3centrality_results <- d3centrality_results %>%
  left_join(d, by = join_by("Individual" == "ID"))
write.csv(d3centrality_results, "C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Exports\\groupG_p3Cent.csv")

#########################################################################################
#d4
# Convert wide format to long format
d4_long <- d4 %>%
  pivot_longer(cols = starts_with("A"), values_to = "Individual", names_to = "ID") %>%
  select(-ID) %>%  # Remove unnecessary column
  filter(!is.na(Individual) & Individual != "NA")

# Group by Date-Time to generate all possible individual pairs per observation
d4_pairs <- d4_long %>%
  group_by(`dt-time`) %>%
  summarise(Pairs = list(combn(unique(c(Focal, Individual)), 2, simplify = FALSE))) %>%
  unnest(Pairs) %>%
  mutate(Pair = map_chr(Pairs, ~ paste(sort(.x), collapse = "-"))) %>%
  count(Pair, name = "X")  # Count how many times each dyad appears

# Extract individual names
d4_pairs <- d4_pairs %>%
  separate(Pair, into = c("Ind_A", "Ind_B"), sep = "-")

# Count total occurrences of each individual
d4individual_counts <- d4_long %>%
  select(Focal, Individual) %>%
  pivot_longer(cols = everything(), values_to = "Individual") %>%
  filter(!is.na(Individual) & Individual != "NA") %>%
  count(Individual, name = "Total_Seen")

# Merge counts for proper SRI calculation
d4pair_data <- d4_pairs %>%
  left_join(d4individual_counts, by = c("Ind_A" = "Individual")) %>%
  rename(Total_A = Total_Seen) %>%
  left_join(d4individual_counts, by = c("Ind_B" = "Individual")) %>%
  rename(Total_B = Total_Seen) %>%
  mutate(
    Y = Total_A - X,
    Z = Total_B - X,
    SRI = X / (X + Y + Z)
  ) %>%
  replace_na(list(SRI = 0)) %>%  # Replace NA values with 0
  select(Ind_A, Ind_B, SRI)

print(d4pair_data)

# Duplicate the data to ensure symmetry
pair_data_symmetric <- d4pair_data %>%
  bind_rows(d4pair_data %>% rename(Ind_A = Ind_B, Ind_B = Ind_A)) %>%
  group_by(Ind_A, Ind_B) %>%
  summarize(SRI = mean(SRI, na.rm = TRUE), .groups = "drop")  # Ensure unique pairs

# Pivot to create adjacency matrix
d4adj_matrix <- pair_data_symmetric %>%
  pivot_wider(names_from = Ind_B, values_from = SRI, values_fill = list(SRI = 0)) %>%
  column_to_rownames("Ind_A") %>%
  as.matrix()

# Ensure it's properly ordered
individuals <- sort(unique(c(pair_data_symmetric$Ind_A, pair_data_symmetric$Ind_B)))
d4adj_matrix <- d4adj_matrix[individuals, individuals]

# Remove self-associations
diag(d4adj_matrix) <- 0

print(d4adj_matrix)

# Create graph from adjacency matrix
d4g <- graph_from_adjacency_matrix(d4adj_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)

# Calculate Centrality Measures
d4eigen_centrality <- eigen_centrality(d4g)$vector

# Store results
d4centrality_results <- data.frame(
  Individual = names(d4eigen_centrality),
  Eigenvector = d4eigen_centrality,
  timePeriod = 4
)

# Print centrality results
print(d4centrality_results)

# Plot the social network
plot(d4g, vertex.size = 10, vertex.label = V(g)$name, edge.width = E(g)$weight * 5)


# Merge centrality results with individual info
d4centrality_results <- d4centrality_results %>%
  left_join(d, by = join_by("Individual" == "ID"))
write.csv(d4centrality_results, "C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Exports\\groupG_p4Cent.csv")


#Binding rows
allTime <- bind_rows(list(d1centrality_results, d2centrality_results, d3centrality_results))
allTime <- allTime |>
  mutate(age.sex = factor(paste(Sex, ADULT, sep = "_")), Individual = factor(Individual), timePeriod= as.factor(timePeriod))
write.csv(allTime, "C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Exports\\groupG_CombinedCent.csv")

#rprR

# Run repeatability analysis on centrality measures
rpt_eigen <- rpt(Eigenvector ~ (1|timePeriod) + (1|Individual), grname = "Individual", data = allTime, datatype = "Gaussian")

summary(rpt_eigen)
results <- data.frame(
  Trait = names(rpt_eigen$R),
  Repeatability = rpt_eigen$R$Individual,
  CI_lower = rpt_eigen$CI_emp$`2.5%`,
  CI_upper = rpt_eigen$CI_emp$`97.5%`,
  P_value = rpt_eigen$P$LRT_P
)
write.csv(results, "C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Exports\\groupG_rpt_results.csv", row.names = FALSE)
#Testing predictors of centrality
egGLM <- lmer(Eigenvector ~ age.sex + (1|Individual), data = allTime)
egGLM
plot(egGLM)
summary(egGLM)

anova(egGLM, lmer(Eigenvector ~ (1 | Individual), data = allTime))


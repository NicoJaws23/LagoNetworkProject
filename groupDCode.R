#Just group D to mess with code
library(tidyverse)
df <- read_csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\groupD.csv")

# Count how many times each individual was seen
individuals <- df %>%
  mutate(individuals = str_split(Subgroup, "/")) %>%
  unnest(individuals) %>%
  select('dt-time', Focal, individuals) %>%
  distinct() %>%
  group_by(individuals) %>%
  summarise(count = n())

print(individuals)

# Count dyads, ensuring subgroups have at least 2 individuals
dyads <- df %>%
  mutate(individuals = str_split(Subgroup, "/")) %>%
  # Filter subgroups with at least 2 individuals
  filter(sapply(individuals, length) >= 2) %>%
  # Generate pairs only for valid subgroups
  mutate(pair = map(individuals, ~ if (length(.x) >= 2) combn(.x, 2, simplify = FALSE) else NULL)) %>%
  # Unnest the pairs (only valid pairs will be kept)
  unnest(pair) %>%
  # Count how many times each dyad occurs
  group_by(pair) %>%
  summarise(dyad_count = n(), .groups = "drop") %>%
  # Optionally, format the pair column
  mutate(pair = map(pair, ~ paste(.x, collapse = "/")))

# View the resulting dyads and counts
View(dyads)

#SRI
library(tidyverse)

# Calculate total observations for each individual
total_obs <- df %>%
  mutate(individuals = str_split(Subgroup, "/")) %>%
  unnest(individuals) %>%
  distinct('dt-time', individuals) %>%
  group_by(individuals) %>%
  summarise(total_obs = n(), .groups = "drop")

# Generate dyads and calculate SRI for each dyad
dyads_sri <- dyads %>%
  separate(pair, into = c("individual1", "individual2"), sep = " /") %>%
  filter(!is.na(individual1) & !is.na(individual2)) %>%  # Ensure we only keep valid dyads
  left_join(total_obs, by = c("individual1" = "individuals")) %>%
  left_join(total_obs, by = c("individual2" = "individuals"), suffix = c(".1", ".2")) %>%
  mutate(SRI = dyad_count / (total_obs.1 + total_obs.2 - dyad_count))

# Print the result
print(dyads_sri)

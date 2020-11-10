
# Loaidng in necessary libraries

library(rjson)
library(tidyverse)

# Pulling in the scraped data

data <- fromJSON(file = "https://raw.githubusercontent.com/alex/nyt-2020-election-scraper/master/results.json")

# Getting the data for each state from the scraped data

states <- as_tibble(as_tibble(data)$data[1])

# Pulling votes for each state of the two top candidates

votes <- states %>% 
  mutate(state = map_chr(races, ~.x[["state_name"]]),
         party_1 = map_chr(races, ~.x[["candidates"]][[1]][["party_id"]]),
         votes_1 = map_dbl(races, ~.x[["candidates"]][[1]][["votes"]]),
         party_2 = map_chr(races, ~.x[["candidates"]][[2]][["party_id"]]),
         votes_2 = map_dbl(races, ~.x[["candidates"]][[2]][["votes"]]),
         total = votes_1 + votes_2) %>% 
  select(-races)

# Splitting the party for organization

first <- votes %>% 
  select(state, party_1, votes_1) %>% 
  rename(party = party_1,
         votes = votes_1)

second <- votes %>% 
  select(state, party_2, votes_2) %>% 
  rename(party = party_2,
         votes = votes_2)

# Matching the parties and then pivoting to long data

votes_clean <- rbind(first, second) %>% 
  pivot_wider(names_from = "party", values_from = "votes") %>% 
  mutate(total = republican + democrat,
         republican = republican / total * 100,
         democrat = democrat / total * 100) %>% 
  select(-total) %>% 
  pivot_longer(republican:democrat, names_to = "party", values_to = "actual_pv2p")

# Comparing my prediction with the scraped results

pred <- read_csv("2020_pv2p_pred.csv")

pred_compare <- votes_clean %>% 
  inner_join(pred, by = c("state", "party")) %>% 
  rename(pred_pv2p = pred_votes) %>% 
  mutate(diff = actual_pv2p - pred_pv2p) %>% 
  filter(party == "democrat")

pred_compare

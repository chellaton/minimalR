library(tidyverse)

# Read taxonomy data
taxonomy_data <- read_tsv('baxter.cons.taxonomy') %>%
  select(-Size) %>%
  rename_all(tolower) %>% 
  mutate(otu = tolower(otu),
         taxonomy = str_replace_all(taxonomy, "\\(\\d+\\)", ""),
         taxonomy = str_replace(taxonomy, ";$", ""),
         taxonomy = str_replace(taxonomy, ".*;", "")) 

# Read file subsample.shared
shared <- read_tsv('baxter.subsample.shared', 
                   col_types = cols(Group = col_character(), 
                                    .default=col_double())) %>% 
  rename_all(tolower) %>% 
  select(group, starts_with('otu')) %>%
  pivot_longer(-group, names_to = 'otu', values_to = 'count') 

# Read metadata file
metadata <- read_tsv('baxter.metadata.tsv') %>%
  rename_all(tolower)

res <- inner_join(shared, taxonomy_data, by='otu') %>%
  group_by(group, taxonomy) %>%
  summarize(count = sum(count)) %>%
  group_by(group) %>%
  mutate(rel_abund = count/sum(count))
{
  library(microbiome)
  source("./code/16S/load_data.R")
  # source("./code/16S/load_breeder_data.R")
}


# Calculate alpha diversity stats
alpha_stats <- table %>% 
  pivot_longer(-featureid, 
               names_to = "sampleid",
               values_to = "count") %>% 
  group_by(sampleid) %>% 
  summarize(
    chao1 = as.double(richness(count, index = "chao1")),
    obs_richness = as.double(richness(count, index = "observed")),
    shannon = vegan::diversity(count, index = "shannon", base = ),
    simpson = vegan::diversity(count, index = "simpson")
  ) %>% 
  left_join(., metadata)

alpha_stats_breeders <- table_breeder %>% 
  pivot_longer(-featureid, 
               names_to = "sampleid",
               values_to = "count") %>% 
  group_by(sampleid) %>% 
  summarize(
    chao1 = as.double(richness(count, index = "chao1")),
    obs_richness = as.double(richness(count, index = "observed")),
    shannon = vegan::diversity(count, index = "shannon", base = ),
    simpson = vegan::diversity(count, index = "simpson")
  ) %>% 
  left_join(., metadata_breeder)

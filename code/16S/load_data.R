{
  library(tidyverse)
  library(readxl)
  library(janitor)
  theme_set(ggprism::theme_prism())
}

# metadata  <- read_tsv('./data/16S/231011_CTL_CF/data/metadata.txt') 
metadata <- read_excel('data/16S/231011_CTL_CF/data/230918_submission.xlsx',
                       skip = 5) %>% 
  clean_names()


table_orig <- read_tsv("data/16S/231011_CTL_CF/data/core-metrics-results_28850/rarefied-feature-table_taxa.tsv",
                       skip = 1) %>% 
  rename(featureid = `#OTU ID`,
         taxon = "Taxon") %>% 
  dplyr::select(featureid, taxon, any_of(metadata$sampleid))

# Pull taxonomy and separate into levels
taxonomy <- table_orig %>% 
  dplyr::select(featureid, taxon) %>% 
  separate(taxon, 
           into = c("Kingdom", "Phylum", "Class", 
                    "Order", "Family", "Genus"),
           sep = "; ") %>% 
  pivot_longer(-featureid, 
               names_to = "level", 
               values_to = "taxon") %>% 
  mutate(taxon = str_sub(taxon, 4, length(taxon))) %>% 
  group_by(featureid) %>% 
  fill(taxon)

# pull only table 
table <- table_orig %>% 
  dplyr::select(-taxon)

filter_table <- function(level){
  table %>% 
    pivot_longer(-featureid, names_to = 'sampleid', values_to = 'count') %>% 
    inner_join(., taxonomy, by= 'featureid') %>% 
    filter(level == {{level}}) %>% 
    group_by(sampleid, taxon) %>% 
    summarise(count = sum(count)) %>% 
    pivot_wider(names_from = 'sampleid', values_from = 'count')
}



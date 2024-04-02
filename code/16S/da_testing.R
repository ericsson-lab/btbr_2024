{
  source('code/16S/load_data.R')
  library(phyloseq)
  library(ANCOMBC)
  library(microViz)
  library(microbiomeMarker)
}

set.seed(1851)

ASV = otu_table(table %>% 
                  column_to_rownames(var = 'featureid') %>% 
                  as.matrix(), taxa_are_rows = TRUE,errorIfNULL = T) 

METADATA = sample_data(metadata %>% 
                         filter(sampleid %in% colnames(ASV)) %>% 
                         column_to_rownames(var = 'sampleid'),
                       errorIfNULL = T)

TAX = tax_table(taxonomy %>% 
                  pivot_wider(names_from = 'level', 
                              values_from = 'taxon') %>% 
                  column_to_rownames(var = 'featureid') %>% 
                  as.matrix(), errorIfNULL = T)

ps <- phyloseq(ASV, TAX, METADATA, check.names = T)

ps_ctl <- ps_filter(ps, group == 'CTL', .keep_all_taxa = T)


ctl_aldex_res <-run_aldex(ps = ps_ctl, 
                            group = 'gm', p_adjust = 'BH',
                            norm = 'none',
                            taxa_rank = 'Genus', pvalue_cutoff = 1)

ctl_aldex_res_table <- marker_table(ctl_aldex_res) %>% as_tibble %>% 
  mutate(enrich_group = case_match(enrich_group,
                                   '4' ~ 'GMLow',
                                   '1' ~ 'GMHigh'))


ctl_aldex_res_table %>% 
  count(padj < 0.05)

61/(106+61)

ctl_aldex_res_table %>% 
  arrange(padj) %>% 
  write_tsv('stats/usv/ctl/ctl_aldex2_res.tsv')



library(ANCOMBC)

ancombc_res <- ancombc2(
  data = ps_ctl, 
  tax_level = 'Genus', 
  fix_formula = 'gm', 
  p_adj_method = 'BH',
  group = 'gm', 
  struc_zero = T, 
  verbose = T
)

ancombc_res$res %>% 
  as_tibble() %>% 
  arrange(q_gm4) %>% 
  write_tsv('stats/usv/ctl/ctl_ancombc2_res.tsv')


ancombc_res$zero_ind %>% 
  write_tsv('stats/usv/ctl/ctl_ancombc2_sz.tsv')

beepr::beep()

fam.df <- onon.df %>%
  select(unique_id, final_id, phylum:species) %>%
  distinct() %>%
  gather(rank, taxon, phylum:species) %>%
  group_by(unique_id, final_id) %>%
  mutate(rank = factor(rank, levels = c("phylum", "subphylum", "class", "subclass",
                                        "order", "suborder", "family", "subfamily",
                                        "tribe", "genus", "species")),
         taxon = if_else(taxon == "", as.character(NA), taxon)) %>%
  fill(taxon) %>%
  spread(rank, taxon)

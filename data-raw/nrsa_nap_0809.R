taxa.df <- read.csv("https://www.epa.gov/sites/production/files/2016-06/nrsa_0809_benttaxa.csv",
                    stringsAsFactors = FALSE)
site.df <- read.csv("https://www.epa.gov/sites/production/files/2015-09/siteinfo_0.csv",
                    stringsAsFactors = FALSE)
counts.df <- read.csv("https://www.epa.gov/sites/production/files/2016-11/nrsa0809bentcts.csv",
                      stringsAsFactors = FALSE)

nrsa_nap_0809 <- site.df %>%
  dplyr::filter(AGGR_ECO9_2015 %in% "NAP") %>%
  dplyr::left_join(counts.df, by = c("UID",
                              "SITE_ID",
                              "VISIT_NO",
                              "DATE_COL")) %>%
  dplyr::select(UID, SITE_ID, RT_NRSA_CAT, TARGET_TAXON, TOTAL) %>%
  dplyr::left_join(taxa.df, by = "TARGET_TAXON") %>%
  dplyr::select(UID:TOTAL, PHYLUM:VOLTINISM) %>%
  dplyr::select(-dplyr::contains("_WSA")) %>%
  dplyr::filter(!is.na(PHYLUM)) %>%
  dplyr::rename_all(base::tolower) %>%
  dplyr::mutate_if(is.character, tolower)

usethis::use_data(nrsa_nap_0809, overwrite = TRUE)

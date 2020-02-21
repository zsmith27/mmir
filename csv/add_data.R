library(tidyverse)
taxa.df <- readr::read_csv("https://www.epa.gov/sites/production/files/2016-06/nrsa_0809_benttaxa.csv")
site.df <- readr::read_csv("https://www.epa.gov/sites/production/files/2015-09/siteinfo_0.csv")
counts.df <- readr::read_csv("https://www.epa.gov/sites/production/files/2016-11/nrsa0809bentcts.csv")

nrsa_nap_0809 <- site.df %>%
  filter(AGGR_ECO9_2015 %in% "NAP") %>%
  left_join(counts.df, by = c("UID",
                              "SITE_ID",
                              "VISIT_NO",
                              "DATE_COL")) %>%
  select(UID, SITE_ID, RT_NRSA_CAT, TARGET_TAXON, TOTAL) %>%
  left_join(taxa.df, by = "TARGET_TAXON") %>%
  select(UID:TOTAL, PHYLUM:VOLTINISM) %>%
  filter(!is.na(PHYLUM))

usethis::use_data(nrsa_nap_0809)
# master.fwmi <- data.table::fread("csv/master_3_24_2017.csv", data.table = FALSE) %>%
#   toolbox::prep_df()
#------------------------------------------------------------------------------
# hier.fwmi <- master.fwmi %>%
#   select(tsn_final, final_id, phylum:species) %>%
#   distinct()
# check_dups(hier.fwmi, final_id) %>%
#   filter(count > 1) %>%
#   pull(final_id)
# devtools::use_data(hier.fwmi, overwrite = TRUE)
#------------------------------------------------------------------------------
# attributes.fwmi <- master.fwmi %>%
#   select(final_id, aspt, beck_class, bibi_tv, bibi_ffg, bibi_habit) %>%
#   distinct()
# check_dups(attributes.fwmi, final_id) %>%
#   filter(count > 1) %>%
#   pull(final_id)
# devtools::use_data(attributes.fwmi, overwrite = TRUE)

#------------------------------------------------------------------------------
# devtools::use_package("data.table")
# devtools::use_package("tidyverse")
# onon.df <- readr::read_csv("csv/ZMS_THESIS_06_18_2017.csv") %>%
#   select(unique_id,
#          final_id,
#          reporting_value,
#          phylum:species) %>%
#   group_by_at(vars(-reporting_value)) %>%
#   summarize(counts = sum(reporting_value)) %>%
#   select(unique_id, final_id, counts, everything())
# usethis::use_data(onon.df,)

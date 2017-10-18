library(tidyverse)
master.fwmi <- data.table::fread("csv/master_2_08_2017.csv", data.table = FALSE) %>%
  clean_df()
#------------------------------------------------------------------------------
hier.fwmi <- master.fwmi %>%
  select(final_id, phylum:species) %>%
  distinct()
check_dups(hier.fwmi, final_id) %>%
  filter(count > 1) %>%
  pull(final_id)
devtools::use_data(hier.fwmi, overwrite = TRUE)
#------------------------------------------------------------------------------
attributes.fwmi <- master.fwmi %>%
  select(final_id, aspt, beck_class, bibi_tv, bibi_ffg, bibi_habit) %>%
  distinct()
check_dups(attributes.fwmi, final_id) %>%
  filter(count > 1) %>%
  pull(final_id)
devtools::use_data(attributes.fwmi, overwrite = TRUE)

#------------------------------------------------------------------------------
devtools::use_package("data.table")
devtools::use_package("tidyverse")

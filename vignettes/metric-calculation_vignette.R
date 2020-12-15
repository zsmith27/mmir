## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install_helper_packages, eval=FALSE--------------------------------------
#  install.packages("devtools",
#                   "dplyr")

## ----install_mmir, eval=FALSE-------------------------------------------------
#  devtools::install_github("zsmith27/mmir")

## ----load_mmir----------------------------------------------------------------
library(mmir)

## ----load_data----------------------------------------------------------------
data("nrsa_nap_0809", package = "mmir")

## -----------------------------------------------------------------------------
nrsa_nap_fill <- nrsa_nap_0809 %>% 
  taxa_fill(.final_id = target_taxon,
            .prefix = "unidentified",
            phylum:genus)

## ----group_nest---------------------------------------------------------------
nest.df <- nrsa_nap_fill %>% 
  dplyr::group_nest(uid,  rt_nrsa_cat, .key = "data") 

## ----rich---------------------------------------------------------------------
rich.df <- nest.df %>% 
  dplyr::mutate(
    rich_family = taxa_rich(.dataframe = .,
                            .key_col = uid,
                            .group_col = family,
                            .counts_col = total,
                            .unnest_col = data),
    rich_genus = taxa_rich(.dataframe = .,
                           .key_col = uid,
                           .group_col = genus,
                           .counts_col = total,
                           .unnest_col = data)
  )


rich.df %>% 
  dplyr::mutate(data = "nested dataframe") %>% 
  head() %>% 
  knitr::kable()

## ----sub_rich-----------------------------------------------------------------
sub_rich.df <- nest.df %>% 
  dplyr::mutate(
    rich_ephemeroptera_fam = taxa_rich(.dataframe = .,
                            .key_col = uid,
                            .group_col = family,
                            .filter = order %in% "ephemeroptera",
                            .counts_col = total,
                            .unnest_col = data),
    rich_ephemeroptera_gen = taxa_rich(.dataframe = .,
                            .key_col = uid,
                            .group_col = genus,
                            .filter = order %in% "ephemeroptera",
                            .counts_col = total,
                            .unnest_col = data),
    rich_ept_gen = taxa_rich(.dataframe = .,
                            .key_col = uid,
                            .group_col = genus,
                            .filter = order %in% c("ephemeroptera",
                                                       "plecoptera",
                                                       "trichoptera"),
                            .counts_col = total,
                            .unnest_col = data)
  )

sub_rich.df %>% 
  dplyr::mutate(data = "nested dataframe") %>% 
  head() %>% 
  knitr::kable() 

## ----div----------------------------------------------------------------------
div.df <- nest.df %>% 
  dplyr::mutate(
    shannon_genus = taxa_div(.dataframe = .,
                            .key_col = uid,
                            .counts_col = total,
                            .group_col = genus,
                            .job = "shannon",
                             .base_log = 2,
                            .unnest_col = data),
    simpson_genus = taxa_div(.dataframe = .,
                            .key_col = uid,
                            .counts_col = total,
                            .group_col = genus,
                            .job = "simpson",
                            .unnest_col = data),
    margalef_genus = taxa_div(.dataframe = .,
                            .key_col = uid,
                            .counts_col = total,
                            .group_col = genus,
                            .job = "margalef",
                            .unnest_col = data),
    menhinick_genus = taxa_div(.dataframe = .,
                            .key_col = uid,
                            .counts_col = total,
                            .group_col = genus,
                            .job = "menhinick",
                            .unnest_col = data),
    pielou_genus = taxa_div(.dataframe = .,
                            .key_col = uid,
                            .counts_col = total,
                            .group_col = genus,
                            .job = "pielou",
                            .unnest_col = data)
  )

div.df %>% 
  dplyr::mutate(data = "nested dataframe") %>% 
  head() %>% 
  knitr::kable() 

## ----sub_div, eval=FALSE------------------------------------------------------
#  sub_div.df <- nest.df %>%
#    dplyr::mutate(
#      gini_simpson_ept = taxa_div(.dataframe = .,
#                              .key_col = uid,
#                              .counts_col = total,
#                              .group_col = order,
#                              .filter = genus %in% c("ephemeroptera",
#                                                         "plecoptera",
#                                                         "trichoptera"),
#                              .job = "gini_simpson",
#                              .unnest_col = data),
#      simpson_ept = taxa_div(.dataframe = .,
#                              .key_col = uid,
#                              .counts_col = total,
#                              .group_col = order,
#                              .filter = genus %in% c("ephemeroptera",
#                                                         "plecoptera",
#                                                         "trichoptera"),
#                              .job = "simpson",
#                              .unnest_col = data),
#      shannon_ept = taxa_div(.dataframe = .,
#                              .key_col = uid,
#                              .counts_col = total,
#                              .group_col = order,
#                              .filter = genus %in% c("ephemeroptera",
#                                                         "plecoptera",
#                                                         "trichoptera"),
#                              .job = "shannon",
#                              .base_log = 2,
#                              .unnest_col = data)
#    )
#  
#  sub_div.df %>%
#    dplyr::mutate(data = "nested dataframe") %>%
#    head() %>%
#    knitr::kable()

## -----------------------------------------------------------------------------
dom.df <- nest.df %>% 
  dplyr::mutate(
    dom_1_target_taxon = taxa_dom(.dataframe = .,
                                  .key_col = uid,
                                  .counts_col = total,
                                  .group_col = target_taxon,
                                  .dom_level = 1,
                                  .unnest_col = data),
    dom_5_target_taxon = taxa_dom(.dataframe = .,
                                  .key_col = uid,
                                  .counts_col = total,
                                  .group_col = target_taxon,
                                  .dom_level = 5,
                                  .unnest_col = data)
  )

dom.df %>% 
  dplyr::mutate(data = "nested dataframe") %>% 
  head() %>% 
  knitr::kable() 

## ----pct----------------------------------------------------------------------
pct.df <- nest.df %>% 
  dplyr::mutate(
    pct_ephemeroptera = taxa_pct(.dataframe = .,
                                  .key_col = uid,
                                  .counts_col = total,
                                  .filter = order %in% "ephemeroptera",
                                 .unnest_col = data),
    pct_ept = taxa_pct(.dataframe = .,
                        .key_col = uid,
                        .counts_col = total,
                        .filter = order %in% c("ephemeroptera",
                                                   "plecoptera",
                                                   "trichoptera"),
                       .unnest_col = data)
  )

pct.df %>% 
  dplyr::mutate(data = "nested dataframe") %>% 
  head() %>% 
  knitr::kable()

## ----abund--------------------------------------------------------------------
abund.df <- nest.df %>% 
  dplyr::mutate(
    abund_ephemeroptera = taxa_abund(.,
                                  .key_col = uid,
                                  .counts_col = total,
                                  .filter = order %in% "ephemeroptera",
                                  .unnest_col = data),
    abund_ept = taxa_abund(.,
                        .key_col = uid,
                        .counts_col = total,
                        .filter = order %in% c("ephemeroptera",
                                                   "plecoptera",
                                                   "trichoptera"),
                        .unnest_col = data)
  )

abund.df %>% 
  dplyr::mutate(data = "nested dataframe") %>% 
  head() %>% 
  knitr::kable()

## ----seq----------------------------------------------------------------------
seq.df <- nest.df %>% 
  dplyr::bind_cols(
    taxa_seq(.dataframe = .,
             .key_col = uid,
             .counts_col = total,
             .filter_cols_vec = c("class", "order"),
             .group_col = genus,
             .job = "rich",
             .exclude_pattern = "unidentified",
             .unnest_col = data)
    )

dplyr::tibble(
  "List of Column Names" = names(seq.df)
) %>% 
knitr::kable()

# seq.df %>% 
#   dplyr::mutate(data = "nested dataframe") %>% 
#   head() %>% 
#   dplyr::select(uid:rich_genus_bivalvia) %>% 
#   knitr::kable()

## ----putting_it_together, eval=TRUE-------------------------------------------
metrics.df <- nest.df %>% 
  dplyr::mutate(
    rich_family = taxa_rich(.dataframe = .,
                            .key_col = uid,
                            .group_col = family,
                            .counts_col = total,
                            .unnest_col = data),
    rich_genus = taxa_rich(.dataframe = .,
                           .key_col = uid,
                           .group_col = genus,
                           .counts_col = total,
                           .unnest_col = data),
    rich_target_taxon = taxa_rich(.dataframe = .,
                                  .key_col = uid,
                                  .group_col = target_taxon,
                                  .counts_col = total,
                                  .unnest_col = data),
    gini_simpson_ept = taxa_div(.dataframe = .,
                                .key_col = uid,
                                .counts_col = total,
                                .group_col = target_taxon,
                                .filter = order %in% c("ephemeroptera",
                                                       "plecoptera",
                                                       "trichoptera"),
                                .job = "gini_simpson",
                                .unnest_col = data),
    simpson_ept = taxa_div(.dataframe = .,
                           .key_col = uid,
                           .counts_col = total,
                           .group_col = target_taxon,
                           .filter = order %in% c("ephemeroptera",
                                                  "plecoptera",
                                                  "trichoptera"),
                           .job = "simpson",
                           .unnest_col = data),
    shannon_ept = taxa_div(.dataframe = .,
                           .key_col = uid,
                           .counts_col = total,
                           .group_col = target_taxon,
                           .filter = order %in% c("ephemeroptera",
                                                  "plecoptera",
                                                  "trichoptera"),
                           .job = "shannon",
                           .base_log = 2,
                           .unnest_col = data),
    pct_ept = taxa_pct(.dataframe = .,
                       .key_col = uid,
                       .counts_col = total,
                       .filter = order %in% c("ephemeroptera",
                                              "plecoptera",
                                              "trichoptera"),
                       .unnest_col = data),
    pct_cote = taxa_pct(.dataframe = .,
                        .key_col = uid,
                        .counts_col = total,
                        .filter = order %in% c("coleoptera",
                                               "odonata",
                                               "trichoptera",
                                               "ephemeroptera"),
                        .unnest_col = data),
    dom_1_target_taxon = taxa_dom(.dataframe = .,
                                  .key_col = uid,
                                  .counts_col = total,
                                  .group_col = target_taxon,
                                  .dom_level = 1,
                                  .unnest_col = data),
    dom_5_target_taxon = taxa_dom(.dataframe = .,
                                  .key_col = uid,
                                  .counts_col = total,
                                  .group_col = target_taxon,
                                  .dom_level = 5,
                                  .unnest_col = data),
    tol_index = taxa_tol_index(.dataframe = .,
                               .key_col = uid,
                               .counts_col = total,
                               .tol_col = ptv,
                               .unnest_col = data)
  ) %>% 
  dplyr::bind_cols(
    taxa_seq(.dataframe = .,
             .key_col = uid,
             .counts_col = total,
             .filter_cols_vec = c("class", "order", "family"),
             .group_col = target_taxon,
             .job = "rich",
             .exclude_pattern = "unidentified",
             .unnest_col = data),
    taxa_seq(.dataframe = .,
             .key_col = uid,
             .counts_col = total,
             .filter_cols_vec = c("class", "order", "family"),
             .group_col = target_taxon,
             .job = "pct_rich",
             .unnest_col = data),
    taxa_seq(.dataframe = .,
             .key_col = uid,
             .counts_col = total,
             .filter_cols_vec = c("class", "order", "family", "genus"),
             .job = "pct",
             .unnest_col = data),
    taxa_seq(.dataframe = .,
             .key_col = uid,
             .counts_col = total,
             .filter_cols_vec = c("class", "order", "family"),
             .group_col = target_taxon,
             .job = "simpson",
             .unnest_col = data),
  )

## -----------------------------------------------------------------------------
nrsa_nap_metrics.df <- metrics.df %>% 
  dplyr::select(-data)

usethis::use_data(nrsa_nap_metrics.df,
                  overwrite = TRUE)


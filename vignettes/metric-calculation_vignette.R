## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("devtools", "DT")

## ---- eval=FALSE--------------------------------------------------------------
#  devtools::install_github("zsmith27/mmir")

## -----------------------------------------------------------------------------
library(mmir)

## -----------------------------------------------------------------------------
data("nrsa_nap_0809", package = "mmir")

## -----------------------------------------------------------------------------
metrics.df <- nrsa_nap_0809 %>% 
  dplyr::group_nest(uid, .key = "data") 

## -----------------------------------------------------------------------------
metrics.df <- metrics.df %>% 
  dplyr::mutate(
    rich_family = taxa_rich(.dataframe = .,
                            .key_col = uid,
                            .group_col = family,
                            .unnest_col = data),
    rich_genus = taxa_rich(.dataframe = .,
                            .key_col = uid,
                            .group_col = genus,
                            .unnest_col = data)
  )


DT::datatable(metrics.df, options = list(scrollX = TRUE))  

## -----------------------------------------------------------------------------
metrics.df <- metrics.df %>% 
  dplyr::mutate(
    rich_ephemeroptera_fam = taxa_rich(.dataframe = .,
                            .key_col = uid,
                            .group_col = family,
                            .filter = order %in% "ephemeroptera",
                            .unnest_col = data),
    rich_ephemeroptera_gen = taxa_rich(.dataframe = .,
                            .key_col = uid,
                            .group_col = genus,
                            .filter = order %in% "ephemeroptera",
                            .unnest_col = data),
    rich_ept_gen = taxa_rich(.dataframe = .,
                            .key_col = uid,
                            .group_col = genus,
                            .filter = order %in% c("ephemeroptera",
                                                       "plecoptera",
                                                       "trichoptera"),
                            .unnest_col = data)
  )

DT::datatable(metrics.df, options = list(scrollX = TRUE))  

## -----------------------------------------------------------------------------
metrics.df <- metrics.df %>% 
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

DT::datatable(metrics.df, options = list(scrollX = TRUE))  

## ---- eval=FALSE--------------------------------------------------------------
#  metrics.df <- metrics.df %>%
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
#  DT::datatable(subdiv.df, options = list(scrollX = TRUE))

## -----------------------------------------------------------------------------
metrics.df <- metrics.df %>% 
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

DT::datatable(metrics.df, options = list(scrollX = TRUE))  

## -----------------------------------------------------------------------------
metrics.df <- metrics.df %>% 
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

DT::datatable(metrics.df, options = list(scrollX = TRUE))  

## -----------------------------------------------------------------------------
metrics.df <- metrics.df %>% 
  dplyr::bind_cols(
    taxa_seq(.dataframe = .,
             .key_col = uid,
             .counts_col = total,
             .filter_cols_vec = c("class", "order", "family"),
             .group_col = genus,
             .job = "rich",
             .unnest_col = data),
    taxa_seq(.dataframe = .,
             .key_col = uid,
             .counts_col = total,
             .filter_cols_vec = c("class", "order", "family"),
             .group_col = genus,
             .job = "pct_rich",
             .unnest_col = data),
    taxa_seq(.dataframe = .,
             .key_col = uid,
             .counts_col = total,
             .filter_cols_vec = c("class", "order", "family"),
             .group_col = genus,
             .job = "pct",
             .unnest_col = data),
    taxa_seq(.dataframe = .,
             .key_col = uid,
             .counts_col = total,
             .filter_cols_vec = c("class", "order", "family"),
             .group_col = genus,
             .base_log = 2,
             .job = "shannon",
             .unnest_col = data))

DT::datatable(metrics.df, options = list(scrollX = TRUE))  


# mmir 0.1.0

## Breaking Changes

* `taxa_rich()` and `taxa_pct_rich()` now require the `.count_col` argument to be specified. In previous versions, the user did not need to provide this value. This change ensures that zero counts will be excluded from the calculations.

## Bug Fix

* When using the `.filter` argument to `taxa_rich()`, if the specified taxon was not observed, a value of `NA` was returned and subsequently counted as a count of 1. This had a downstream impact on `taxa_pct_rich()`, which was sometimes producing values of 100. This issue has been resolved.

## New Feature

* `taxa_seq()` has a new argument, `.exclude_pattern`. This argument allows the user to specify a character string pattern or a vector of patterns that will exclude taxonomic names that match the pattern(s) from the iterative metric calculation sequence. For example, `.exclude_pattern = "unidentified"` would remove any taxonomic name that contains "unidentified" from the iterative taxa sequence (i.e., no metrics will be returned for taxa that contain this pattern). Multiple patterns can be supplied within a character vector (e.g., `.exclude_pattern = c("unidentified", "hyallella")`.

# mmir 0.0.3

## Bug Fix

* `taxa_seq()`: when the argument `.group_col = NULL` and the `.job = "pct"` "NULL" would appear in the metric name. Now "NULL" will be correctly filtered out of the name.

# mmir 0.0.2

* Added a `NEWS.md` file to track changes to the package.

* Added `taxa_fill()` to replace `NA`s in the taxonomic hierarchy with the nearest identified taxonomic rank. Note that the order the columns are presented is used to create the hierarchy that identifies the previous taxonomic rank. For example, you want your columns to be ordered as: Phylum, Class, Order, Family, Genus.

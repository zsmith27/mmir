# mmir 0.0.3

## Bug Fix

* `taxa_seq()`: when the argument `.group_col = NULL` and the `.job = "pct"` "NULL" would appear in the metric name. Now "NULL" will be correctly filtered out of the name.

# mmir 0.0.2

* Added a `NEWS.md` file to track changes to the package.

* Added `taxa_fill()` to replace `NA`s in the taxonomic hierarchy with the nearest identified taxonomic rank. Note that the order the columns are presented is used to create the hierarchy that identifies the previous taxonomic rank. For example, you want your columns to be ordered as: Phylum, Class, Order, Family, Genus.

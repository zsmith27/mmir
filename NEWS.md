# mmir 0.0.3

# mmir 0.0.2

* Added a `NEWS.md` file to track changes to the package.

* Added `taxa_fill()` to replace `NA`s in the taxonomic hierarchy with the nearest identified taxonomic rank. Note that the order the columns are presented is used to create the hierarchy that identifies the previous taxonomic rank. For example, you want your columns to be ordered as: Phylum, Class, Order, Family, Genus.

# plantR 0.1.3

* Fixing bugs in `formatDwc()` related to the difference in the number of
columns that speciesLink returns for queries using different taxa and for the
binding of columns with different data types.

* `rgbif2()` now has warnings and workarounds related to the maximum limit of
100,000 records per query

* fixes in `formatOcc()`, `validateCoord()`, `validateDup()`, and
`summaryFlags()` for the particular case of only one record per species

* minor fix in `prepFamily()` to get missing family names based on genera

* small improvements in `prepCoord()`, `getYear()`, `addRank()` and
`formatTax()`


# plantR 0.1.2

* New tutorial on how __plantR__ can be used to update databases of biological
collections (currently in Portuguese, only)

* `rspeciesLink()` now supports download at the family level

* `rgbif2()` now supports download at any taxonomic level and using user-defined 
maximum number of records for download

* New accessory functions added to better handle taxonomic information

* `fixSpecies()` with several improvements related to scientific name casing and
authorities

* `saveData()` now supports user-specified file names

* `getYear()` improved to handle more date formats

* `mergeDup()` now also takes into account the scientific name status to
prioritize the merge of taxonomic information within duplicates

* minor bug fixes and improvements in `prepSpecies()` and a small update of the
list of taxonomists related to the new tutorial 

# plantR 0.1.1

* Added function `readData()` to read DwC-A zip files from GBIF

* `checkCoord()` now supports user provided maps

* `prepSpecies()` adapted to the new __Taxonstand__ version 2.3

* Added a `NEWS.md` file to track changes to the package.


# plantR 0.1.0

* The first public version of the package in GitHub.

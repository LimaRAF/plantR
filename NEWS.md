# plantR NEWS

<br/>

## version 0.1.5

* Fixing a small bug function `prepDup()` related to issues #86 and #87

* Minor improvements in functions `fixLoc()`, `prepCoord()`, 
`prepFamily()` and `getYear()`. 

* Minor changes in several package functions related to the internal use of 
functions from the package `stringr`. 


## version 0.1.4

* Fixing an issue related to non-NA type specifications in `validateTax()` and
`checkList()` used to validate species identifications and detect type
specimens.

* Fixing a small issue related to non-preserved specimens observation in
`validateTax()` to assign confidence levels to species identifications. 

* New (simple) internal function `plotCoord()` to help the
visualization of the coordinate validation process

* New (simple) internal function `fixEncoding()` to help solving 
common encoding problems of text in 'latin1'

* Inclusion of an argument to make optional the prints of functions:
`prepFamily()`, `summaryData()`, `summaryFlags()` and `validateTax()`

* Major updates on the plant collection code dictionary and minor changes in
the plant taxonomist dictionary

* Function `getDup()` now makes the distinction between physical and purely
virtual duplicates

* Function `mergeDup()` now provides the reference specimens used to homogenize
the information within groups of duplicates. The reference specimen is provided
separately for the taxonomic, locality and coordinate information. We also fixed
a bug related to the homogenization of the geographical and locality information

* Function `prepTDWG()` (and consequently `prepName()`) now provides the
argument 'pretty' which controls how the output names are presented. By default
the function returns, as before, names in a 'pretty' way (i.e. only the first
letter of names capitalized, initials separated by points and no spaces, and
family name prepositions in lower cases). But now the function also returns the
names in the desired format but presented in the same way as the input names

* Function `formatDwc()` now accepts data downloaded from the BIEN database and
it includes a dataset-specific option to solve common latin1 encoding problems

* Function `fixName()` now includes an option to detect and solve potentially
problematic cases when the name notation uses commas to separate multiple
people's names. This option is controlled by the new argument `bad.comma`

* Minor improvements in `checkList()`, `fixSpecies()`, `prepName()`,
`getPrep()`, `getInit()`, `getYear()`, `colNumber()`, `lastName()`,
`checkCoord()`, `mahalanobisDist()`, `prepFamily()` and `addRank()`.

* Adding tests to most of the package functions

* Solving some minor problems in the world map objects to make them compatible
to the use of the spherical geometry operators of package __s2__, which is now
the default of package __sf__ version >= 1.0



<br/>

## version 0.1.3

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


<br/>

## version 0.1.2

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


<br/>

## version 0.1.1

* New function `readData()` to read DwC-A zip files from GBIF

* `checkCoord()` now supports user provided maps

* `prepSpecies()` adapted to the new __Taxonstand__ version 2.3

* Added a `NEWS.md` file to track changes to the package.


<br/>

## version 0.1.0

* The first public version of the package in GitHub.

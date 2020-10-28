
<!-- README.md is generated from README.Rmd. Please edit that file -->
plantR
======

Management of Plant Occurrence Records.

Description
-----------

The package plantR provides tools for retrieving, processing, cleaning and validating occurrence records of plant species from biological collections.

### Installation

You can install the plantR from [github](https://github.com/) with:

``` r
library("remotes")
install_github("LimaRAF/plantR")
library("plantR")
```

Details
-------

Managing the information linked to species records from herbarium collections is an important but often difficult task. The notation often differs between collections and even within collections because collection authors often provide information in different formats. In addition, it is often difficult to validate the geographical locality and taxonomy of individual records, especially when working with thousands of records. Thus, having tools to perform automatic processing and validation of herbarium records can be quite handy for collection curators, taxonomists, ecologists and conservationists.

The package **plantR** was developed to deal with data from herbarium collections, providing tools to perform:

-   the download of herbarium occurrences for a list of species names, collections codes or other search fields;

-   the batch processing of typical herbarium fields (e.g. collector name);

-   the validation of the locality and geographical coordinates of species occurrences, based on maps and gazetteers;

-   the validation of plant nomenclature, based on different taxonomic backbones of accepted names and spelling;

-   the assessment of the confidence level of species identifications, based on a global list of plant taxonomists.

Currently, the download of records is available for the Global Biodiversity Information Facility ([GBIF](https://www.gbif.org/)), and [speciesLink](http://splink.cria.org.br/), but the user can also provide their own dataset as an input. Field editing covers most of the typical variation in the notation of author and determiner names, trying to provide standardized outputs in the TDWG format. Currently, geographical validation can be performed at the county level for Latin American countries and at the country level for the rest of the world. We provide a gazetteer to retrieve and check geographical coordinates, which is currently biased towards Latin American countries, particularly for Brazil. Taxonomical validation is performed based on the correction of plant names to their most recent valid and accepted name and the confidence on the taxonomical identification uses a compilation of plant taxonomist names from all over the world.

Basic assumptions of the data validation
----------------------------------------

In case of invalid or missing coordinates, we assume that the country, state, county (and locality) given in the specimen label are correct (i.e. locality prevails over coordinates), and so the working coordinates are taken from a gazetteer instead.

We ignore records with coordinates given only at the county level, assuming that our gazetteer is a more complete and/or safe source of county coordinates (this may not be the case for sites outside Latin America). It is also important to note that if the occurrence information on the localities are indeed mistaken (eg. wrong/missing county name), then the locality will not be found in the gazetteer and thus, even if the original coordinates are good, they may be replaced by other coordinates taken from the gazetteer.

During the assignment of the taxonomic confidence level of the identifications, we did not attempt to set priorities for different specialists within a given family. That is, all the specimens determined by the specialist within their family of expertise were taken as being validated. Although we recognize that there are specialists for one or a few genera within a family, the current validation process is only carried at the family level.

In the case of conflicting species identification among specialists for duplicates across different collections, we simply assume the most recent identification as being the most up-to-date one.

Authors
-------

Renato A. F. de Lima, Sara R. Mortara, Andrea Sánchez-Tapia, Hans ter Steege & Marinez de Siqueira Ferreira

References
----------

Funding
-------

The development of this package was supported by the European Union’s Horizon 2020 research and innovation program under the Marie Skłodowska-Curie grant agreement No 795114.

Acknowledgements
----------------

We thank Sidnei Souza from speciesLink for his help with the network API.

Many localities used to construct the gazetteer used by the package were provided from [CNCFlora](http://cncflora.jbrj.gov.br) and TreeCo database.

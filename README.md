
<!-- README.md is generated from README.Rmd. Please edit that file -->
plantR
======

Management of Plant Occurrence Records.

Description
-----------

The package plantR provides tools for retrieving, processing, cleaning and validating occurrence records of plant species.

Details
-------

Managing the information linked to species records from herbarium collections is an important but often difficult task. The notation often differ between collections and even within collections because collection authors often provide information in different formats. In addition, it is often difficult to validate the geographical locality and taxonomy of individual records, especially when working with thousands of records. Thus, having tools to perform the (fast) processing and validation of herbarium records can be quite handy for collection curators, taxonomists, ecologists and conservationists.

The package plantR was developed to deal with data from herbarium collections, providing tools to perform:

-   the download of herbarium data for a list of species names or collections codes;
-   the batch processing of typical herbarium fields (e.g. collector name);
-   thw geographical and taxonomic validation of herbarium records.

Currently, the download of records is available for speciesLink (website) and GBIF (website), but the user can also provide its own dataset as an input. Field editing covers most of the typical variation in the notation of author and determiner names, trying to provide standardized ouputs in the TDWG format. Currently, geographical validation can be performed at county-level for Latin American countries and at country level for the rest of the world. We provide a gazetteer to retrieve and check geographical coordinates, which is currenlty biased towards Latin American countries, particularly for Brazil. Taxonomical validation is performed based on a compilation of plant taxonomist names from all over the world.

Authors
-------

References
----------

See Also
--------

Installation
------------

You can install the released version of plantR from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("plantR")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:

``` r
#summary(cars)
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date.

You can also embed plots, for example:

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub!

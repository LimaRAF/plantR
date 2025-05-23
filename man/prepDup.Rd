% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepDup.R
\name{prepDup}
\alias{prepDup}
\title{Prepare For Duplicate Specimen Search}
\usage{
prepDup(
  x,
  col.names = c(family = "family.new", species = "scientificName.new", col.name =
    "recordedBy.new", col.last.name = "last.name", col.number = "recordNumber.new",
    col.year = "year.new", col.loc = "municipality.new", loc.str = "loc.correct"),
  comb.fields = list(c("family", "col.last.name", "col.number", "col.loc"), c("family",
    "col.year", "col.number", "col.loc"), c("species", "col.last.name", "col.number",
    "col.year"), c("col.year", "col.last.name", "col.number", "col.loc")),
  rec.ID = "numTombo",
  noYear = "s.d.",
  noName = "s.n.",
  noNumb = "s.n.",
  ignore.miss = TRUE
)
}
\arguments{
\item{x}{a data frame with the species records.}

\item{col.names}{vector. A named vector containing the names of columns in
the input data frame for each of the information that should be used to
create the duplicate search string(s). Default to the \strong{plantR} output
column names.}

\item{comb.fields}{list. A list containing one or more vectors with the
information that should be used to create the duplicate search strings.
Default to four vectors of information to be combined.}

\item{rec.ID}{character. The name of the columns containing the unique record
identifier (see function \code{getTombo()}). Default to 'numTombo'.}

\item{noYear}{character. Standard for missing data in Year. Default to
"n.d.".}

\item{noName}{character. Standard for missing data in collector name. Default
to "s.n.".}

\item{noNumb}{character. Standard for missing data in collector number.
Default to "s.n.".}

\item{ignore.miss}{logical. Should the duplicate search strings with
missing/unknown information (e.g. 'n.d.', 's.n.', NA) be excluded from the
duplicate search. Default to TRUE.}
}
\description{
This function creates the duplicate search strings by
concatenating the information on the taxonomy, collection and locality of
the records.
}
\details{
Three groups of fields are available to produce the duplicate search
string, and they are related to taxonomy, collection and locality of the
specimen. These fields should be provided to the argument \code{col.names} and
they are:
\itemize{
\item 'family': the botanical family (default: 'family.new')
\item 'species': the scientific name (default: 'scientificName.new')
\item 'col.name': the collector name (default: 'recordedBy.new')
\item 'col.last.name': the collector last name (default: 'last.name')
\item 'col.number': the collector serial number (default: 'recordNumber.new')
\item 'col.year': the collection year (default: 'year.new')
\item 'col.loc': the collection locality (default: 'municipality.new')
}

The corresponding columns that should be used to retrieve these fields in
the input data frame must be provided as a named vector in the argument
\code{col.names}, in which the fields listed above are the names and
each element is the corresponding column name in the input data frame.

If an element named 'loc.str' containing the column name of the \strong{plantR}
locality string (i.e. 'loc.correct') is also provided, it can be used to
complement any missing locality information in the locality of the
collection (i.e 'col.loc') that may have been retrieved in the data
processing within the \strong{plantR} workflow.

The duplicate search strings are created by combining the fields listed
above. Each combination of those fields (e.g. 'col.name' and 'col.number')
should be provided to the argument \code{comb.fields} as a vector within a list.
The number of strings to be generated will correspond to the number of
vectors in this list. The order of the fields within vectors does not
change the duplicate search process.

The argument \code{rec.ID} should indicate the column name in the input data
containing the unique record identifier, which in the \strong{plantR} workflow
is obtained using the function \code{getTombo()}. If only GBIF data is used,
this column could be the field 'gbifID'. This identifier is used to
indicate the groups of duplicated records, which is one of the outputs of
function \code{getDup()} and is used to homogenize information within the groups
of duplicates (function \code{mergeDup()}).

Please note that the retrieval of duplicates greatly depends on the
completeness of the input information and in the amount of differences of
notation standards among collections. In addition, the smaller the vectors
of fields to be combined to create the duplicate strings, the higher the
number of (true and false) duplicates will be retrieved.
}
\seealso{
\link[plantR]{getTombo}, \link[plantR]{getDup} and \link[plantR]{mergeDup}.
}
\author{
Renato A. F. de Lima
}

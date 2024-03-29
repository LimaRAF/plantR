% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepCountry.R
\name{prepCountry}
\alias{prepCountry}
\title{Format Country Name}
\usage{
prepCountry(x, to.lower = TRUE, special.char = FALSE, rm.abbrev = TRUE)
}
\arguments{
\item{x}{a vector of country names to be standardized}

\item{to.lower}{logical. Should the output names be return in lower cases?
Default to TRUE.}

\item{special.char}{logical. Should special characters be maintained? Default
to FALSE.}

\item{rm.abbrev}{logical. Should common name abbreviation be replaced?
Default to TRUE.}
}
\value{
The input vector \code{x} in the standard name notation (see Details)
}
\description{
Simple function to standardize the notation of country name
(administrative level 0) by converting country codes to their long names
and by removing special characters and some country name prepositions and
separators.
}
\details{
Country information is formatted into a standard notation, i.e. long
name format (in English). By default, all letters are lower-cased (argument
\code{to.lower}) and special characters (argument \code{special.char}) and common
abbreviations (e.g. 'st.') are removed (argument \code{rm.abbrev}). These edits
aim at reducing possible variation in country name notation and facilitate
further data processing and comparison within the \strong{plantR} workflow.

All country information with less than four letters are treated as country
codes and they are converted to the long format. Currently, only the ISO
3166-1 alpha-2 and alpha-3 codes are considered for conversion to the long
country name format.
}
\examples{
# Creating a data frame with locality information
paises <- c("VC", "VCT", "St. Vincent and the Grenadines",
"St. Vincent & Grenadines", "Saint-Martin", "Falkland Is.", NA)

# Formating the locality information
prepCountry(paises)
prepCountry(paises, to.lower = FALSE)
prepCountry(paises, rm.abbrev = FALSE)

}
\author{
Renato A. F. de Lima
}
\keyword{internal}

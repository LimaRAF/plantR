% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fixSpecies.R
\name{fixSpecies}
\alias{fixSpecies}
\title{Edit Scientific Name Notation}
\usage{
fixSpecies(
  x = NULL,
  tax.name = "scientificName",
  rm.rank = FALSE,
  rm.indet = FALSE
)
}
\arguments{
\item{x}{a vector or data.frame containing the species name}

\item{tax.name}{character. Name of the columns containing the species name.
Default to "scientificName"}

\item{rm.rank}{logical. Should the infra-specific rank abbreviation be
removed from the name? Default to FALSE}

\item{rm.indet}{logical. Should the abbreviations for unspecific names (i.e.
sp. or spp.) be removed? Default to FALSE}
}
\value{
The original data frame (or the input vector as a data frame) with the new
columns \code{verbatimSpecies} with small edits before flagging,
\code{scientificNameStatus} with the flags in original data and
\code{scientificName.new} with a suggestion for a more correct name. See Details
for a description of flags in the column \code{scientificNameStatus}.
}
\description{
Identifies open nomenclature (aff., cf.) in scientific names,
classification under species level (var. and subsp.). It creates a new
column with the new suggested name and it also flags problematic names
(character string with numbers, authors, wrong case, or other names besides
genus and epithet etc). Names can be returned with or without
infra-specific ranks (var. and subsp.) or abbreviations of unspecific
names (sp. or spp.). In the case of names with authors, authorship is
currently removed from scientific names.
}
\details{
Possible flags returned in \code{scientificNameStatus}: \describe{
\item{\code{possibly_ok}}{scientific name following the expected pattern
'Genus epithet'}
\item{\code{not_Genus_epithet_format}}{scientific name not following
the expected pattern Genus epithet}
\item{\code{variety}}{scientific name with variety}
\item{\code{subspecies}}{scientific name with subspecies}
\item{\code{form}}{scientific name with form}
\item{\code{infra_specific}}{scientific name with genus, specific epiteth and
infra-specific, but no infra-specific rank}
\item{\code{hybrid_species}}{scientific name of a hybrid species}
\item{\code{conferre}}{open nomenclature cf. in the scientific name}
\item{\code{affinis}}{open nomenclature aff. in the scientific name}
\item{\code{indet}}{taxon identified only at genus level}
\item{\code{subfamily_as_genus}}{subfamily as genus, not a valid name}
\item{\code{family_as_genus}}{family as genus, not a valid name}
\item{\code{order_as_genus}}{order as genus, not a valid name}
\item{\code{incertae_sedis}}{scientific name of uncertain placement}
\item{\code{species_nova}}{species name contains an indication of a new
species, possibly not yet a valid name}
\item{\code{name_w_authors}}{scientific name has authors}
\item{\code{name_w_wrong_case}}{scientific name has upper/lowercase issues}
\item{\code{name_w_non_ascii}}{species name has non ASCII characters, not a
valid name}
\item{\code{abbreviated_genus}}{genus is abbreviated}
\item{\code{not_name_has_digits}}{scientific name has digits, not a valid
name} }
}
\examples{
df <- data.frame(scientificName =
c("Lindsaea lancea", "Lindsaea lancea (L.) Bedd.",
"Lindsaea lancea var. Angulata",
"Lindsaea Aff. lancea",
"Lindsaea", "Lindsaea sp.", "Lindsaeaceae sp.",
"Lindsaea aff. lancea (L.) Bedd.",
"Lindsaea ×improvisa K.U.Kramer",
"Parablechnum C.Presl",
"Blechnum spannagelii Rosenst.",
"Blechnum occidentale leopoldense Dutra",
"Blechnum austrobrasilianum de la Sota"))

fixSpecies(df)
fixSpecies(df, rm.rank = TRUE)
fixSpecies(df, rm.rank = TRUE, rm.indet = TRUE)

}
\references{
Sigovini, M., Keppel, E. and Tagliapietra, D. (2016) Open Nomenclature in the
biodiversity era. Methods in Ecology and Evolution 7(10): 1217-1225.
}
\author{
Sara Mortara & Renato A. Ferreira de Lima
}

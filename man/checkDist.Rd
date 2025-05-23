% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkDist.R
\name{checkDist}
\alias{checkDist}
\title{Check Valid Distribution}
\usage{
checkDist(
  x,
  tax.name = "scientificName",
  tax.author = "scientificNameAuthorship",
  loc = "loc.correct",
  sep = "_",
  source = "bfo"
)
}
\arguments{
\item{x}{a data.frame containing the taxon name, taxon authorship, and the
record location.}

\item{tax.name}{character. The name of the column containing the taxon name.
Default to "scientificName".}

\item{tax.author}{character. The name of the column containing the taxon name
authorship. Default to "scientificNameAuthorship".}

\item{loc}{character. The name of the column containing the record location.
Default to "loc.correct".}

\item{sep}{character. The separator defining different administrative levels
in both location and known distribution columns. (e.g.,
"country_state_municipality_locality"). \code{sep} can be one of \code{"_"},
\code{"/"}, or \code{"|"}. Default to \code{"_"}.}

\item{source}{character or \code{data.frame}. Source to be consulted for known
distribution information. One of "bfo", "wcvp", or a user-defined
\code{data.frame}. Default to "bfo".}
}
\value{
The input data.frame \code{x} with two additional columns containing the
information if there was a match in the location string (\code{dist.check}) and
auxilliary information (\code{dist.check.obs}).
}
\description{
The function checks the location information obtained for each
species record with the Flora do Brasil Online, the World Checklist of
Vascular Plants (WCVP), or a user-defined data set.
}
\details{
The function provides a way to check if the species distribution
obtained from the \strong{plantR} location string (e.g, \code{loc.correct} =
"brazil_sao paulo_campinas") matches the species known distribution in a
specific \code{source}. The information is returned in the column \code{dist.check},
with auxilliary information being returned in column \code{dist.check.obs}. If
\code{dist.check} is "ok.dist", it means that the location string of the record
is within the species known distribution. If \code{dist.check} is
"invalid.dist", it means that the record is outside of the species known
distribution. For \code{source} = "bfo" and \code{dist.check} "no.cannot.check", it
means that the record cannot be validated at state-level (\code{loc.correct} =
"brazil") or that it occurs outside of Brazil (\code{loc.correct} =
"chile_santiago"). \code{dist.check} is \code{NA} when there is no location string
available for the record or the species distribution is unknown. Column
\code{dist.check.obs} will indicate if the input species name is complete
(\code{scientificName}+\code{scientificAuthorship}; "full.name.match"), partially
complete (\code{scientificName} only; "canonical.name.match"), or do not exist
in the \code{source} (for \code{source} = "bfo" only; "no.match"). When \code{source} is a
user-defined \code{data.frame}, \code{checkDist()} works with two administrative
levels (e.g., country and state; state and municipality) and checks if the
record was found in a coarser administrative level, e.g., only at
country-level ("ok.dist.coarser") or in a finer ("ok.dist.finer"). For a
user-defined \code{data.frame}, the \code{data.frame} should contain columns called
\code{tax.name}, \code{tax.author}, and \code{taxon.distribution} and indicate the
occurrence of multiple species known distributions with a pipe \code{"|"}.

The species known distribution can be obtained from three \code{source}: Flora
do Brasil Online ("bfo", the default), The World Checklist of Vascular
Plants (WCVP) or a "user-defined" \code{data.frame}. "bfo" provides the species
known distribution at Brazilian state-level (e.g, BR-PR), while "wcvp" uses
the World Scheme for Recording Plant Distribution (WGSRPD, Brummitt et al.,
2006) and provides the species known distribution at the
\emph{Botanical Country} and \emph{Basic Recording Units} levels.

Due to taxonomic differences between backbones, we recommend to prepare the
names according to the source of species known distribution to be used
(ideally using \strong{plantR} \code{formatTax()} function). That is, if the user is
using BFO backbone to check taxonomy, use \code{source} = "bfo". As an example,
in BFO, Araucaria angustifolia (Bertol.) Kuntze is the accepted name, while
in WCVP is Araucaria angustifolia (Bertol.) Steud. If the user were to use
the BFO species name to check botanical countries (\code{source} = "wcvp") he
would incur in a warning in the output column \code{dist.check.obs}, indicating
that only the canonical name could be found.
}
\examples{
spp_names <- c(rep("Ocotea porosa", 8),
               rep("Araucaria angustifolia", 2),
               rep("Oreodaphne porosa", 2),
               "Echites hirsutus var. angustifolius",
               "Acalypha annobonae")
spp_authors <- c(rep("(Nees & Mart.) Barroso", 8),
                rep(NA, 2),
                "Nees & Mart.", NA,
                "Stadelm.",
                "Pax & K.Hoffm.")
loc <- c("brazil", "brazil_parana",
"brazil_santa catarina_blumenau_parque são francisco",
"paraguay",
"japan_hokkaido",
"paraguay_asuncion",
"brazil_amazonas_manaus",
NA,
"brazil_espirito santo",
"brazil_santa catarina_blumenau",
"chile_santiago", "brazil_parana_curitiba", "brazil_minas gerais",
"argentina_buenos aires")

df <- data.frame(scientificName = spp_names,
                 scientificNameAuthorship = spp_authors,
                 loc.correct = loc)
checkDist(df)

}
\references{
Brummitt, R. K., Pando, F., Hollis, S., & Brummitt, N. A. (2001). World
geographical scheme for recording plant distributions (Vol. 951, p. 952).
Pittsburg: International working group on taxonomic databases for plant
sciences (TDWG).
}
\seealso{
Functions \link[plantR]{fixLoc}, \link[plantR]{prepLoc},
\link[plantR]{checkCoord}, \link[plantR]{validateCoord}.
}
\author{
Guilherme S. Grittz & Renato A. F. de Lima
}

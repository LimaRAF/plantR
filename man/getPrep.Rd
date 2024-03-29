% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getPrep.R
\name{getPrep}
\alias{getPrep}
\title{Isolate Family Name Prepositions}
\usage{
getPrep(
  x,
  preps = c("De", "Dos", "Do", "Da", "Das", "Del", "Du", "Des", "Di", "Dalla", "Della",
    "Ter", "Von", "Van", "De La", "De Las", "De Lo", "De Los", "Van Der", "Van Den"),
  rm.prep = FALSE,
  output = "matrix",
  format = "last_init_prep"
)
}
\arguments{
\item{x}{a name string, a vector of names or a two-column matrix or
data.frame, containing the last name in the first column and other names in
the second.}

\item{preps}{a vector with the name prepositions to be isolated. Defaults to
some common prepositions in Portuguese, Spanish, Italian, French, and Dutch
family names.}

\item{rm.prep}{logical. Should the preposition be removed? Default to FALSE.}

\item{output}{character. Should the names be returned as a vector of
standardized names or organized as a matrix? Default to "matrix".}

\item{format}{character. Format of the output vector of names The default
("last_init") is the TDWG standard, but the inverse format can also be
chosen ("init_last").}
}
\value{
A vector or a matrix containing the names provided in \code{x} with
the name prepositions isolated from the last name.
}
\description{
This function isolates and removes (optional) last name
prepositions (i.e. 'da' in 'da Silva') from multiple people's names, if
present.
}
\details{
By default, the function searches for common name prepositions but
the user can provide a personal list.

The function assumes that prepositions can be at the start/end of the
string for names in the 'Last name, First name(s)' format (e.g. "da Silva,
Maria" or "Silva, Maria da"), separated by an space to the right/left from
other names. Or it assumes that prepositions can be at the middle in the
'First name(s) Last name' format ('Maria da Silva'), separated by spaces in
both sides.

In the case of a vector of names, the 'Last name, First name(s)' name
format is prioritized over the 'First name(s) Last name' format while
separating last from first names. In addition, only the prepositions in the
last name are isolated and can be returned. Prepositions in middle names
are silently excluded.

Names can be provided as vectors of names or as a two-column matrix/data
frame in which the last names are provided in the first column and other
names in the second. Users can also chose the output format between
"last_init_prep" or "last_init" (the default; e.g. "Silva, Maria A.
Pereira"), "prep_last_init" (e.g. "da Silva, Maria A. Pereira") or
"init_last" (e.g. "Maria A. Pereira (da) Silva"), using the argument
\code{format}.
}
\examples{
names <- c("Silva, Maria A. Pereira da", "Silva, Maria A. Pereira Da",
"da Silva, Maria A. Pereira", "ter Braak, Hans", "Braak, Hans ter",
"Silva, Maria A. Pereirada", "Braak, Hanster", "Maria A. Pereira da Silva",
"Hans ter Braak", "Maria A. Pereirada Silva", "Hanster Braak", "da Silva",
"Silva")

\dontrun{
getPrep("Maria da Silva")
getPrep(names)
getPrep(names, output = "vector")
getPrep(names, output = "vector", format = "prep_last_init")
getPrep(names, output = "vector", format = "init_last")
}

}
\author{
Renato A. F. de Lima
}
\keyword{internal}

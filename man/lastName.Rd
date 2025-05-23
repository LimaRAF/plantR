% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lastName.R
\name{lastName}
\alias{lastName}
\title{Get Last Name}
\usage{
lastName(
  name,
  noName = "s.n.",
  invert = FALSE,
  initials = FALSE,
  first.capital = TRUE
)
}
\arguments{
\item{name}{the character string or vector containing the names.}

\item{noName}{character. The standard notation for missing names in
\code{name}. Default to "s.n.".}

\item{invert}{logical. Should first name(s) be returned instead of
the last name? Default to FALSE.}

\item{initials}{logical. If first name(s) are chosen, should they
be returned in full or only their initials? Default to FALSE.}

\item{first.capital}{logical. Should the name returned have only
the first letter(s) capitalized or returned as provided? Default
to TRUE.}
}
\value{
the last name provided in \code{name} with the first letter
capitalized.
}
\description{
Extract the last name of the collector or determiner
of a biological specimen.
}
\details{
The function works for simple last names or compound last
names, independently if names are provided in lower or capital
letters. It is relatively stable to the format and spacing of the
string provided, but it may not work in all cases.

It implicitly assumes that last names are (i) the ones provided
at the end of the name string if there is no comma, or (ii) the
first name preceding the comma, if there is a comma in the name
string. Few exceptions related to names not in the 'first + last
name' or 'last name + comma + first name' formats (e.g.
'Hatschbach G.G.') are also considered.

If only one name is given, the function returns the same name
with the first letter capitalized.

For missing names (i.e. "", " " or NAs) the function returns the
character defined by the argument \code{noName}.

The function can also return all names but the last name detected
by setting the argument \code{invert} to TRUE. In this case, user can
choose between full first names and only their initials by
setting the argument \code{initials} to TRUE.
}
\examples{

# Simple last name
 lastName("Gert Hatschbach")
 lastName("Gert Guenther Hatschbach")
 lastName("Gert G. Hatschbach")
 lastName("GERT GUENTHER HATSCHBACH")
 lastName("HATSCHBACH, G.G.")
 lastName("HATSCHBACH, G. G.")
 lastName("G. G. HATSCHBACH")
 lastName("Hatschbach GG")

 # Last name with generational suffixes
 lastName("Hermogenes Leitao Filho")
 lastName("Leitao Filho, H.")
 lastName("Leitao Filho, H.F.")
 lastName("Filho Neto, S.J.")

 # Compound last name
 lastName("Augustin Saint-hilaire")
 lastName("Saint-Hilaire A.")

 # Multiple names
 names <- c("Hatschbach, GG", "Hatschbach G.G.",
 "Leitao filho, H. F.","Saint-Hilaire, augustin")
 lastName(names)
 lastName(names, invert = TRUE)
 lastName(names, invert = TRUE, initials = TRUE)

 # Unusual formatting
 lastName("Cesar Sandro, Esteves, F")
 lastName("C.S. Esteves F.")
 lastName("Mendonca Filho, C.V. Neto, F.C.C.")
 # two or more names, separated by comma: output correct
 lastName("A. Alvarez, A. Zamora & V. Huaraca")
 # two or more names, not separated by comma: output incorrect
 lastName("Karl Emrich & Balduino Rambo")

 # Some problematic (unresolved) examples
 lastName("Maria Da Silva", invert = TRUE, initials = TRUE)

}
\author{
Renato A. F. de Lima & Hans ter Steege
}

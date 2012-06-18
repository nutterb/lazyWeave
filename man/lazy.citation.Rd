\name{lazy.citation}
\alias{lazy.citation}
\title{Cite an R package}
\description{Generates the text for a citation of R or an R package.}

\usage{
lazy.citation(pkg=NULL, author=TRUE, title=TRUE, org=TRUE, 
    address=TRUE, volume=TRUE, year=TRUE, note=TRUE)
}

\arguments{
  \item{pkg}{a character(1) vector giving the name of a package.  If \code{NULL}, a citation for R is produced.}
  \item{author}{Include author name.}
  \item{title}{Include title of package.}
  \item{org}{Include organization.}
  \item{address}{Include address.}
  \item{volume}{Include volume.}
  \item{year}{Include year}
  \item{note}{Include note.}
}
\details{
  Not every option is populated in every package.  Future improvements might include automatic detection of NULL fields, but for now,
  observing the output with all the options set to \code{TRUE} will tell you which ones are empty.
}

\author{Benjamin Nutter \email{nutterb@ccf.org}}

\examples{
lazy.citation()
lazy.citation(pkg="Hmisc", org=FALSE, address=FALSE, volume=FALSE)
}

\keyword{ methods }


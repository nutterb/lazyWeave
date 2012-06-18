\name{lazy.footnote}
\alias{lazy.footnote}
\title{Add a Footnote}
\description{Adds a footnote to the text in a .tex file.}

\usage{
lazy.footnote(text, number=NULL, translate=FALSE)
}

\arguments{
  \item{text}{Text for the footnote.}
  \item{number}{Footnote number.}
  \item{translate}{Determines if \code{\link{latexTranslate}} is applied to \code{text}.}
}
\details{
  Be warned that this is not a perfect function.  If you use special characters that require that \code{latexTranslate} be turned off, 
  you'll also need to turn of \code{latexTranslate} in \code{lazy.write}.  I'm including this as is for ease of use, but it does
  have some kinks to work out.
}

\author{Benjamin Nutter \email{nutterb@ccf.org}}

\examples{
lazy.footnote("Enter a reference to an article in this argument", number=3)
lazy.footnote(lazy.citation(), number=4)
}

\keyword{ methods }


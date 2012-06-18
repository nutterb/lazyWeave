\name{lazy.insert.code}
\alias{lazy.insert.code}
\title{Insert Code to a TeX Document}
\description{Places TeX code from a file into a TeX document.  Text is placed within a verbatim environment}

\usage{
  lazy.insert.code(file, prompt.symbol=options()$prompt, lines)
}

\arguments{
  \item{file}{Filename containing the code to be inserted.}
  \item{prompt.symbol}{Character to be placed at the left most side of the page.  Defaults to the system's
        current prompt symbol.}
  \item{lines}{A vector giving the lines in \code{file} to be inserted into the document.}
}
\details{
  Text is inserted in a verbatim environment to preserve whitespace.  This function is performed 
  better by \code{Sweave} and \code{knitr}.  Those packages will also display any result printed by the
  code.  This function will not display results.
}

\author{Benjamin Nutter \email{nutterb@ccf.org}}

\keyword{ methods }


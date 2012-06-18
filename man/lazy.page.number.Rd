\name{lazy.page.number}
\alias{lazy.page.number}
\title{Change Page Numbering}
\description{Allows page numbering style to be changed.  For instance, from roman numerals in an introduction
  to arabic numbers in the body of a report.}
\usage{
lazy.page.number(num_style = c("arabic", "roman", "Roman", "alph", "Alph"))
}


\arguments{
  \item{num_style}{A character(1) giving the numbering style for the page}
}

\details{
  Each time the page numbering is changed, the page counter is reset to 0 (meaning the next page will be numbered 1).  If the number
  needs to be preserved, this can be done using \code{lazy.counter} with \code{counter="page"}.
}

\author{Benjamin Nutter \email{nutterb@ccf.org}}

\examples{
lazy.page.number("Roman")
}

\keyword{ methods }


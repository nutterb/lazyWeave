\name{map.size}
\alias{map.size}
\title{Convert text size specifications between LaTeX and HTML formats}
\description{The default text size specifications in LaTeX are character strings, but in HTML, the font size is determined by an integer.  In order
    to be able to switch formats in existing code, there needs to be some way to map HTML text size to LaTeX text size, and vice versa.  This is
    managed by \code{map.size} in the \code{lazyWeave} functions so the user won't have to worry about it.}
\usage{
map.size(x, reportFormat=getOption("lazyReportFormat"))
}

\arguments{
  \item{x}{The text size specification to be mapped to the desired format.}
  \item{reportFormat}{The format to which \code{x} should be mapped.}
}
\details{
  Text sizes are mapped according to the following:
  \tabular{ll}{
    LaTeX        \tab HTML\cr
    tiny         \tab 5\cr
    scriptsize   \tab 7\cr
    footnotesize \tab 8\cr
    small        \tab 9\cr
    normalsize   \tab 10\cr
    large        \tab 12\cr
    Large        \tab 14\cr
    LARGE        \tab 18\cr
    huge         \tab 20\cr
    Huge         \tab 24\cr
  }
  
  HTML codes that don't appear in the table are mapped to the next smallest value.  HTML values less than 5 are mapped to "tiny."
}

\author{Benjamin Nutter \email{nutterb@ccf.org}}

\examples{
map.size(10, "latex")
map.size(11, "latex")
map.size(3, "latex")

map.size("tiny", "html")
map.size("Huge", "html")
}

\keyword{ methods }


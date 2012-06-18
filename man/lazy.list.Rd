\name{lazy.list}
\alias{lazy.list}
\title{Lists in LaTeX}
\description{Produce code for lists in LaTeX documents.}
\usage{
lazy.list(item, ordered = TRUE, 
    counter = NULL, counterSet=1, 
    title = NULL, style = "arabic", 
    symbol = "$\\\\bullet$")
}

\arguments{
  \item{item}{A vector with the items to be placed in the list.}
  \item{ordered}{Denotes if the list is ordered or bulleted.}
  \item{counter}{For future inclusion.  Specifies what counter should be used
    for numbering.  Currently not in use.}
  \item{counterSet}{The value to which \code{counter} should be set.  In other words, the number for the first item in the list.}
  \item{title}{A title for the list}
  \item{style}{A character string denoting how the ordered list should be
    numbered.  Options are \code{"arabic", "roman", "Roman", "alph", "Alph"}.}
  \item{symbol}{A symbol for bulleted lists to be used as the bullet.}
}
\details{
  With \code{style}, the options produce the following:
  \tabular{ll}{
    arabic \tab Arabic numbers\cr
    roman  \tab Lower case roman numerals (i, ii, iii, ...)\cr
    Roman  \tab Upper case roman numerals (I, II, III, ...)\cr
    alph   \tab Lower case alphabetic ordering (a, b, c, ...)\cr
    Alph   \tab Upper case alphabetic ordering (A, B, C, ...)\cr
  }
}

\author{Benjamin Nutter \email{nutterb@ccf.org}}

\examples{
lazy.write(
  lazy.file.start(),
  lazy.text("In Boy Scouts, participants can earn several 
      ranks that denote the level of skill and accomplishment 
      they have developed.  These ranks are:"),
  lazy.list(c("Scout", "Tenderfoot", "Second Class", 
      "First Class", "Star", "Life", "Eagle"), style="Alph"),
  lazy.file.end(),
  OutFile="Example 1.tex")
  
unlink("Example 1.tex")
}

\keyword{ methods }


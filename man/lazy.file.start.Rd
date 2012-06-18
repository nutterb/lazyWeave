\name{lazy.file.start}
\alias{lazy.file.start}
\title{Start LaTeX Documents}
\description{Write code to open a new LaTeX document with packages, classes,
  a title, and page settings.}
\usage{
lazy.file.start(docClass = "article", packages = NULL, 
  counters = NULL, layout="", page = "arabic", 
  ligatures = TRUE, title = NULL, author = NULL, date="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{docClass}{a character string giving a valid LaTeX document class. 
    For example, \code{article}, \code{slide}, \code{report}, \code{book}.}
  \item{layout}{LaTeX code for page layout.  Remember to escape backslashes!}
  \item{packages}{A character vector of additional LaTeX packages to use.}
  \item{counters}{A character vector of additional counters to initialize.}
  \item{page}{A character string denoting the page numbering style.  Options
    are \code{"arabic", "roman", "Roman", "alph", "Alph"}.}
  \item{ligatures}{Determines if ligatures are enabled.  See the references for a link about ligatures.}
  \item{title}{A title for the document.}
  \item{author}{Author of the document.}
  \item{date}{Date to be printed on the title page}
}

\details{Titles are only made when either \code{title} or \code{author} are 
  not \code{NULL}.  
  
  Packages automatically included are "xcolor", "graphicx", "colortbl", 
  "soul", and "Sweave".  Any user 
  defined templates made in conjuction with \code{lazyWeave} must include these
  packages in order to use figures and underlined text.

  With \code{page}, the options produce the following:
  \tabular{ll}{
    arabic \tab Arabic numbers\cr
    roman  \tab Lower case roman numerals (i, ii, iii, ...)\cr
    Roman  \tab Upper case roman numerals (I, II, III, ...)\cr
    alph   \tab Lower case alphabetic ordering (a, b, c, ...)\cr
    Alph   \tab Upper case alphabetic ordering (A, B, C, ...)\cr
  }
}

\author{Benjamin Nutter \email{nutterb@ccf.org}}

\references{Ligatures: \url{http://en.wikibooks.org/wiki/LaTeX/Formatting\#Ligatures}}

\examples{
lazy.file.start(docClass="report", 
    packages=c("pslatex", "palatino", "avant"),
    title="Report Name", author="Your Name")
}

\keyword{methods}


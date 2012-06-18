\name{lazy.section}
\alias{lazy.section}
\title{Open Sections in LaTeX Documents}
\description{Provides the code to start a new section.}
\usage{
lazy.section(heading, type=c("section", "sub", "sub2", "chapter"), 
    ordered=FALSE, counter, counterSet=NULL, label=NULL)
}


\arguments{
  \item{heading}{The name of the section or subsection.}
  \item{type}{Type of section.  \code{"section"} for section; 
    \code{"sub"} for subsection; and \code{"sub2"} for subsubsection.}
  \item{ordered}{Logical.  Determines if the section is numbered.}
  \item{counter}{Name of a the counter to be used for this section.  When \code{NULL}, the value defaults to the counter corresponding
    to the type of section.  See \code{\link{lazy.counter}} for more details about counters.}
  \item{counterSet}{Value to which \code{counter} should be set.  In other words, the number for this section (or similar division).}
  \item{label}{The label to be used with \code{lazy.ref}.}
}

\author{Benjamin Nutter \email{nutterb@ccf.org}}

\examples{
lazy.write(
  lazy.file.start(),
  lazy.section("Section A", ordered=TRUE),
  lazy.text("Notice that Section A is numbered"),
  lazy.section("Subsection", type="sub", ordered=FALSE),
  lazy.text("But the subsection is not numbered"),
  lazy.file.end(),
  OutFile="Example-1.tex")
  
unlink("Example-1.tex")
}

\keyword{ methods }


\name{lazy.page.break}
\alias{lazy.page.break}
\title{Start New Page in LaTeX}
\description{Insert code to start a new page in a LaTeX document.}
\usage{
lazy.page.break()
}

\author{Benjamin Nutter \email{nutterb@ccf.org}}

\examples{
lazy.write(
  lazy.file.start(),
  lazy.text("First we type something on the first page"),
  lazy.page.break(),
  lazy.text("Then we type something on the second page"),
  lazy.file.end(),
  OutFile="Example 1.tex")
  
unlink("Example 1.tex")
}
  

\keyword{ methods }


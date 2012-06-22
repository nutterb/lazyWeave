\name{lazy.write}
\alias{lazy.write}
\title{Output LaTeX Code to a File}
\description{Output text and LaTeX code to a file for building.}
\usage{
lazy.write(..., OutFile, append=FALSE, collapse="\n", footnotes=TRUE)
}

\arguments{
  \item{\dots}{Strings, expressions, and statements to be written 
    to a .tex file}
  \item{OutFile}{Filename to which the code should be written.}
  \item{append}{logical.  Indicates if the code should be appended to 
    and existing file.}
  \item{collapse}{Sets the character string that is placed between elements in \code{\dots}}
  \item{footnotes}{logical.  For HTML only, when \code{TRUE}, the footnotes stored in 
     \code{options("lazy.footnotes")} will be appended to the end of the HTML document.}
}
\details{The contents of \code{\dots} will be pasted together.}

\author{Benjamin Nutter \email{nutterb@ccf.org}}


\keyword{ methods }


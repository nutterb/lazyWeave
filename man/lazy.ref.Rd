\name{lazy.ref}
\alias{lazy.label}
\alias{lazy.ref}
\title{Reference Tables, Figures, Sections, and Pages}
\description{Provides the code to label and reference objects that have a label statement with them.}
\usage{
lazy.label(label)
lazy.ref(label, page=FALSE)
}


\arguments{
  \item{label}{A character(1) giving the name of the to be created or referenced.}
  \item{page}{Indicates if the page number on which the label lies should be returned or the object number.}
}

\author{Benjamin Nutter \email{nutterb@ccf.org}}

\examples{
lazy.label("Label1")
lazy.ref("Label1")
lazy.ref("Label1", page=TRUE)
}

\keyword{ methods }


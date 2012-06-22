\name{lazy.landscape}
\alias{lazy.landscape}
\title{Landscape Page Orientation}
\description{Allows the user to turn on or off landscape page orientation.}
\usage{
lazy.landscape(begin=TRUE)
}

\arguments{
  \item{begin}{If \code{TRUE}, begins the landscape environment.  Otherwise, the environment is closed.}
}

\details{
  This function has no effect on HTML files.  The page orientation is determined by the browser.
}

\value{ Returns a string that either begins or ends a landscape environment.}

\author{Benjamin Nutter \email{nutterb@ccf.org}}

\examples{
lazy.landscape()
lazy.landscape(begin=FALSE)
}

\keyword{ methods }


\name{lazy.link}
\alias{lazy.link}
\title{Links to Webpages or External Documents}
\description{While \code{lazy.ref} provides the option of linking to areas within a document, \code{lazy.link} provides the option of linking
  to areas outside of the document.  Web pages are perhaps the most obvious example, but links could also go to files on a directory.}
\usage{
lazy.link(url, text, web=TRUE, secure=FALSE)
}


\arguments{
  \item{url}{A character(1) giving the URL for the link or a file path.}
  \item{text}{The text to be highlighted as the link.  If this is missing, \code{url} is used.}
  \item{web}{When \code{TRUE}, \code{"http://"} is added to \code{url}, (if not already present), to ensure a link to the web.  For 
    files on a local dis, set this to \code{FALSE}.}
  \item{secure}{Should the link be to a secure site "https://".}
}

\author{Benjamin Nutter \email{nutterb@ccf.org}}

\examples{
lazy.label("Label1")
lazy.ref("Label1")
lazy.ref("Label1", page=TRUE)
}

\keyword{ methods }


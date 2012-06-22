\name{lazy.verbatim}
\alias{lazy.verbatim.start}
\alias{lazy.verbatim.end}
\title{Verbatim Environments}
\description{Text within a verbatim block appears exactly as type, including whitespace.
  This is useful when inserting code into a document.}
\usage{
lazy.verbatim.start()
lazy.verbatim.end()
}


\details{
  A verbatim block takes any text entered and typsets it exactly as it was entered.  
  White space is preserved and the font changes.  This is typically done to display
  code, since the whitespace may preserve readability.
  
  For HTML documents, this is done by opening a "<p ...>" tag with font family "monospace" and 
  font "courier".  These are applicable until \code{lazy.verbatim.end} is called and the 
  "</p>" tag is placed, closing the verbatim environment.
  
  It should be noted that HTML code in this forced environment will still not render whitespace as in the
  LaTeX verbatim environment.  This can be enforced by running the text in the environment through a function
  like \code{gsub(" ", "&nbsp ", [text])} (\code{&nbsp} is the HTML character for a non-breaking space).
}

\author{Benjamin Nutter \email{nutterb@ccf.org}}


\keyword{methods}


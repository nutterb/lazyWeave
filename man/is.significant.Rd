\name{is.significant}
\alias{is.significant}
\title{Determine if a p-value will denote a result as significant.}

\description{
	Makes a decisions about the significance of a test by evaluating the p-value.
}

\usage{ is.significant(pvalue,alpha=.05) }

\arguments{
 \item{pvalue}{p-value of a test.  May be a number, or may be a character string, such as one produced by
	\code{pvalue.QHS}.}
 \item{alpha}{Significance level for the test.  \code{is.significant} returns \code{TRUE} when 
	\code{pvalue <= alpha}.}
}

\details{
      This function was built with the intent of using it to identify rows in descriptive tables (such as
	\code{cattable} and \code{conttable}) with significant results.  These rows could then be highlighted
	using bold print automatically.  This might prove useful for large tables.

	Whereas it was intended to be used in concert with \code{pvalue.QHS}, \code{is.significant} will peel 
	off and '>' or '<' symbols in a p-value.
}

\author{
	Benjamin Nutter \email{nutterb@ccf.org}\cr
	\tabular{ll}{
	  Created:  \tab April 2008\cr
	}
}

\examples{
	is.significant(wilcox.test(mpg~am,data=mtcars)$p.value)
 
	x=pvalue.QHS(wilcox.test(mpg~am,data=mtcars)$p.value)
	is.significant(x)

	is.significant(">0.99")

}
\keyword{methods}

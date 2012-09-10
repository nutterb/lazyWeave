\name{pvalue.QHS}
\alias{pvalue.QHS}
\title{Format p-values}

\description{Prepares p-values for printing according to the Alpha-Beta team standards.}

\usage{ pvalue.QHS(value) }

\arguments{
 \item{value}{A single number.  0 value < 1.}
}

\details{
	Cleveland Clinic Foundation's Alpha-Beta team uses the following format for p-values when
	presenting results to researchers:
	\tabular{ll}{
	 	\tab	values greater than 0.99 are reported as '> 0.99'\cr
	 	\tab values between 0.99 and 0.10 are reported with two significant digits\cr
	 	\tab values between 0.001 and 0.10 are reported with three significant digits\cr
	 	\tab values less than 0.001 are reported as '< 0.001'.
	}
}

\author{
	Benjamin Nutter \email{nutterb@ccf.org}\cr
	\tabular{ll}{
        Created: \tab October 2007
	}
}

\examples{
  pvalue.QHS(1.0)
  pvalue.QHS(runif(1,0,1))

  ## The dataset 'women' is in the package 'datasets'
  pvalue.QHS( t.test(women$height)$p.value )
}

\keyword{methods}

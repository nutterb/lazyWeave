\name{univ}
\alias{univ}

\title{Univariate Table}
\description{Similar to the QHS SAS macro, provides a numerical simple
  summary of numerical variables.}

\usage{
univ(data, vars, byVar, alpha=0.05)
}

\arguments{
  \item{data}{A \code{ccf.df} or \code{data.frame} containing the variables in
    \code{vars} and \code{byVar}.}
  \item{vars}{Character vector naming the variables in \code{data} to be
    summarized.}
  \item{byVar}{A categorical variables giving groups by which statistics should be computed.}
  \item{alpha}{significance level for determining the confidence limits.}
}

\details{
	Statistics available in univ, and the calls to get them are:
	\enumerate{
        \item{\code{n}}{number of non-missing values.}
        \item{\code{missing}}{number of missing values}
        \item{\code{mean}}{arithmetic mean}
        \item{\code{median}}{median value}
        \item{\code{sd}}{standard deviation}
        \item{\code{lcl}}{lower confidence limit}
        \item{\code{ucl}}{upper confidence limit}
        \item{\code{min}}{minimum value}
        \item{\code{max}}{maximum value}
        \item{\code{p25}}{25th percentile}
        \item{\code{p75}}{75th percentile}
        \item{\code{cv}}{coefficient of variation}
	}

  \code{univ} does not perform any kind of inference on the varaibles supplied in the argument.  
	It returns only summary statistics.  If comparisons are desired, try using \code{conttable}.  
	Confidence limits are determined using the t-distribution.
}

\seealso{
  \code{\link{write.univ}}
}

\author{
	Benjamin Nutter (Modified)\email{nutterb@ccf.org}}
	
\keyword{ methods }

\examples{
#Read in the delivery dataset from the CCFmisc library
require(Hmisc)

data(Delivery)

#use Hmisc library to labeling variables in univariate tables
label(Delivery$maternal.age) <- "Maternal Age"
label(Delivery$ga.weeks) <- "Gestation weeks"
label(Delivery$wt.gram) <- "Weight (g)"


#a univariate table of the variables maternal age,
#ga.weeks and wt.grams.  The object resulting
#from univ() can be used in other functions to create html or
#LaTeX tables.

uni <- univ(Delivery,
            vars=c("maternal.age", "ga.weeks", "wt.gram"))

#a univariate table of the variables maternal age,
#ga.weeks and wt.grams by delivery.type.  The object resulting
#from univ() can be used in other functions to create html or
#LaTeX tables.

deliv.uni <- univ(Delivery,
	vars=c("maternal.age", "ga.weeks", "wt.gram"),
	byVar="delivery.type")

#if you want to take advantage of the confidence interval
#output from univ() different alpha levels can be set
#by the alpha= argument.

deliv_99.uni <- univ(Delivery,
	vars=c("maternal.age", "ga.weeks", "wt.gram"),
	byVar="delivery.type",
	alpha=0.01)
}
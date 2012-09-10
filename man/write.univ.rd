\name{write.univ}
\alias{write.univ}

\title{Write Univariate Table to a File}
\description{Write the output of \code{univ} to a file.}

\usage{
write.univ(x, round=1, 
    Factor=TRUE, Group=FALSE, N=TRUE, Missing=FALSE,
    Mean=TRUE, SD=TRUE, LCL=FALSE, UCL=FALSE, Min=TRUE,
    P25=TRUE, Median=TRUE, P75=TRUE, Max=TRUE, CV=FALSE, ...)
}

\arguments{
  \item{x}{An object of type \code{univ}.}
  \item{round}{Number of significant digits to be printed.}
  \item{Factor}{Determines if the Factor (variable) description is printed.}
  \item{Group}{Determines if the Group is printed}
  \item{N}{Determines if the number of non missing values is printed}
  \item{Missing}{Determines if the number of missing values is printed}
  \item{Mean}{Determines if the mean is printed}
  \item{SD}{Determines if the standard deviation is printed}
  \item{LCL}{Determines if the lower confidence limit is printed}
  \item{UCL}{Determines if the upper confidence limit is printed}
  \item{Min}{Determines if the minimum value is printed}
  \item{P25}{Determines if the 25th percentile is printed}
  \item{Median}{Determines if the median value is printed}
  \item{P75}{Determines if the 75th percentile is printed}
  \item{Max}{Determines if the maximum value is printed}
  \item{CV}{Determines if the coefficient of variation is printed}
  \item{...}{additional arguments to be passed to \code{lazy.matrix} and
    \code{html.matrix}.}
}

\author{
	Benjamin Nutter (Maintenance)\email{nutterb@ccf.org}}
	
\keyword{ methods }

\examples{

#output will be written to the working directory
getwd()

#write.univ function must be written to either a LaTeX
#or HTML file.  HTML format is through the lazyHTML package.
require(lazyWeave)
options(lazyReportFormat="html")

#Delivery dataset from CCFmisc library
data(Delivery)

#label the variables that will be used
label(Delivery$maternal.age) <- "Maternal Age"
label(Delivery$ga.weeks) <- "Gestation weeks"
label(Delivery$wt.gram) <- "Weight (g)"

#summaries of the continuous variables
#maternal.age, ga.weeks and wt.gram in the 
#Delivery dataset.
deliv.uni <- univ(Delivery,
	vars=c("maternal.age", "ga.weeks", "wt.gram")
)

#summaries of continuous variables
#by level of delivery.type
delivBy.uni <- univ(Delivery,
	vars=c("maternal.age", "ga.weeks", "wt.gram"),
	byVar="delivery.type"
)

#to write univ based table to an HTML file enclose the
#write.univ() in the html_write function as below.
#see documentation for other options.

#To print byVariable group names in the table, 
#set the Group=T flag in the write.univ() function.

\dontrun{
lazy.write(
	lazy.file.start(),
	write.univ(deliv.uni),
	write.univ(delivBy.uni, Group=TRUE),
	lazy.file.end(),
	OutFile="ExampleFile.html"
)

unlink("ExampleFile.html")
}
}
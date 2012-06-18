\name{lazy.counter}
\alias{lazy.counter}
\title{Create and Manage Counters for LaTeX Documents}
\description{Provides the code to create, manipulate, or extract values of counters in a LaTeX document.}
\usage{
lazy.counter(counter, value, oldcounter, 
    fn=c("new", "addto", "set", "use", "value"))
}


\arguments{
  \item{counter}{A character(1) giving the name of the counter to be created and/or manipulated}
  \item{value}{An integer.  For \code{fn="addto"}, it is the value by which the counter is to be increased.  For 
      \code{fn="set"}, it is the value to which the counter should be set.}
  \item{oldcounter}{character(1).  An optional argument for \code{fn="new"}.  It must be a previously named counter.  If 
      present, the new counter will be reset whenever \code{oldcounter} is incremented.}
  \item{fn}{Selects the LaTeX function to be used.}
}

\details{
Counters are used to provide table, figure, and section numbers.  After each use of each command, the counter is incremented so that it
can be referred to in later uses.  New counters may be defined by users, but several LaTeX environments have default counters that do
not need to be defined.  These are \code{part, chapter, section, subsection, subsubsection, paragraph, subparagraph, page, equation,
figure, table, footnote, mpfootnote}.  Any of these may be manipulated by \code{lazyWeave}.

Referring to and manipulating counters is done using \code{lazy.counter}.  Different actions are achieved by changing the \code{fn}
argument.  

\code{fn="new"} creates a new counter with name \code{counter}.

\code{fn="addto"} adds \code{value} to the current value of \code{counter}.

\code{fn="set"} changes the current value of \code{counter} to \code{value}.

\code{fn="use"} designates \code{counter} for use in the current environment.

\code{fn="value"} returns the value of \code{counter}.  This value isn't printed, but can be useful for doing arithmetic with 
counter values.
}

\author{Benjamin Nutter \email{nutterb@ccf.org}}

\examples{
lazy.counter("myCounter")
lazy.counter("myCounter", value=3, fn="set")
lazy.counter("myCounter", value=2, fn="addto")
lazy.counter("myCounter", fn="use")
lazy.counter("myCounter", fn="value")

lazy.counter("table", fn="use")
}

\keyword{ methods }


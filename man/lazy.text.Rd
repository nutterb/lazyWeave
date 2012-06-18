\name{lazy.text}
\alias{lazy.text}
\title{Paragraphs in LaTeX}
\description{Write paragraphs in LaTeX code.}
\usage{
lazy.text(..., title = NULL, 
    size = "normalsize", align = "left", 
    italic = FALSE, bold = FALSE, underline = FALSE, 
    sep = "", translate=TRUE)
}
\arguments{
  \item{\dots}{Text and other objects to be included in the paragraph.}
  \item{title}{A title for the paragraph}
  \item{size}{Text size of the paragraph.  See "Details" for options.}
  \item{align}{alignment of the paragraph.  Options are \code{"left", 
    "center", "right"}.}
  \item{italic}{Logical.  Indicates if the text should be italicized.}
  \item{bold}{Logical.  Indicates if the text shoudl be bolded.}
  \item{underline}{Logical.  Indicates if the text should be underlined.}
  \item{sep}{Character.  Denotes the separation string for the items in 
    \code{...} when they are pasted together.}
  \item{translate}{Toggles if inputs in \code{x} should be passed through 
    \code{latexTranslate}.  This should be set to \code{FALSE} if writing
    custom code.}
}

\details{
  Options for text size are
  \tabular{ll}{
    tiny \tab smallest \cr
    scriptsize \tab \cr
    footnotesize \tab \cr
    small \tab \cr
    normalsize \tab \cr
    large \tab \cr
    Large \tab \cr
    LARGE \tab \cr
    huge \tab \cr
    Huge \tab BIGGEST\cr
  }
  
  Additional formatting may be applied using commands such as \code{textbf\{\}} for bold text, \code{emph\{\}} for italics, and
  \code{ul\{\}} for underlined text (assuming the \code{soul} package is available).   
}

\author{Benjamin Nutter \email{nutterb@ccf.org}}

\examples{
lazy.write(
  lazy.file.start(),
  lazy.text("Typically we want our paragraphs to be left 
    justified.  This is often what we expect to see when reading."),
  lazy.text("However, we may also have occasions where we would 
    like to center our text.  It's one of many ways we can make the 
    words stand out on the page", align="center"),
  lazy.text("A more traditional way to make the text stand out might be
    to use bold text or italics, such as these", bold=TRUE, italic=TRUE),
  lazy.file.end(),
  OutFile="Example 1.tex")
  
unlink("Example 1.tex")
}

\keyword{ methods }


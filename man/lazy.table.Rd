\name{lazy.table}
\alias{lazy.table}
\title{Tables in LaTeX}
\description{Generate code for custom LaTeX tables}
\usage{
lazy.table(x, align = "center", justify="center", placement="!H", 
  cspan = 1, cborder = NULL, cwidth = NULL, 
  rborder = NULL, rbspan = NULL, rheight = NULL, 
  rcol = NULL, usecol = "lightgray", 
  caption = NULL, footnote = NULL, textsize="\\\\normalsize",
  counter, counterSet=NULL, label=NULL, 
  open = TRUE, close = TRUE, translate=TRUE)
}

\arguments{
  \item{x}{Matrix to be put into the table.  Other objects are coerced to
    matrices.  Vectors are coerced to a row vector.}
  \item{align}{Character vector or string giving the alignment for each 
    column.  Options are \code{"left", "center", "right"}.}
  \item{justify}{Character string giving the alignment for the table on the
    page.  Options are \code{"left", "center", "right"}.}
  \item{placement}{Controls the placement of the figure.  Options are
    \code{"ht", "t", "b", "p", "H"} and can be supplemented with 
    \code{"!"}. See "Details" for more explanation.}
  \item{cspan}{A vector specifying how many columns of the table each 
    column of \code{x} should span.  This is used when using successive
    calls to \code{latex.table} to build tables with complex headers.}
  \item{cborder}{A vector denoting vertical border positions.  Borders are
    placed to the right of the given columns.  See "Details".}
  \item{cwidth}{Currently not in use.  Will be used to specify the width
    of each column.}
  \item{rborder}{A vector denoting horizontal border positions.  Borders 
    are placed at the bottom of the given rows.  See "Details".}
  \item{rbspan}{A vector or list giving the start and stop positions of the
    horizontal borders.  Use a list when borders should go from columns 
    1 - 3 and 5 - 7, but not at column 4.}
  \item{rheight}{Currently not in use.  Will be used to specify the width
    of each row.}
  \item{rcol}{A vector denoting which rows should be colored.}
  \item{usecol}{A character vector or string giving the color to be used for
    the rows in \code{rcol}.  The color must be a recognized LaTeX color.}
  \item{caption}{Caption for the table.  Currently, captions are placed
    above tables.}
  \item{footnote}{Additional footnotes to be placed below tables.}
  \item{textsize}{A character string giving the text size for the table.  This
    must be valid LaTeX code.  Remember to use two backslashes.}
  \item{counter}{The name of the counter to be used for this table}
  \item{counterSet}{The value to which \code{counter} should be set.  In other words, the number of this table.}
  \item{label}{The label to be used by \code{lazy.ref}.}
  \item{open}{Logical.  Indicates if a new table environment should be 
    opened.}
  \item{close}{Logical.  Indicates if the current table environment should
    be closed.}
  \item{translate}{Toggles if inputs in \code{x} should be passed through 
    \code{latexTranslate}.  This should be set to \code{FALSE} if writing
    custom code.}
}

\details{
  \code{cborder} (or column border) will create vertical borders in the table.
  Borders are placed on the right side of the specified columns.  If a 
  border is needed on the far left side of the table, use 0.
  
  \code{rborder} (or row border) acts similarly, but for rows.  Borders are
  placed under the specified rows.  Use 0 if a line is needed at the top of 
  a table.
  
  Multiple calls to \code{latex.table} may be used to make complex tables.
  For instance, a header may be desired with group names that span over 
  three summary values (See example 2).  In these instances, \emph{it is the
  responsibility of the user to make sure the number of columns in each 
  call is the same as in the other calls.}  There is no way in \code{lazyWeave}
  to check the column consistency of tables.
  
  \code{placement} options are used as follows:
    \tabular{ll}{
      ht \tab Place the float here, i.e., 
              approximately at the same point it occurs \cr
      t  \tab Position at the top of the page\cr
      b  \tab Position at the bottom of the page \cr
      p  \tab Put on a special page for floats only \cr
      H  \tab Places the float at precisely the location in the LaTeX code. 
              Requires the float package\cr
    }
  The \code{"!"} may be used after any of these in order to override 
  LaTeX float rules and force your selection.  More can be learned by 
  reading about floats in a LaTeX manual.
}

\author{Benjamin Nutter \email{nutterb@ccf.org}}

\examples{
#*** Example 1: Simple Table
tab.text <- lazy.table(mtcars[, c(1, 2, 4, 6)], align="right", 
    cborder=c(0, 4), rborder=c(0, nrow(mtcars)))
    
lazy.write(
  lazy.file.start(),
  tab.text,
  lazy.file.end(),
  OutFile="Example 1.tex")
  
unlink("Example 1.tex")
  
#*** Example 2: Complex Table
person <- c("Rachel", "John", "Elizabeth", "George", "Ryan")
veg <- c("", "x", "x", "", "x")
meat <- c("x", "", "", "x", "")
soup <- c("x", "", "x", "x", "")
salad <- c("", "x", "", "", "x")
ice <- c("", "x", "x", "x", "")
cake <- c("x", "", "", "", "x")

dinner <- cbind(person, veg, meat, soup, salad, ice, cake)
colnames(dinner) <- c("Name", "Vegetarian", "Meat", 
    "Soup", "Salad", "Ice Cream", "Cake")
                      
tab1 <- lazy.table(c("", "Entree", "Side", "Dessert"), 
    cspan=c(1, 2, 2, 2),
    rborder=c(0, 0, 1), rbspan=2:7, 
    caption="Dinner Orders", close=FALSE)
tab2 <- lazy.table(colnames(dinner), 
    align=c("left", rep("center", 6)),
    cborder=c(3, 5),
    open=FALSE, close=FALSE)
tab3 <- lazy.table(dinner, 
    align=c("left", rep("center", 6)),
    cborder=c(1, 3, 5),
    rborder=c(0, nrow(dinner)), open=FALSE)
                    
lazy.write(
  lazy.file.start(),
  tab1, tab2, tab3,
  lazy.file.end(),
  OutFile="Example 2.tex")
  
unlink("Example 2.tex")
}

\keyword{ methods }


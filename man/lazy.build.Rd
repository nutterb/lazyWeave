\name{lazy.build}
\alias{lazy.build}
\title{Finalize Reports in LaTeX and HTML Formats}
\description{Calls \code{texi2dvi} to convert tex code to a pdf.

HTML documents may references figures and images from several different locations. These images and figures will only be viewable by others if they have access to the paths on which they are stored. For instance, if an HTML report is e-mailed to a client, the client may not be able to view the images because if those files are not stored locally or on a server to which the client has access. html_zip searches the HTML code for links to images, collects the files, and gathers them all into a zip file with the HTML document. Image references are changed, if necessary, to reflect that images can be found in the same folder as the HTML file. In this manner, complete reports can be sent to clients with all of the images and figures viewable.}

\usage{
lazy.build(filename, pdf.zip=NULL, quiet=TRUE, clean=TRUE, replace=TRUE, ...)
}

\arguments{
  \item{filename}{Character string giving the location of the TeX or HTML.}
  \item{pdf.zip}{Optional character vector in which to store the built file.  If
    \code{NULL}, the pdf/zip file will be placed in the same directory as \code{filename}.}
  \item{quiet}{Suppresses the output from \code{texi2dvi}.  When the build
    is successful, it's just a bunch of noise.  When the build is not
    successful, it may give useful error messages.  This argument only applies when 
    \code{options("lazyReportFormat")} is set to \code{"latex"}}
  \item{clean}{Passed to \code{tools::texi2dvi}.   If TRUE, all auxiliary 
    files are removed (texi2dvi command line option --clean). May not work on some platforms.
    This argument only applies when 
    \code{options("lazyReportFormat")} is set to \code{"latex"}}
  \item{replace}{Should the current build replace a previous build.  This argument only applies when 
    \code{options("lazyReportFormat")} is set to \code{"latex"}}
  \item{...}{Additional arguments to be passed to \code{texi2dvi} or \code{zip}.}
}
\details{
  Building with \code{texi2dvi} seems to recognize jpeg formats for figures,
  but not .eps or .ps.  I'm certain this is because of something I'm doing
  wrong and not any limitation of other software.  If anyone would care to 
  point out how this could be changed, it'd be greatly appreciated.
  
  \code{texi2dvi} does accept pdf's very well.  My recommendation is to use the \code{pdf} function for graphics.
    
  \code{texi2dvi} requires a full path specification in order to build the
  pdf.  If \code{tex} is given as "file.tex", the working directory will
  need to be pasted to it to create, for example, "C:/..../file.tex".  Sadly,
  I haven't figured out a way to automate this decision in a platform 
  independent manner.  For now, the user will need to make this explicit.)
  
  Depending on the working directory (for Windows users), the user may encounter the error 
  "texi2dvi.exe: Windows API error 5: Access is denied." when trying
  to build documents.  This happens when the working directory is write protected.  It is advisable to change the working directory to 
  something not write protected.  This can be done permanently by right clicking on the R shortcut icon, selecting properties, and changing
  the directory in the "Start in:" box.
  
  When there is a problem with the LaTeX code, the document will not build properly.  When this happens, several lines may appear in the
  R Console that give a nearly unintelligible explanation for the error.  I highly recommend first checking that you've included 
  \code{lazy.file.close()} at the end of your document.  This is easily my most frequent error.
  
  In cases where the R Console prints something like what is given below (sometimes the output will be very, very long).  
  The primary piece of information here is that there was an error on lines 72 and 73.  By opening the .tex file, you can determine 
  what \code{lazyWeave} function generated the problematic code.  Look at the top of the block for those lines and the function call is
  given as a comment in the LaTeX code.
  
  \code{SampleFile.tex:72: [perhaps some text, perhaps not]}\cr
  \code{SampleFile.tex:73: }  
  
  One common problem with \code{lazy.table} is using a \code{cwidth} argument that has a length not equal to the number of columns in 
  the table.
  
  
  
  For HTML files, to identify figure files, html_zip searches code for lines containing "<img src='". In these lines, the content between "<img src=" and "' height" is assumed to be the filename. If any other attribute is defined after src and before height, the function will fail. This won't be a problem when all figures references are created by html_figure, but user defined references are likely to create problems if this issue is not carefully observed.

In some systems, the contents of a .zip folder are only visible after a local copy has been made by opening a file. The consequence of this behavior is that images in a zip folder are not viewable from the HTML docment until all of the images are opened independently. In order to alert the user of this behavior, an additional file titled "00_Troubleshooting.txt" is placed in the zip folder with an explanation. Unzipping folders will allow the user to view the images from the HTML file as expected.
  

}

\author{Benjamin Nutter \email{nutterb@ccf.org}}

\keyword{ methods }


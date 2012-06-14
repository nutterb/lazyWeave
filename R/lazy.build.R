lazy.build <- function(tex, pdf=NULL, quiet=TRUE, clean=TRUE, replace=TRUE, ...){
  path.comp <- unlist(strsplit(tex, .Platform$file.sep))
  path <- paste(path.comp[-length(path.comp)], collapse=.Platform$file.sep)
  if (path == "") path <- getwd()

  outfile <- gsub("[.]tex", ".pdf", path.comp[length(path.comp)])
  outfile <- paste(getwd(), outfile, sep=.Platform$file.sep)
                                  
  if (is.null(pdf)) pdf <- gsub("[.]tex", ".pdf", tex)
  path.pdf <- unlist(strsplit(pdf, .Platform$file.sep))
  path.pdf <- paste(path.pdf[-length(path.pdf)], collapse=.Platform$file.sep)
  if (path.pdf == "") path.pdf <- getwd()
    
  #*** Used when I was using system() to build the pdf.
  #quiet <- if (quiet) "--quiet" else ""
  #system(paste("texi2dvi --pdf --clean", quiet, shQuote(tex)))
  
  tools::texi2dvi(tex, pdf=TRUE, quiet=quiet, clean=clean, ...)
  
  if (!(pdf %in% path)){
    if(path.pdf != getwd()){
      file.copy(outfile, pdf, overwrite=replace)
      file.remove(outfile)
    }
  }
}


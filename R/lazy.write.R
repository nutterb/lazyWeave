lazy.write <-
function(..., OutFile, append=FALSE, collapse="\n", footnotes=TRUE){
  
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html")) stop("option(\"lazyReportFormat\") must be either 'latex' or 'html'")

  #*** Stop Function if no OutFile is given
  if (missing(OutFile))
    stop("'OutFile' must be explicitly specfied using 'OutFile=[filename]'")

  #*** As awful as this sounds, I have no idea what this does.  It is a relic
  #*** from html.write in my earlier version of CCFmisc, but I never documented
  #*** why I needed this.  I think it has something to do with making vectors
  #*** appear across a row instead of as a column.  But until the function 
  #*** shows behavior contrary to what I desire, I don't want to bother 
  #*** investigating what this is doing.
  f <- function(x){
    x <- ifelse(!is.null(dim(x)) || is.vector(x), paste(t(x), collapse=" "), x)
    return(x)
  }


  #*** Combine all the code into one string for exporting to the file.
  code <- list(...)
  code <- lapply(code, f)
  code <- paste(code, collapse = collapse)
  
  
  if (reportFormat == "html" & footnotes & !is.null(getOption("html.footnotes"))){
    code <- gsub("</html>", paste("\n\n\n", getOption("html.footnotes"), "\n</html>"), code)
    options(html.footnotes = NULL)
  }
  
  write(code, OutFile, append = append)
}


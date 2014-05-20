lazy.write <-
function(..., OutFile, append=FALSE, collapse="\n", footnotes=TRUE){
  
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html")) stop("option(\"lazyReportFormat\") must be either 'latex' or 'html'")

  #*** Stop Function if no OutFile is given
  if (missing(OutFile))
    stop("'OutFile' must be explicitly specfied using 'OutFile=[filename]'")
  
  file <- unlist(strsplit(OutFile, "[.]"))
  file.ext <- tail(file, 1)
  if (reportFormat == "latex" && file.ext %in% c("html", "htm")) 
    OutFile <- paste(file[-length(file)], ".tex", sep="")
  if (reportFormat == "html" && file.ext == "tex")
    OutFile <- paste(file[-length(file)], "html", sep=".")
  

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
  
  
  if (reportFormat == "html" & footnotes & !is.null(get("HTML.FOOTNOTES", envir=htmlCounters))){
    code <- gsub("</html>", paste("\n\n\n", get("HTML.FOOTNOTES", envir=htmlCounters), "\n</html>"), code)
    assign("HTML.FOOTNOTES", NULL, envir=htmlCounters)
  }
  
  write(code, OutFile, append = append)
}


lazy.citation <- function(pkg=NULL, author=TRUE, title=TRUE, org=TRUE, 
                          address=TRUE, volume=TRUE, year=TRUE, note=TRUE){
  
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html", "markdown")) stop("option(\"lazyReportFormat\") must be either 'latex', 'html', or 'markdown'")
  

  #*** Construct the comment with the function call
  comment.char <- if (reportFormat == "latex") c("%%", "")
                   else if (reportFormat == "html") c("<!--", "-->")
  
  fncall <- paste(comment.char[1], paste(deparse(match.call()), collapse=" "), comment.char[2], "\n")
  

  #*** get the right left quote characters for the report format
  quote.string <- if (reportFormat == "latex") "``"              
  else if (reportFormat %in% c("html", "markdown")) "\""
  
  #*** Construct the citation
  cit <- if (is.null(pkg)) citation() else citation(pkg)

  paste( if (author) paste(paste(cit$author, collapse=", "), ", ", sep="") else "",
         if (title)  paste(quote.string, cit$title, ",\" ", sep="") else "",
         if (org)    paste(cit$organization, ", ", sep="") else "",
         if (address) paste(cit$address, ", ", sep="") else "",
         if (volume)  paste("Vol. ", cit$volume, ", ", sep="") else "",
         if (year)    paste("(", cit$year, ") ", sep="") else "",
         if (note)    paste(cit$note, ".", sep="") else "", sep="")
}


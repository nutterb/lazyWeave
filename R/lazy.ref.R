lazy.ref <- function(label, text, page=FALSE){ 
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html")) stop("option(\"lazyReportFormat\") must be either 'latex' or 'html'")
  
  #*** Construct the comment with the function call
  comment.char <- if (reportFormat == "latex") c("%%", "")
  else if (reportFormat == "html") c("<!--", "-->")
  fncall <- paste(comment.char[1], paste(deparse(match.call()), collapse=" "), comment.char[2], "\n")
  
  if (reportFormat == "latex"){
    code <- if (page)  paste(fncall, "\\pageref{", label, "}", sep="") else paste(fncall, "\\ref{", label, "}", sep="")
  }
  
  if (reportFormat == "html"){
    if (missing(text)) text <- "(link)"
    code <- paste(fncall, "<a href='#", label, "'> ", text, " </a>\n\n", sep="")
  }
  
  return(code)
}

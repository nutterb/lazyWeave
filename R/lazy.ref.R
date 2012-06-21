lazy.ref <- function(label, text, page=FALSE, link=TRUE){ 
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html")) stop("option(\"lazyReportFormat\") must be either 'latex' or 'html'")
  
  #*** Construct the comment with the function call
  comment.char <- if (reportFormat == "latex") c("%%", "")
  else if (reportFormat == "html") c("<!--", "-->")
  fncall <- paste(comment.char[1], paste(deparse(match.call()), collapse=" "), comment.char[2], "\n")

  #*** latex
  if (reportFormat == "latex"){
    code <- if (page)  paste("\\pageref{", label, "}", sep="") else paste("\\ref{", label, "}", sep="")
    if (link) code <- paste("\\hyperref[", label, "]{", code, "}", sep="")
  }
  
  if (reportFormat == "html"){
    if (missing(text)) text <- "(link)"
    code <- paste("<a href='#", label, "'> ", text, " </a>\n\n", sep="")
  }
  
  return(code)
}

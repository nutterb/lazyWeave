lazy.link <- function(url, text, web=TRUE, secure=FALSE){
  
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html", "markdown")) stop("option(\"lazyReportFormat\") must be either 'latex', 'html', or 'markdown'")
  
  #*** Construct the comment with the function call
  comment.char <- if (reportFormat == "latex") c("%%", "")
  else if (reportFormat == "html") c("<!--", "-->")
  
  fncall <- paste(comment.char[1], paste(deparse(match.call()), collapse=" "), comment.char[2], "\n")
  
  #*** append http (or https) if not on url
  if (web){
    first.seven <- substr(url, 1, if(secure) 8 else 7)
    if (!first.seven %in% if (secure) "https://" else "http://") 
      url <- paste(if (secure) "https://" else "http://", url, sep="")
  }
  
  if (reportFormat == "latex"){
    code <- if (missing(text)) paste("\\url{", url, "}\n", sep="") else paste("\\href{", url, "}{", text, "}\n", sep="")
  }
  
  if (reportFormat == "html"){
    code <- paste("<a href='", url, "'>", if (missing(text)) url else text, " </a>\n\n", sep="")
  }
  
  if (reportFormat == "markdown"){
    fncall <- ""
    code <- paste("[", text, "](", url, ")", sep="")
  }
 
  return(paste(fncall, code))
}
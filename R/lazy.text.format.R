lazy.text.format <- function(text, italic=FALSE, bold=FALSE, underline=FALSE, translate=TRUE){

  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html", "markdown")) stop("option(\"lazyReportFormat\") must be either 'latex', 'html', or 'markdown'")
  
  if (reportFormat == "latex"){
    if (translate) text <- latexTranslate(text)
  
    if (underline) text <- paste("\\ul{", text, "}", sep="")
    if (bold)      text <- paste("\\textbf{", text, "}", sep="")
    if (italic)    text <- paste("\\emph{", text, "}", sep="")
  }
  
  if (reportFormat == "html"){
    if (underline) text <- paste("<ul>", text, "</ul>")
    if (italic) text <- paste("<i>", text, "</i>")
    if (bold) text <- paste("<b>", text, "</b>")  
  }
  
  if (reportFormat == "markdown"){
    if (italic) text <- paste("_", text, "_")
    if (bold) text <- paste("**", bold, "**")
  }
  
  return(text)
}

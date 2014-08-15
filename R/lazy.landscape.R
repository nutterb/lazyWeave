lazy.landscape <- function(begin=TRUE){
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html", "markdown")) stop("option(\"lazyReportFormat\") must be either 'latex', 'html', or 'markdown'")

  if (reportFormat == "latex") if (begin) return("\\begin{landscape}\n") else return("\\end{landscape}")

  if (reportFormat == "html") return("")
  
  if (reportFormat == "markdown"){
    warning("Landscape orientation is not available for markdown")
    return("")
  }
}
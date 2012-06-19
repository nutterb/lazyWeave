lazy.verbatim.end <- function(){
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html")) stop("option(\"lazyReportFormat\") must be either 'latex' or 'html'")
  
  if (reportFormat == "latex") return("\\end{verbatim}")
  
  if (reportFormat == "html") return("</p>")
}

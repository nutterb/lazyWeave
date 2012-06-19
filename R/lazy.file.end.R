lazy.file.end <- function(){

  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html")) stop("option(\"lazyReportFormat\") must be either 'latex' or 'html'")
  
  if (reportFormat == "latex") return("\n\n\\end{document}")
  else if (reportFormat == "html") return("\n\n</html>\n")
}


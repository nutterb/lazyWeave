lazy.landscape <- function(begin=TRUE) 
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html")) stop("option(\"lazyReportFormat\") must be either 'latex' or 'html'")

  if (reportFormat == "latex") if (begin) return("\\begin{landscape}\n") else return("\\end{landscape}")

  if (reportFormat == "html") return("")
}
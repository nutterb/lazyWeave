lazy.page.break <- function(){
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html")) stop("option(\"lazyReportFormat\") must be either 'latex' or 'html'")
  
  if (reportFormat == "latex") return("%% lazy.page.break()\n\\newpage")
  if (reportFormat == "html") return("<!--html_page_break()-->\n<div style='page-break-before:always'></div>")
}

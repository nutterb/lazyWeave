lazy.verbatim.start <- function(){
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html")) stop("option(\"lazyReportFormat\") must be either 'latex' or 'html'")
  
  
  if (reportFormat == "latex") return("\\begin{verbatim}")

  #*** opens a paragraph, but does not end it.  Text inserted between lazy.verbatim.start and lazy.verbatim.end
  #*** will thus appear in monospace courier font
  if (reportFormat == "html") return("<p style='font-family:monospace, courier; font-size:11pt; text-align:left; font-style:none;
  font-weight:none; text-decoration:none;'>")
  
}

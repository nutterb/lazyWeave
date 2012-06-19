lazy.page.number <- function(num_style = c("arabic", "roman", "Roman", "alph", "Alph")){
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html")) stop("option(\"lazyReportFormat\") must be either 'latex' or 'html'")
  
  if (reportFormat == "latex"){
    num_style <- match.arg(num_style, c("arabic", "roman", "Roman", "alph", "Alph"))
    return(paste("\\pagenumbering{", num_style, "}\n\n", sep=""))
  }
  if (reportFormat == "html") return("")
    
}
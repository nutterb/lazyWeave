map.size <- function(x, reportFormat=getOption("lazyReportFormat")){
  
  size.ref <- data.frame(latex=c("tiny", "scriptsize", "footnotesize", "small", "normalsize", "large",
                                 "Large", "LARGE", "huge", "Huge"),
                         html=c(5, 7, 8, 9, 10, 12, 14, 18, 20, 24),
                         stringsAsFactors=FALSE)
  
  if (reportFormat == "latex"){
    if (gsub("[[:punct:]]", "", x) %in% size.ref$latex) return(x)
    if (is.numeric(x) && x >= 5) html.size <- tail(size.ref$html[size.ref$html <= x], 1) else html.size <- 5
    x <- paste("\\", size.ref$latex[size.ref$html == html.size], sep="")
  }
  
  if (reportFormat == "html"){
    if (is.numeric(x)) return(x)
    if (gsub("[[:punct:]]", "", x) %in% size.ref$latex) x <- size.ref$html[size.ref$latex == gsub("[[:punct:]]", "", x)]
    else{
      x <- get(".HTML.FONT.SIZE.", envir=.GlobalEnv)
      warning("Could not map character size description to html font size.  The option '.HTML.FONT.SIZE.' is used instead")
    }
  }
      
  return(x)
}      


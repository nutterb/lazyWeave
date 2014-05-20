lazy.footnote <- function(text, number=NULL, translate=FALSE,
                          name, ref, counter="footnote", size=8){
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html")) stop("option(\"lazyReportFormat\") must be either 'latex' or 'html'")
  
  if (reportFormat == "latex"){
    num <- if (is.null(number)) "" else paste("[", number, "]", sep="")  
    code <- paste("\\footnote", num, "{", text, "}", sep="")
  }
  
  if (reportFormat == "html"){
    if (!is.null(number)){
      if (!is.numeric(number)) stop("'number' must be numeric")
      lazy.counter(counter, number, fn="set")
    } 
    
    end.value <- lazy.counter(counter, fn="value")
    lazy.counter(counter, end.value + 1, "set")
    
    code <- paste("<sup>[<a name='", name, "' href='#", ref, "'>", end.value, "</a>]</sup>", sep="")
    
    to.add <- lazy.text(paste("<sup>[<a name='", ref, "' href='#", name, "'>", end.value, "</a>]</sup>", sep=""), text, size=size)
    assign(".HTML.FOOTNOTES.", paste(get("HTML.FOOTNOTES", envir=htmlCounters), to.add, sep="\n"), envir=htmlCounters) 
  }
  
  return(code)
}

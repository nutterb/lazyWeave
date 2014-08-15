lazy.section <- function(heading, type=c("section", "sub", "subsub", "chapter", "sub2"),
    ordered=FALSE, counter, counterSet=NULL, label=NULL,
    font=getOption("html.font.font"), family=getOption("html.font.family"),
    size=getOption("html.font.size"), leadspace=TRUE, floatBarrier=TRUE){
  
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html", "markdown")) stop("option(\"lazyReportFormat\") must be either 'latex' 'html', or 'markdown'")
  
  #*** Construct the comment with the function call
  comment.char <- if (reportFormat == "latex") c("%%", "")
  else if (reportFormat == "html") c("<!--", "-->")
  fncall <- paste(comment.char[1], paste(deparse(match.call()), collapse=" "), comment.char[2], "\n")

  
  type <- match.arg(type, c("section", "sub", "subsub", "chapter", "sub2"))
        
  
  if (reportFormat == "latex"){
    counterStr <- if (!missing(counter)) lazy.counter(counter, fn="use") else "%% \\usecounter{}\n"
    if (!is.null(counterSet)) counterStr <- paste(counterStr, lazy.counter(counter, value=counterSet - 1, fn="set"), sep="\n")
  
    label <- if (!is.null(label)) lazy.label(label) else "%% \\label{}\n"

    if (type == "sub")                         type <- "subsection"
    if (type == "sub2" || type == "subsub")    type <- "subsubsection"
    if (type == "chapter")                     type <- "chapter"
  
    star <- if(ordered) "" else "*"
  
    code <- paste(fncall, counterStr, "\n", if (floatBarrier) "\\FloatBarrier\n" else "", 
                  "\\", type, star, "{", heading, "}\n", label, "\n\n", sep="")
  }
  
  
  if (reportFormat == "html"){
    if (missing(counter)) counter <- type
    
    if (!is.null(counterSet)) lazy.counter(counter, counterSet, fn="set")
    count.val <- lazy.counter(counter, fn="value")
    
    sec.number <- switch(type,
                         "chapter" = paste("Chapter ", count.val, ": ", sep=""),
                         "section" = paste(lazy.counter("chapter", fn="value") - 1, ".", count.val, ": ", sep=""),
                         "sub" = paste(lazy.counter("chapter", fn="value") - 1, ".", lazy.counter("section", fn="value") - 1, ".", count.val, ": ", sep=""),
                         "sub2" = paste(lazy.counter("chapter", fn="value") - 1, ".", lazy.counter("section", fn="value") - 1, ".",
                                        lazy.counter("sub", fn="value") - 1, ".", count.val, ": ", sep=""),
                         "subsub" = paste(lazy.counter("chapter", fn="value") - 1, ".", lazy.counter("section", fn="value") - 1, ".",
                                          lazy.counter("sub", fn="value") - 1, ".", count.val, ": ", sep=""))
    
    if (ordered){
      lazy.counter(counter, count.val + 1, fn="set")
      if (counter %in% "chapter"){
        lazy.counter("section", 1, "set")
        lazy.counter("sub", 1, "set")
        lazy.counter("subsub", 1, "set")
      }
      if (counter %in% "section"){
        lazy.counter("sub", 1, "set")
        lazy.counter("subsub", 1, "set")
      }
      if (counter %in% "sub") lazy.counter("subsub", 1, "set")
    }
    
    if (type %in% "section") H <- 3
    else if (type %in% "sub") H <- 4
    else if (type %in% c("subsub", "sub2")) H <- 5
    else H <- 2
    
    code <- paste(if (leadspace) "<br><br><br>" else "", #May need to change to </br> in the future.
                  "<H", H, " style='font-family:", font, ", ", family, "; font-weight:bold;'>",
                  if (ordered) sec.number else "",
                  heading, "</H", H, ">\n", sep="")  
  }
  
  if (reportFormat == "markdown"){
    if (missing(counter)) counter <- type
    
    if (!is.null(counterSet)) lazy.counter(counter, counterSet, fn="set")
    count.val <- lazy.counter(counter, fn="value")
    
    sec.number <- switch(type,
                         "chapter" = paste("Chapter ", count.val, ": ", sep=""),
                         "section" = paste(lazy.counter("chapter", fn="value") - 1, ".", count.val, ": ", sep=""),
                         "sub" = paste(lazy.counter("chapter", fn="value") - 1, ".", lazy.counter("section", fn="value") - 1, ".", count.val, ": ", sep=""),
                         "sub2" = paste(lazy.counter("chapter", fn="value") - 1, ".", lazy.counter("section", fn="value") - 1, ".",
                                        lazy.counter("sub", fn="value") - 1, ".", count.val, ": ", sep=""),
                         "subsub" = paste(lazy.counter("chapter", fn="value") - 1, ".", lazy.counter("section", fn="value") - 1, ".",
                                          lazy.counter("sub", fn="value") - 1, ".", count.val, ": ", sep=""))
    
    if (ordered){
      lazy.counter(counter, count.val + 1, fn="set")
      if (counter %in% "chapter"){
        lazy.counter("section", 1, "set")
        lazy.counter("sub", 1, "set")
        lazy.counter("subsub", 1, "set")
      }
      if (counter %in% "section"){
        lazy.counter("sub", 1, "set")
        lazy.counter("subsub", 1, "set")
      }
      if (counter %in% "sub") lazy.counter("subsub", 1, "set")
    }
    
    if (type %in% "section") H <- "##"
    else if (type %in% "sub") H <- "###"
    else if (type %in% c("subsub", "sub2")) H <- "####"
    else H <- "#"
    
    code <- paste(H, if (ordered) sec.number else "", heading, sep=" ")
    
    
  }
  
  return(code)
}
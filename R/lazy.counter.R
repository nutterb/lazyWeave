lazy.counter <- function(counter, value, oldcounter, fn=c("new", "addto", "set", "use", "value")){

  fn <- match.arg(fn, c("new", "addto", "set", "use", "value"))

  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")

  
  #*** Latex Counters
  if (reportFormat == "latex"){
    if (!missing(value)) if (!is.numeric(value)) stop("'value' must be numeric")
  
    #*** newcounter function
    if (fn %in% "new"){
      txt <- paste("\\newcounter{", counter, "}", sep="")
      if (!missing(oldcounter)) txt <- paste(txt, "[", oldcounter, "]", sep="")
    }
  
    #*** addtocounter function
    if (fn %in% "addto"){
      txt <- paste("\\addtocounter{", counter, "}{", value, "}", sep="")
    }
  
    #*** setcounter function
    if (fn %in% "set"){
      txt <- paste("\\setcounter{", counter, "}{", value, "}", sep="")
    }
  
    #*** usecounter function
    if (fn %in% "use"){
      txt <- paste("\\usecounter{", counter, "}", sep="")
    }
  
    #*** value function
    if (fn %in% "value"){
      txt <- paste("\\value{", counter, "}", sep="")
    }
  
    return(txt)
  }
  
  
  #*** HTML counters
  if (reportFormat == "html"){
    if ("set" %in% fn){
      if ("table" %in% counter)         options("html.counter.table" = value)
      else if ("figure" %in% counter)   options("html.counter.figure" = value)
      else if ("footnote" %in% counter) options("html.counter.footnote" = value)
      else if ("chapter" %in% counter)  options("html.counter.chapter" = value)
      else if ("section" %in% counter)  options("html.counter.section" = value)
      else if ("sub" %in% counter)      options("html.counter.subsection" = value)
      else if ("sub2" %in% counter || "subsub" %in% counter)    options("html.counter.subsubsection" = value)
      else {
        opt.name <- paste("html.counter.", counter, sep="")
        options(opt.name = value)
      }
    }
    else if ("new" %in% fn){
      opt.name <- paste("html.counter.", counter, sep="")
      options(opt.name = value)
    }
    else if ("value" %in% fn){
      if (counter %in% "table")    return(getOption("html.counter.table"))
      else if (counter %in% "figure")   return(getOption("html.counter.figure"))
      else if (counter %in% "footnote") return(getOption("html.counter.footnote"))
      else if (counter %in% "chapter")  return(getOption("html.counter.chapter"))
      else if (counter %in% "section")  return(getOption("html.counter.section"))
      else if (counter %in% "sub")      return(getOption("html.counter.subsection"))
      else if (counter %in% "sub2")     return(getOption("html.counter.subsubsection"))
      else {
        opt.name <- paste("html.counter.", counter, sep="")
        return(getOption(opt.name))
      }
    }
    else message("The functions 'addto' and 'use' are not defined for HTML format.  No action is taken.")
  }
}
  
  
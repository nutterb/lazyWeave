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
      if ("table" %in% counter)         assign("HTML.COUNTER.TABLE", value, envir=htmlCounters)
      else if ("figure" %in% counter)   assign("HTML.COUNTER.FIGURE", value, envir=htmlCounters)
      else if ("footnote" %in% counter) assign("HTML.COUNTER.FOOTNOTE", value, envir=htmlCounters)
      else if ("chapter" %in% counter)  assign("HTML.COUNTER.CHAPTER", value, envir=htmlCounters)
      else if ("section" %in% counter)  assign("HTML.COUNTER.SECTION", value, envir=htmlCounters)
      else if ("sub" %in% counter)      assign("HTML.COUNTER.SUBSECTION", value, envir=htmlCounters)
      else if ("sub2" %in% counter || "subsub" %in% counter)    assign("HTML.COUNTER.SUBSUBSECTION", value, envir=htmlCounters)
      else assign(paste("HTML.COUNTER", counter, ".", sep=""), value, envir=htmlCounters)
    }
    else if ("new" %in% fn){
      assign(paste("HTML.COUNTER", counter, ".", sep=""), value, envir=htmlCounters)
    }
    else if ("value" %in% fn){
      if (counter %in% "table")    return(get("HTML.COUNTER.TABLE", envir=htmlCounters))
      else if (counter %in% "figure")   return(get("HTML.COUNTER.FIGURE", envir=htmlCounters))
      else if (counter %in% "footnote") return(get("HTML.COUNTER.FOOTNOTE", envir=htmlCounters))
      else if (counter %in% "chapter")  return(get("HTML.COUNTER.CHAPTER", envir=htmlCounters))
      else if (counter %in% "section")  return(get("HTML.COUNTER.SECTION", envir=htmlCounters))
      else if (counter %in% "sub")      return(get("HTML.COUNTER.SUBSECTION", envir=htmlCounters))
      else if (counter %in% "sub2")     return(get("HTML.COUNTER.SUBSUBSECTION", envir=htmlCounters))
      else get(paste("HTML.COUNTER.", counter, ".", sep=""), envir=htmlCounters)

    }
    else message("The functions 'addto' and 'use' are not defined for HTML format.  No action is taken.")
  }
}
  
  

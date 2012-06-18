lazy.counter <- function(counter, value, oldcounter, fn=c("new", "addto", "set", "use", "value")){
  fn <- match.arg(fn, c("new", "addto", "set", "use", "value"))
  
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
  
  
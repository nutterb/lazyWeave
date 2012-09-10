'pvalue.QHS' <- function(value){
  f <- function(p){
    if(is.na(p)) p <- NA
    else if(p < 0 || p > 1) stop(paste(p,"is not a valid probability"))
    else if(p > 0.99) p <- "> 0.99"
    else if(p > 0.10) p <- format(round(p,2),nsmall=2)
    else if(p > 0.001) p <- format(round(p,3),nsmall=2)
    else p <- "< 0.001"
    return(p)
  }
  p <- sapply(value,f)
  return(p)
}
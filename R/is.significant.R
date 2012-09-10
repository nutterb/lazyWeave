'is.significant' <- function(pvalue,alpha=.05){

#*** 1. Eliminate < and > characters.  These are produced by pvalue.QHS
  pvalue <- sub("<","",pvalue)
  pvalue <- sub(">","",pvalue)
  pvalue <- as.numeric(pvalue)

  return(pvalue<alpha)
}

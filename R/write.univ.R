write.univ <- function(x, round=1, 
    Factor=TRUE, Group=FALSE, N=TRUE, Missing=FALSE,
    Mean=TRUE, SD=TRUE, LCL=FALSE, UCL=FALSE, Min=TRUE,
    P25=TRUE, Median=TRUE, P75=TRUE, Max=TRUE, CV=FALSE, Pval=FALSE, 
    pvalFormat="default", pvalArgs=list(),
    cat=getOption("lazyWeave_cat"), ...){

  reportFormat <- getOption("lazyReportFormat")
  
  x <- x[, c(Factor, Group, N, Missing, Mean, SD, LCL, UCL,
             Min, P25, Median, P75, Max, CV, Pval), drop=FALSE]
  
  x$PVAL <- do.call("pvalString", 
                    c(list(p=x$PVAL, 
                           format=pvalFormat), 
                      pvalArgs))

  num <- sapply(x, is.numeric)
  x[, num] <- lapply(x[, num], round, round)
  x[is.na(x)] <- ""
  rownames(x) <- NULL

  if (cat) cat(lazy.matrix(x, ...))
  else return(lazy.matrix(x, ...))
}


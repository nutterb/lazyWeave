write.univ <- function(x, round=1, 
    Factor=TRUE, Group=FALSE, N=TRUE, Missing=FALSE,
    Mean=TRUE, SD=TRUE, LCL=FALSE, UCL=FALSE, Min=TRUE,
    P25=TRUE, Median=TRUE, P75=TRUE, Max=TRUE, CV=FALSE, ...){

  reportFormat <- getOption("lazyReportFormat")
  
  x <- x[, c(Factor, Group, N, Missing, Mean, SD, LCL, UCL,
             Min, P25, Median, P75, Max, CV), drop=FALSE]

  num <- sapply(x, is.numeric)
  x[, num] <- lapply(x[, num], round, round)
  x[is.na(x)] <- ""
  rownames(x) <- NULL

  return(lazy.matrix(x, ...))
}


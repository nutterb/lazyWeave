#' @name pvalue.QHS
#' @export pvalue.QHS
#' 
#' @title Format p-values
#' @description Prepares p-values for printing according to the 
#'     Alpha-Beta team standards
#' 
#' @param value A single number.  0 value < 1
#' 
#' @details 
#'   Cleveland Clinic Foundation's Alpha-Beta team uses the following format 
#'   for p-values when presenting results to researchers:
#'     \tabular{ll}{
#'       \tab	values greater than 0.99 are reported as '> 0.99'\cr
#'       \tab values between 0.99 and 0.10 are reported with two significant digits\cr
#'       \tab values between 0.001 and 0.10 are reported with three significant digits\cr
#'       \tab values less than 0.001 are reported as '< 0.001'.
#'     }
#'     
#' @author Benjamin Nutter
#' 

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

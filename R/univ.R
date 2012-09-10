'univ' <- function(data, vars, byVar, alpha=0.05){

#********************************************************************
#* 3. Provide dummy byVar if necessary
#* 4. Coerce byVar to a factor variable
#********************************************************************

#*** 3. Provide dummy byVar if necessary
  if(missing(byVar)){
    byVar <- "SomePlaceHolderVariable"
    data[,byVar] <- factor(1)  
    byVar.miss <- TRUE }
  else byVar.miss <- FALSE

#*** 4. Coerce byVar to a factor variable
  if(!is.factor(data[,byVar])){
    data[,byVar] <- factor(data[,byVar])
    warning(paste(expression(byVar),"was coerced to a factor variable"))  }

#********************************************************************
#* 1. Generic function to obtain statistics
#* 2. Generic function to obtain counts
#********************************************************************

#*** 1. Generic function to obtain statistics
  stat.func <- function(v,func,...){
    st <- suppressWarnings(tapply(data[,v],data[,byVar],func,na.rm=TRUE,...))
    st <- ifelse(is.finite(st), st, NA)
    return(st)
  }

#*** 2. Generic function to obtain counts
  count.func <- function(v,missing=FALSE){
    if(missing) tapply(is.na(data[,v]),data[,byVar],sum)
    else        tapply(!is.na(data[,v]),data[,byVar],sum)  }

#********************************************************************
#* Get Factor and Group names and corresponding statistics
#********************************************************************

  if(byVar.miss) Factor <- label(data[vars], default=names(data[, vars]))
  else Factor <- unlist(lapply(vars,function(v) 
                    c(label(data[, v], default=v),
                      rep(NA,nlevels(data[,byVar])-1))))
  Group <- rep(levels(data[,byVar]),length(vars))
  N <- as.vector(sapply(vars,count.func))
  MISSING <- as.vector(sapply(vars,count.func,missing=TRUE))
  MEAN <- as.vector(sapply(vars,stat.func,func="mean"))
  MEAN <- ifelse(is.nan(MEAN), NA, MEAN)
  SD <- as.vector(sapply(vars,stat.func,func="sd"))
  LCL <- suppressWarnings(ifelse(N > 1, MEAN - qt(1-alpha/2, N-1) * SD, NA))
  UCL <- suppressWarnings(ifelse(N > 1, MEAN + qt(1-alpha/2, N-1) * SD, NA))
  MIN <- as.vector(sapply(vars,stat.func,func="min"))
  P25 <- as.vector(sapply(vars,stat.func,func="quantile",probs=0.25))
  MEDIAN <- as.vector(sapply(vars,stat.func,func="median"))
  P75 <- as.vector(sapply(vars,stat.func,func="quantile",probs=0.75))
  MAX <- as.vector(sapply(vars,stat.func,func="max"))
  CV <- SD/MEAN

#********************************************************************
#* 1. Prepare Output Data Frame
#* 2. Prepare Report Data Frame
#* 3. Format Non-Count Values in Report
#* 4. Remove Group Column if byVar is not provided
#********************************************************************

#*** 1. Prepare Output Data Frame
  output <- data.frame(Factor=Factor, Group=Group, N=N, MISSING=MISSING,
                       MEAN=MEAN, SD=SD, LCL=LCL, UCL=UCL,
                       MIN=MIN, P25=P25, MEDIAN=MEDIAN, P75=P75,
                       MAX=MAX, CV=CV,
                       stringsAsFactors=FALSE)
  names(output) <- c("Factor", "Group", "N", "Missing", "Mean", "SD", "LCL",
                     "UCL", "Min", "P25", "MEDIAN", "P75", "MAX", "CV")

  rownames(output) <- NULL
  class(output) <- c("univ", "data.frame")
  return(output)
}

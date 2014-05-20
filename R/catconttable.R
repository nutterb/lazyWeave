catconttable <- function(data, vars, byVar, vars.cat=NULL, fisher=NULL, fisher.arg=NULL,
                     cmh=NULL, row.score=NULL, col.score=NULL,
                     normal = NULL, var.equal = NULL, 
                     median=NULL, odds=NULL, odds.scale=NULL, odds.unit=NULL,
                     none=NULL,
                     row.p=TRUE, alpha=0.05, B=1000, seed=NULL){
  
  if (missing(byVar)){
    byVar <- "PlAcE_hOlDeR_fOr_CaTcOnTtAbLe"
    data[, byVar] <- factor("")
  }
  
  if (!all(vars %in% names(data))){
    bad.vars <- vars[!vars %in% names(data)]
    bad.vars.msg <- paste("The following variables are not found in 'data':", paste(bad.vars, collapse=", "))
    stop(bad.vars.msg)
  }

  all.missing <- sapply(data[, c(vars, byVar)], function(x) all(is.na(x)))
  if (any(all.missing)){
    miss.vars <- c(vars, byVar)[all.missing]
    miss.vars.msg <- paste("The following variables contain only missing values:", paste(miss.vars, collapse=", "))
    stop(miss.vars.msg)
  }
  
  var.info <- function(v, ...){
    if (!is.numeric(data[, v]) | v %in% vars.cat)
      cattable(data=data, vars=v, byVar=byVar, fisher=fisher, fisher.arg=fisher.arg,
                    cmh=cmh, row.score=row.score, col.score=col.score,
                    odds=odds,
                    none=none, row.p=row.p, alpha=0.05)
    else conttable(data=data, vars=v, byVar=byVar,
                 normal = normal, var.equal = var.equal, median=median,
                 odds = odds, odds.scale=odds.scale, odds.unit=odds.unit,
                 alpha = alpha, B=B, seed=seed)
  }

  ctable <- do.call("rbind", lapply(vars, var.info))
  attributes(ctable)$byVar <- data[, byVar]
  label(attributes(ctable)$byVar) <- label(data[, byVar])
  attributes(ctable)$vars <- vars  
  return(ctable)
}

             
             

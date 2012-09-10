catconttable <- function(data, vars, byVar, vars.cat=NULL, fisher=NULL,
                     cmh=NULL, row.score=NULL, col.score=NULL,
                     normal = NULL, var.equal = NULL, 
                     median=NULL, odds=NULL, odds.scale=NULL, odds.unit=NULL,
                     row.p=TRUE, alpha=0.05, B=1000, seed=NULL){

  var.info <- function(v, ...){
    if (!is.numeric(data[, v]) | v %in% vars.cat)
      cattable(data=data, vars=v, byVar=byVar, fisher=fisher,
                    cmh=cmh, row.score=row.score, col.score=col.score,
                    odds=odds, row.p=row.p, alpha=0.05)
    else conttable(data=data, vars=v, byVar=byVar,
                 normal = normal, var.equal = var.equal, median=median,
                 odds = odds, odds.scale=odds.scale, odds.unit=odds.unit,
                 alpha = alpha, B=B, seed=seed)
  }

  if (missing(byVar)){
    byVar <- "PlAcE_hOlDeR_fOr_CaTcOnTtAbLe"
    data[, byVar] <- factor("")
  }
  ctable <- do.call("rbind", lapply(vars, var.info))
  attributes(ctable)$byVar <- data[, byVar]
  label(attributes(ctable)$byVar) <- label(data[, byVar])
  attributes(ctable)$vars <- vars  
  return(ctable)
}

             
             
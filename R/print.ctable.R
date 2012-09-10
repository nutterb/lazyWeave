print.ctable <- function(x, ...){

  nlev <- nlevels(attributes(x)$byVar)
  lev <- levels(attributes(x)$byVar)

  x <- as.data.frame(x)

  summary.names <- function(r){
    k <- x$type[r]
    if (k == "Bootstrap Mean"){
      n <- c("boot",  "lowerb", "upperb")
      note <- "*a*"
    }
    else if (k == "Parametric Mean"){
      n <- c("mean",   "sd",     "prop")
      note <- "*b*"
    }
    else if (k == "Median"){
      n <- c("median", "p25",    "p75")
      note <- "*c*"
    }
    else{
      n <- c("prop",   "mean",   "sd")
      note <- "*d*"
    }

    n <- as.vector(t(sapply(n, grep, names(x))))
    summ <- x[r, n]
    names(summ) <- paste("stat", rep(LETTERS[1:3], nlev), sep="")
    names(summ) <- paste(names(summ), ".", rep(lev, each=3), sep="")
    if (k %in% c("Bootstrap Mean", "Parametric Mean", "Median") ||
        all(is.na(summ)))
      rownames(summ) <- paste(rownames(summ), note)
    summ
  }
  
  descrip <- round(do.call("rbind", lapply(1:nrow(x), summary.names)), 2)
  out <- cbind(x[, "total", drop=FALSE], descrip,
               x[, c("test.stat", "pvalue")])
  out$test.stat <- round(out$test.stat, 2)
  out$pvalue <- round(out$pvalue, 3)
  out[is.na(out)] <- ""
  rownames(out) <- rownames(descrip)
  print(out)
  cat("\n*a* Mean and Bootstrap Confidence Limits",
      "\n*b* Mean and Standard Deviation",
      "\n*c* Median and Quartiles",
      "\n*d* Percentage\n")
}


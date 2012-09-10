'mantel.test' <-
function(x,byVar,row.scores=c("equal","midrank"),
                  col.scores=c("equal","midrank")){

#*****************************************************************************
#* Calculate the midranks for a set of categorical observations.
#*****************************************************************************
  midrank <- function(table,row=TRUE){
    if(row==TRUE){ Mar.Tot <- rowSums(table) }
    else{          Mar.Tot <- colSums(table) }
    Cum.Tot <- cumsum(Mar.Tot)
    scores <- mean(c(1,Cum.Tot[1]))
    for(i in 2:length(Cum.Tot)){
      scores[i] <- mean(c(Cum.Tot[i-1]+1,Cum.Tot[i]))
    }
    return(scores)
  }

#****************************************************************************
#* Prep Work
#* 1. Make table of data.
#* 2. Determine row scores
#* 3. Determine column scores
#* 4. If user defined scores are passed, verify that the number of scores
#*    is equal to the number of levels in the variable.
#****************************************************************************

#*** 1. Make a table of data
  if(missing(byVar)) temp <- x else temp <- table(x,byVar)

#*** 2. Determine row scores
  suppressWarnings(
      # Equally spaced
      if(row.scores=="equal"){ row.scores <- seq(1,nrow(temp)) }
      # Midrank sores
      else if(row.scores=="midrank"){ row.scores <- midrank(temp) } )

#*** 3. Determine column scores
  suppressWarnings(
      # Equally spaced
      if(col.scores=="equal"){ col.scores <- seq(1,ncol(temp)) }
      # Midrank sores
      else if(col.scores=="midrank"){ col.scores <- midrank(temp,row=FALSE) } )

#*** 4. Verify validity of user defined scores
  if(length(row.scores)!=nrow(temp)){
    warn.string <- paste("Number of row scores given does not match\n",
                      "the number of levels in",substitute(x),"\n")
    warning(warn.string) }
  else if(length(col.scores)!=ncol(temp)){
    warn.string <- paste("Number of column scores given does not match\n",
                      "the number of levels in",substitute(x),"\n")
    warning(warn.string) }

#*****************************************************************************
#* Calculate Test Statistic
#* 1. Calculate Test Statistic.  Statistic is calculated in parts to save
#*    the headache of long strings of parentheses.
#* 2. Build htest object
#*****************************************************************************

#*** 1. Calculate Test Statistic
  else{
    u <- row.scores
    v <- col.scores
    n <- sum(temp)
    p1 <- sum(t(t(temp)*v)*u)
    p2 <- sum(u*rowSums(temp))
    p3 <- sum(v*colSums(temp))
    p4 <- sum(u^2*rowSums(temp))
    p5 <- sum(u*rowSums(temp))^2
    p6 <- sum(v^2*colSums(temp))
    p7 <- sum(v*colSums(temp))^2
    num <- p1-(p2*p3/n)
    den <- sqrt((p4-p5/n)*(p6-p7/n))
    r <- num/den
    M2 <- (n-1)*r^2
    pvalue <- 1-pchisq(M2,1)
    df <- 1

#*** 2. Build htest object
    names(M2) <- "M^2"
    names(df) <- "df"
    #names(r) <- "correlation"
    METH <- "Mantel Haenszel Chi-Square Test for Two Way Tables"
    structure(list(statistic=M2,parameter=df,
              p.value=pvalue, method=METH,
              correlation=r),class="htest")
  }
}


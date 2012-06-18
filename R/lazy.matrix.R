lazy.matrix <-
function(x, align="center", justify="center", rcol=NULL, usecol="lightgray",
    caption=NULL, footnote=NULL, placement="!H", translate=TRUE, ...){
    
     
  fncall <- paste("%%", paste(deparse(match.call()), collapse=""), "\n")

#*** Coerce x to a matrix
  if (!is.matrix(x)) x <- as.matrix(x)
  
  if ("cwidth" %in% names(list(...))){
    cw <- list(...)$cw
    if (!is.null(rownames(x))){
      if (length(cw) != 1 && ((ncol(x) + 1) != length(cw)))
        stop("'cwidth' must have length 1 or equal to ncol(x)--remember your row names")
    }
  }
  
#*** Extend length of align to number of columns of x.  This will be useful
#*** if we add a column for rownames
  if (length(align) == 1) align <- rep(align, ncol(x))

#*** Add the rownames to x and assign them left justification
  if (!is.null(rownames(x))){
    x <- cbind(rownames(x), x)
    rownames(x) <- NULL
    align = c("left", align)
  }

#*** Table if colnames are present
  if (!is.null(colnames(x))){
    header <- lazy.table(colnames(x), align=align, cspan=1,
                          justify=justify, rborder=c(0, 0, 1), 
                          open=TRUE, close=FALSE,
                          caption=caption, placement=placement,
                          translate=translate, ...)
    body <- lazy.table(x, align=align, cspan=1,
                        rborder=nrow(x), rcol=rcol,
                        justify=justify, usecol=usecol, 
                        open=FALSE, close=TRUE, 
                        footnote=footnote,
                        translate=translate, ...)
  }

#*** Table if colnames are not present
  else{
    header <- ""
    body <- lazy.table(x, align=align, cspan=1,
                        justify=justify, rborder=c(0, 0, nrow(x)),
                        open=TRUE, close=TRUE, 
                        rcol=rcol, usecol=usecol,
                        caption=caption, footnote=footnote,
                        placement=placement,
                        translate=translate, ...)
  }
  
  paste(fncall, header, body, sep="")
}


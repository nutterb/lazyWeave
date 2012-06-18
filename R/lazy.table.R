lazy.table <-
function(x, align="center", justify="center", placement="!H",
    cspan=1, cborder=NULL, cwidth=NULL,
    rborder=NULL, rbspan=NULL, rheight=NULL,
    rcol=NULL, usecol="lightgray",
    caption=NULL, footnote=NULL, textsize="\\normalsize",
    counter=NULL, counterSet=NULL, label=NULL,
    open=TRUE, close=TRUE, translate=TRUE){

  fncall <- paste("%%", paste(deparse(match.call()), collapse=" "), "\n")

#*** Change left or right justify to valid LATEX command
  if (justify %in% c("left", "right")) 
    justify <- paste("flush", justify, sep="")
  
#*** Coerce x to a matrix.  Vectors are coerced to a row matrix
  if (is.null(dim(x))) x <- matrix(x, nrow=1)
  if (!is.matrix(x)) x <- as.matrix(x)
  xdim <- dim(x)
  
  if (!is.null(cwidth)){
    if (length(cwidth) != 1 && ((ncol(x)) != length(cwidth)))
      stop("'cwidth' must have length 1 or equal to ncol(x)")
  }
  
  if (translate) x <- latexTranslate(x)
  
#*** Create a string for the number of columns.  This is used when
#*** initializing the tabular environment.
  tot.col <- max(sum(cspan), xdim[2])
  env.align <- paste(rep("c", tot.col), collapse="")
  
#*** Strings to open and close the tabular environment, if requested.
  if (is.null(caption)) caption <- "" 
  else{
    caption <- paste("\\caption{", caption, "}\n", sep="")
    if (translate) caption <- latexTranslate(caption)
  }
  
  if (is.null(footnote)) footnote <- ""
  if (translate) footnote <- latexTranslate(footnote)
  
#*** String for the counter
  counterStr <- if (!is.null(counter)) paste(lazy.counter(counter, fn="use"), "\n", sep="") else "%% \\usecounter{}\n"
  if (!is.null(counterSet)) counterStr <- paste(counterStr, lazy.counter(counter, value=counterSet - 1, fn="set"), "\n", sep="")
  
#*** String for label
  label <- if (!is.null(label)) lazy.label(label) else "%% \\label{}\n"

  code.open <- 
    if (open) paste("\\begin{table}", "[", placement, "]\n",
                    counterStr,
                    caption,
                    label, 
                    "\\begin{", justify, "}", textsize, "\n", 
                    "\\begin{tabular}{", env.align, "}\n", sep="") else ""
            
  code.close <- 
    if (close) paste("\\end{tabular}\n",
                     "\\end{", justify, "}", footnote, 
                     "\n\\end{table}", sep="") else ""
  if (close & (nrow(x) %in% rborder)){
    nline <- paste(rep("\\hline", sum(rborder %in% nrow(x))), collapse="")
    code.close <- paste(nline, "\n", code.close, sep="")
  }
   
#*** The alignment for each cell, the col border, and the color all go in the
#*** \multicol command.  A matrix is built here to faciliate easy assembly
  align <- substr(align, 1, 1)
  if (is.null(cwidth)) cwidth <- rep("", length(align))
  cwidth <- ifelse(cwidth != "", paste("b{", cwidth, "in}", sep=""), cwidth)
  if ("c" %in% align) cwidth[align=="c"] <- gsub("b", ">{\\\\centering}b", cwidth[align=="c"])
  if ("r" %in% align) cwidth[align=="r"] <- gsub("b", ">{\\\\raggedleft}b", cwidth[align=="r"])
  align[!cwidth %in% ""] <- cwidth[!cwidth %in% ""]
  
  align.mat <- matrix(align, nrow=xdim[1], ncol=xdim[2], byrow=TRUE)
  align.mat <- cbind("", align.mat)  
  
  cbord.mat <- rep("", xdim[2] + 1)
  cbord.mat[cborder + 1] <- "|"
  cbord.mat <- matrix(cbord.mat, nrow=xdim[1], ncol=xdim[2] + 1, byrow=TRUE)
  
  color.mat <- matrix("", nrow=xdim[1], ncol=xdim[2])
  color.mat[rcol, ] <- paste(">{\\columncolor{", usecol, "}}", sep="")
  color.mat <- cbind("", color.mat)
  
  row.mat <- matrix(paste(color.mat, align.mat, cbord.mat, sep=""),
                    nrow=xdim[1], ncol=xdim[2] + 1, byrow=FALSE)
  row.mat[, 2] <- paste(row.mat[,1], row.mat[,2], sep="")
  row.mat <- row.mat[, -1]
  
#*** Create a string to denote the row borders (underlined columns)
  if (is.null(rbspan)) rbspan <- c(1, tot.col)
  if (is.list(rbspan)){
    cline <- paste("\\\\cline{", sapply(rbspan, min), "-", 
                   sapply(rbspan, max), "}", sep="")
    cline <- paste(paste(cline, collapse=""), "", sep="")
  }
  else cline <- paste("\\\\cline{", min(rbspan), "-", 
                      max(rbspan), "}", sep="")
                      
#*** Row Borders for row 0 (top of table)
  if(0 %in% rborder){ 
    nline <- paste(rep("\\hline", sum(rborder %in% 0)), collapse="")
    code.open <- paste(code.open, nline, "\n", sep="")
    rborder <- rborder[rborder != 0]
  }
                      
#*** Construct the body of the table
  code.tab <- paste("  \\multicolumn{", cspan, "}", 
                    "{", t(row.mat), "}{", t(x), "}", sep="")
  code.tab <- matrix(code.tab, nrow=xdim[1], ncol=xdim[2], byrow=TRUE)
  code.tab[, xdim[2]] <- paste(code.tab[, xdim[2]], "\\\\\n", sep="")
  code.tab <- apply(code.tab, 1, paste, collapse=" & ")
  code.tab[rborder] <- gsub("\\\n", cline, code.tab[rborder])
  code.tab <- paste(code.tab, collapse="")
  
  paste(fncall, code.open, code.tab, code.close, if (close) "\n\n" else "", sep="")
}


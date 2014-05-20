lazy.table <- function(x, 
                       align="center", cspan=1, cwidth=NULL, cwidth.units="in", 
                       cborder=NULL, cborder.thick=1, cborder.style="solid black",
                       rborder=NULL, rbspan=NULL, rborder.thick=1, rborder.style="solid black", 
                       rcol=NULL, usecol="lightgray",
                       font, family, size,
                       justify="center", placement="h",
                       open=TRUE, close=TRUE, 
                       caption=NULL, footnote=NULL, label=NULL,
                       counter=NULL, counterSet=NULL,
                       translate=TRUE, textsize=NULL){
  
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html")) stop("option(\"lazyReportFormat\") must be either 'latex' or 'html'")
  
  #*** Construct the comment with the function call
  comment.char <- if (reportFormat == "latex") c("%%", "")
  else if (reportFormat == "html") c("<!--", "-->")
  
  fncall <- paste(comment.char[1], paste(deparse(match.call()), collapse=" "), comment.char[2], "\n")
  
  #*** Enforce that x is a matrix
  if (is.null(dim(x))) x <- matrix(x, nrow=1)
  if (!is.matrix(x)) x <- as.matrix(x)
  if (is.table(x)) x <- matrix(x, nrow=nrow(x), ncol=ncol(x), byrow=FALSE, dimnames=list(rownames(x), colnames(x)))
  xdim <- dim(x)
  
  if (!is.null(cwidth)){
    if (length(cwidth != 1) && ((ncol(x)) != length(cwidth)))
      stop("'cwidth' must have length 1 or equal to ncol(x)")
  }

  if (missing(font)) font <- get(".HTML.FONT.FONT.", envir=.GlobalEnv)
  if (missing(family)) family <- get(".HTML.FONT.FAMILY.", envir=.GlobalEnv)
  if (missing(size)) size <- get(".HTML.FONT.SIZE.", envir=.GlobalEnv)
  
  if (!is.null(textsize)){
    size <- textsize
    warning("The argument 'textsize' is scheduled for deletion in 2013.  Please use the 'size' argument instead")
  }
  
  x[is.na(x)] <- ""
  if (reportFormat == "latex" && translate) x <- latexTranslate(x)

  #****************************************************************************************************************************
  #* Arguments will be processed into their respective formats in the order they are listed in the function call
  #* This might not be the most efficient way to do this, but I expect it will make it easier for me to troubleshoot problems.
  #* Note that some objects will be altered for latex, some for html, and some for both.  If not alteration is made, it is
  #* safe to assume that no alteration was necessary.
  #****************************************************************************************************************************
  
  #*** font -- no processing necessary (affects only HTML)
  
  #*** family -- no processing necessary (affects only HTML)
  
  #*** convert size to appropriate format
  size <- map.size(size, reportFormat)
  
  #*** column alignment
  if (length(align) == 1) align <- rep(align, length.out=ncol(x))
  if (reportFormat == "latex") align <- substr(align, 1, 1)
  if (reportFormat == "html"){ 
    align <- rep(align, length.out=ncol(x))
    align <- matrix(rep(align, nrow(x)), nrow=nrow(x), ncol=ncol(x), byrow=TRUE)
  }
  
  #*** column span
  if (length(cspan) == 1) cspan <- rep(cspan, length.out=ncol(x))
  if (reportFormat == "html") cspan <- matrix(rep(cspan, nrow(x)), nrow=nrow(x), byrow=TRUE)
  
  #*** column width
  if (is.null(cwidth)) cwidth <- rep("", ncol(x))
  if (reportFormat == "latex"){
    cwidth <- ifelse(cwidth != "", paste("b{", cwidth, cwidth.units, "}", sep=""), cwidth)
    if ("c" %in% align) cwidth[align == "c"] <- gsub("b", ">{\\\\centering}b", cwidth[align == "c"])
    if ("r" %in% align) cwidth[align == "r"] <- gsub("b", ">{\\\\raggedleft}b", cwidth[align == "r"])
    align[!cwidth %in% ""] <- cwidth[!cwidth %in% ""]
  }
  if (reportFormat == "html"){
    cwidth <- paste(cwidth, cwidth.units)
    cwidth <- rep(cwidth, length.out=ncol(x))
    cwidth <- matrix(rep(cwidth, nrow(x)), nrow=nrow(x), byrow=TRUE)
  }
  
  #*** column borders
  if (reportFormat == "latex"){
    cbord.mat <- rep("", ncol(x) + 1)
    cbord.mat[cborder + 1] <- "|"
    cbord.mat <- matrix(cbord.mat, nrow=nrow(x), ncol=ncol(x) + 1, byrow=TRUE)
  }
  if (reportFormat == "html"){
    if (length(cborder.thick == 1) && !is.null(cborder)) cborder.thick <- rep(cborder.thick, length(cborder))
    
    blft <- matrix("none", nrow=nrow(x), ncol=ncol(x))
    bord.thick.lft <- matrix("", nrow=nrow(x), ncol=ncol(x))
    if (0 %in% cborder) bord.thick.lft[, 1] <- cborder.thick[which(rborder == 0)][1]
    
    brht <- matrix("none", nrow=nrow(x), ncol=ncol(x))
    bord.thick.rht <- matrix("", nrow=nrow(x), ncol=ncol(x))
    if (!is.null(cborder))
      bord.thick.rht[, cborder] <- cborder.thick[if (length(cborder) > 0) which (cborder != 0) else 1]
    
    if (!is.null(cborder)){
      if (0 %in% cborder) blft[, 1] <- cborder.style
      brht[, cborder[cborder != 0]] <- cborder.style
    }
  }
  
  #*** row borders
  if (reportFormat == "latex"){
    if (is.null(rbspan)) rbspan <- c(1, max(sum(cspan), ncol(x)))
    if (is.list(rbspan)){
      cline <- paste("\\\\cline{", sapply(rbspan, min), "-", sapply(rbspan, max), "}", sep="")
      cline <- paste(paste(cline, collapse=""), "", sep="")
    }
    else cline <- paste("\\\\cline{", min(rbspan), "-", max(rbspan), "}", sep="")
    
    if (0 %in% rborder){ #*** border on top of table
      nline <- paste(rep("\\hline", sum(rborder %in% 0)), collapse="")
      rborder <- rborder[rborder != 0] #*** needs to be removed for a paste operation later
    }
    else nline <- ""
  }
  if (reportFormat == "html"){
    if (length(rborder.thick ==1) && !is.null(rborder)) rborder.thick <- rep(rborder.thick, length(rborder))
    if (is.null(rbspan)) rbspan <- 1:ncol(x)
    
    btop <- matrix("none", nrow=nrow(x), ncol=ncol(x))
    bord.thick.top <- matrix("", nrow=nrow(x), ncol=ncol(x))
    if (0 %in% rborder) bord.thick.top[1, ] <- rborder.thick[which(rborder==0)][1]
    
    bbot <- matrix("none", nrow=nrow(x), ncol=ncol(x))
    bord.thick.bot <- matrix("", nrow=nrow(x), ncol=ncol(x))
    if (!is.null(rborder))
#       return(list(x, bord.thick.bot, rborder, rbspan))
      bord.thick.bot[rborder, rbspan] <- rborder.thick[if (length(rborder) > 0) which(rborder != 0) else 1]

    if (!is.null(rborder)){
      if (0 %in% rborder) btop[1, ] <- rborder.style
      bbot[rborder[rborder != 0], ] <- rborder.style
    }
  }
  
  #*** row background colors
  if (reportFormat == "latex"){
    color.mat <- matrix("", nrow=nrow(x), ncol=ncol(x))
    color.mat[rcol, ] <- paste(">{\\columncolor{", usecol, "}}", sep="")
    color.mat <- cbind("", color.mat)
  }
  if (reportFormat == "html"){
    row.color <- matrix("", nrow=nrow(x), ncol=ncol(x))
    if (!is.null(rcol)) row.color[rcol, ] <- usecol
  }
  
  #*** Justification of table on page
  if (length(justify) > 1) stop("'justify' must have length 1")
  justify <- match.arg(justify, c("center", "left", "right"))
  
  if (reportFormat == "latex" && justify %in% c("left", "right")) justify <- paste("flush", justify, sep="")
  if (reportFormat == "html"){
    if (justify %in% "left") justify <- " margin-left:0px auto; margin-right;0px auto"
    else if (justify %in% "center") justify <- " margin-left: auto; margin-right: auto"
    else justify <- " margin-left: auto; margin-right:0px auto"
  }
  
  #*** placement -- no changes necessary
  
  #*** caption
  if (reportFormat == "latex"){
    if (is.null(caption)) caption <- ""
    else{
      caption <- paste("\\caption{", caption, "}\n", sep="")
      if (translate) caption <- latexTranslate(caption)
    }
  }
  if (reportFormat == "html"){
    if (is.null(caption)) caption <- ""
    else{ 
      if (is.null(counter)) counter <- "table"
      if (!is.null(counterSet)) lazy.counter(counter, counterSet, fn="set")
      count.val <- lazy.counter(counter, fn="value")
      caption <- paste("Table ", lazy.counter(counter, fn="value"), ": ", caption, sep="")
      lazy.counter(counter, count.val + 1, fn="set")
    }
  }
    
  #*** table footnote
  if (is.null(footnote)) footnote <- ""
  if (reportFormat == "latex" && translate) footnote <- latexTranslate(footnote)
  
  #*** label
  if (reportFormat == "latex" && !is.null(label)) label <- lazy.label(label) else "%% \\label{}\n"
  
  #*** counter manipulation (this was handled in the caption for html reports)
  if (reportFormat == "latex"){
     counterStr <- if (!is.null(counter)) paste(lazy.counter(counter, fn="use"), "\n", sep="") else "%% \\usecounter{}\n"
    if (!is.null(counterSet) & !is.null(counter)) 
      counterStr <- paste(counterStr, lazy.counter(counter, value=counterSet - 1, fn="set"), "\n", sep="")

  }

   
  #**************************************************************************************************************
  #* Write the latex table
  #* a couple of utility matrices will be made first
  #* then the open code
  #* the close code
  #* the table body code
  #**************************************************************************************************************
  
  if (reportFormat == "latex"){
    align.mat <- matrix(align, nrow=nrow(x), ncol=ncol(x), byrow=TRUE)
    align.mat <- cbind("", align.mat)
    
    row.mat <- matrix(paste(color.mat, align.mat, cbord.mat, sep=""),
                      nrow=nrow(x), ncol=ncol(x) + 1, byrow=FALSE)
    row.mat[, 2] <- paste(row.mat[, 1], row.mat[, 2], sep="")
    row.mat <- row.mat[, -1]
    
    code.open <- if (open)
      paste("\\begin{table}", "[", placement, "]\n",
            counterStr,
            caption,
            label,
            "\\begin{", justify, "}", size, "\n",
            "\\begin{tabular}{", paste(rep("c", sum(cspan)), collapse=""), "}", nline, "\n", sep="")
      else ""
    
    code.close <- if (close)
      paste("\\end{tabular}\n",
            "\\end{", justify, "}", footnote,
            "\n\\end{table}", sep="")
      else ""
    
    if (close && (nrow(x) %in% rborder)){
      bottomlines <- paste(rep("\\hline", sum(rborder %in% nrow(x))), collapse="")
      code.close <- paste(bottomlines, "\n", code.close, sep="")
    }
    
    code.tab <- paste("  \\multicolumn{", cspan, "}", 
                      "{", t(row.mat), "}{", t(x), "}", sep="")
    code.tab <- matrix(code.tab, nrow=nrow(x), ncol=ncol(x), byrow=TRUE)
    code.tab[, ncol(x)] <- paste(code.tab[, ncol(x)], "\\\\\n", sep="")
    code.tab <- apply(code.tab, 1, paste, collapse=" & ")
    code.tab[rborder] <- gsub("\\\n", cline, code.tab[rborder])
    code.tab <- paste(code.tab, collapse="")
    
    return(paste(fncall, code.open, code.tab, code.close, if (close) "\n\n" else "", sep=""))
  }
  
  
  #******************************************************************************************************
  #* HTML Code
  #******************************************************************************************************
  
  if (reportFormat == "html"){
    code <- paste("    <td colspan=", cspan, "  ", 
                  "style='font-family", font, ", ", family, "; ",
                  "font-size:", size, "pt;", 
                  "width:", cwidth, "; ",
                  "text-align:", align, "; ",
                  #"vertical-align:", valign, "; ",
                  "background-color:", row.color, "; ",
                  "padding:1.5px 5.5px 1.5px 5.5px; ",
                  "border-top:", btop, " ", bord.thick.top, "pt; ", 
                  "border-bottom:", bbot, " ", bord.thick.bot, "pt; ",
                  "border-left:", blft, " ", bord.thick.lft, "pt; ",
                  "border-right:", brht, " ", bord.thick.rht, "pt;'>",
                  x,
                  " </td>\n", sep="")
    code <- matrix(code, nrow=nrow(x), ncol=ncol(x))
    code <- cbind("  <tr>\n", code, "</tr>\n")
    code <- apply(code, 1, paste, collapse=" ")
    code <- paste(code, collapse = "\n")
    if (open) code <- paste(lazy.text(caption, italic=TRUE, align="center"),
                            "\n<table style='border-collapse:collapse;", justify, ";'>\n", code, sep="")
    if (close) code <- paste(code, "</table><br>\n", sep="")
    
    if (!is.null(label)) code <- paste("<br>", lazy.label(label), code, sep="\n")
    return(paste(fncall, code, footnote, "\n\n"))
  }

}

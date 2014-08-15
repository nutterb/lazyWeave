lazy.text <-
function(..., title=NULL, align="left",
    italic=FALSE, bold=FALSE, underline=FALSE, sep="",
    translate=TRUE, font, family, size){
    
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html", "markdown")) stop("option(\"lazyReportFormat\") must be either 'latex', 'html', or 'markdown'")
  
  #*** Construct the comment with the function call
  comment.char <- if (reportFormat == "latex") c("%%", "")
  else if (reportFormat == "html") c("<!--", "-->")
  fncall <- paste(comment.char[1], paste(deparse(match.call()), collapse=" "), comment.char[2], "\n")
  
  if (missing(font)) font <- get("HTML.FONT.FONT", envir=options()$htmlCounters)
  if (missing(family)) family <- get("HTML.FONT.FAMILY", envir=options()$htmlCounters)
  if (missing(size)) size <- get("HTML.FONT.SIZE", envir=options()$htmlCounters)
  
  
  if (reportFormat == "latex"){

    #*** Set title in bold text (can't be changed)    
    if (is.null(title)) title <- ""
    else title <- paste("\\textbf{", title, "}\\\\", sep="")

    #*** Set alignment command
    if (align %in% c("left", "right")) align <- paste("flush", align, sep="")
  
    size <- paste(map.size(size), "\n", sep="")

    #*** Opening String for alignment
    align.open <- paste("\\begin{", align, "}\n", sep="")

    #*** Closing String for alignment
    align.close <- paste("\\end{", align, "}", sep="")

    #*** Style String
    style <- ""
    if (italic)    style <- paste(style, "\\emph{", sep="")
    if (bold)      style <- paste(style, "\\textbf{", sep="")
    if (underline) style <- paste(style, "\\ul{", sep="")

    #*** Opening call to style
    style.open <- if (style %in% "") "%% \\emph{ \\textbf \\ul{\n" else style
    style.close <- paste(
                      paste(rep("}", italic + bold + underline), collapse=""),
                      sep="")
    style.close <- if (style.close %in% "") "%% } } } %%close emph, textbf, and ul" else style.close
    style.close <- paste(style.close, "\n", sep="")
           
    #*** Closing call to style            
    text <- paste(..., sep=sep)
    if (translate) text <- latexTranslate(text)
    text <- paste(text, "\n", sep="")
  
    #*** Paste all code together
    code <- paste(fncall, align.open, style.open, size, text,
                  style.close, align.close, "\n\n", sep="")
  }
  
  
  if (reportFormat == "html"){
    txt <- paste(..., sep=sep)
    
    code <- paste("<p style='font-family:", font, ", ", family, "; font-size:", map.size(size), "pt; ",
                  "text-align:", align, "; ",
                  "font-style:", if (italic) "italic; " else "none; ",
                  "font-weight:", if (bold) "bold; " else "none; ",
                  "text-decoration:", if (underline) "underline;" else "none;",
                  "'>", sep="")
    code <- paste(code, txt, "</p>")
  }
  
  if (reportFormat == "markdown"){
    code <- paste(..., sep=sep)
    code <- paste(if (bold) "**" else "", if (italic) "_" else "", code, if (italic) "_" else "", if (bold) "**" else "", sep="")
  }
  
  return(code)
}


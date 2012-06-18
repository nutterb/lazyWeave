lazy.text <-
function(..., title=NULL, size="normalsize", align="left",
    italic=FALSE, bold=FALSE, underline=FALSE, sep="",
    translate=TRUE){
    
  fncall <- paste("%%", paste(deparse(match.call()), collapse=" "), "\n")

#*** Set title in bold text (can't be changed)    
  if (is.null(title)) title <- ""
  else title <- paste("\\textbf{", title, "}\\\\", sep="")

#*** Set alignment command
  if (align %in% c("left", "right")) align <- paste("flush", align, sep="")
  
  size <- paste("\\", size, "\n", sep="")

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
  
  return(code)
}


lazy.text.format <- function(text, italic=FALSE, bold=FALSE, underline=FALSE, translate=TRUE){

  if (translate) text <- latexTranslate(text)
  
  if (underline) text <- paste("\\ul{", text, "}", sep="")
  if (bold)      text <- paste("\\textbf{", text, "}", sep="")
  if (italic)    text <- paste("\\emph{", text, "}", sep="")
  
  return(text)
}

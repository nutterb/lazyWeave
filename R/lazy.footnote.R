lazy.footnote <- function(text, number=NULL, translate=FALSE){
  num <- if (is.null(number)) "" else paste("[", number, "]", sep="")
  paste("\\footnote", num, "{", text, "}", sep="")
}
lazy.ref <- function(label, page=FALSE){ 
  if (page)  paste("\\pageref{", label, "}", sep="") else paste("\\ref{", label, "}", sep="")
}

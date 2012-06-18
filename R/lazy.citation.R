lazy.citation <- function(pkg=NULL, author=TRUE, title=TRUE, org=TRUE, address=TRUE, volume=TRUE, year=TRUE, note=TRUE){
  cit <- if (is.null(pkg)) citation() else citation(pkg)
  cit
  paste( if (author) paste(paste(cit$author, collapse=", "), ", ", sep="") else "",
         if (title)  paste("\"", cit$title, ",\" ", sep="") else "",
         if (org)    paste(cit$organization, ", ", sep="") else "",
         if (address) paste(cit$address, ", ", sep="") else "",
         if (volume)  paste("Vol. ", cit$volume, ", ", sep="") else "",
         if (year)    paste("(", cit$year, ") ", sep="") else "",
         if (note)    paste(cit$note, ".", sep="") else "", sep="")
}


lazy.toc <- function(type=c("contents", "figures", "tables"), add=FALSE, desc="",
    withPage=TRUE, sec_unit=c("chapter", "section", "subsection", "subsubsection", "part")){

  fncall <- paste("%%", paste(deparse(match.call()), collapse=" "))
  type <- match.arg(type, c("contents", "figures", "tables"))
  
  sec_unit <- match.arg(sec_unit, c("chapter", "section", "subsection", "subsubsection", "part"))
  
  if (!add){
    code <- switch(type, 
        "contents" = "\\tableofcontents",
        "figures"  = "\\listoffigures",
        "tables"   = "\\listoftables")
  }
  else{
    code <- switch(type,
        "contents" = "toc",
        "figures" = "lof",
        "tables" = "lot")
    if (withPage) code <- paste("\\addcontentsline{", code, "}{", sec_unit, "}{", desc, "}", sep="")
    else code <- paste("\\addtocontents{", code, "}{", desc, "}", sep="")
  }
  
  code <- paste(fncall, "\n", code, "\n\n")
  return(code)
}
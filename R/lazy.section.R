lazy.section <- function(heading, type=c("section", "sub", "sub2", "chapter"),
    ordered=FALSE, counter, counterSet=NULL, label=NULL){
    
  fncall <- paste("%%", paste(deparse(match.call()), collapse=" "), "\n")
  
  type <- match.arg(type, c("section", "sub", "sub2", "chapter"))
        
  counterStr <- if (!missing(counter)) lazy.counter(counter, fn="use") else "%% \\usecounter{}\n"
  if (!is.null(counterSet)) counterStr <- paste(counterStr, lazy.counter(counter, value=counterSet - 1, fn="set"), sep="\n")
  
  label <- if (!is.null(label)) lazy.label(label) else "%% \\label{}\n"

  if (type == "sub")     type <- "subsection"
  if (type == "sub2")    type <- "subsubsection"
  if (type == "chapter") type <- "chapter"
  
  star <- if(ordered) "" else "*"
  
  paste(fncall, counterStr, "\n\\", type, star, "{", heading, "}\n", label, "\n\n", sep="")
}
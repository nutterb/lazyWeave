lazy.file.start <-
function(docClass="article", packages=NULL, 
    counters=NULL, layout="", page="arabic", ligatures=TRUE,
    title=NULL, author=NULL, date=""){
    
  fncall <- paste("%%", paste(deparse(match.call()), collapse=" "))
#*** Automatically uses packages graphicx (for importing graphics) 
#*** and soul (for the \ul function)

#*** Build a string for loading user designated packages    
  packages <- if (!is.null(packages)) paste(paste("\\usepackage{", packages, "}", sep=""), collapse="\n") else "%% \\usepackage{}"
      

#*** Build a string to initialize counters
  counters <- if (!is.null(counters)) paste(paste("\\newcounter{", counters, "}", sep=""), collapse="\n") else "%% \\newcounter{}"
  
#*** Build a string to set the title and author.  No title is made when both
#*** title and author are NULL
  if (!is.null(title) | !is.null(author))
    title <- paste("\\title{", title, "}\n",
                   "\\author{", author, "}\n",
                   "\\date{", date, "}\n", 
                   "\\maketitle", sep="")
  else title <- ""

#*** Paste all the elements together to open the LATEX file.
  paste(fncall, "\n",
        "\\documentclass{", docClass, "}\n",
        "\\usepackage{graphicx}\n",
        "\\usepackage{xcolor}\n", 
        "\\usepackage{colortbl}\n",
        "\\usepackage{soul}\n",
        "\\usepackage{Sweave}\n",
        "\\usepackage{lscape}\n",
        "\\usepackage{microtype}\n",
        packages, "\n",
        if (!ligatures) "\\DisableLigatures{encoding=*, family=*}\n" else "",
        if (layout %in% "") "%% layout commands may be written here" else layout, "\n",
        counters, "\n",
        "\\begin{document}\n\n",
        lazy.page.number(page), "\n\n",
        title, sep="")
}


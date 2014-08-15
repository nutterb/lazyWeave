lazy.file.start <-
function(docClass="article", packages=NULL, 
    counters=NULL, layout="", page="arabic", ligatures=TRUE,
    title=NULL, author=NULL, date="", initialize=TRUE){
    
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html")) stop("option(\"lazyReportFormat\") must be either 'latex' or 'html'")
  
  
  #*** Construct the comment with the function call
  comment.char <- if (reportFormat == "latex") c("%%", "")
  else if (reportFormat == "html") c("<!--", "-->")
  
  fncall <- paste(comment.char[1], paste(deparse(match.call()), collapse=" "), comment.char[2], "\n")
  
  
  if (reportFormat == "latex"){
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
    code <- paste(fncall, "\n",
                  "\\documentclass{", docClass, "}\n",
                  "\\usepackage{breakurl, colortbl, fancyhdr, float, graphicx,\n", 
                  "             lastpage, lscape, microtype, soul, Sweave, url, xcolor}\n",
                  "\\usepackage[section]{placeins}\n",
                  packages, "\n",
                  if (!ligatures) "\\DisableLigatures{encoding=*, family=*}\n" else "",
                  if (layout %in% "") "%% layout commands may be written here" else layout, "\n",
                  counters, "\n",
                  "\\begin{document}\n\n",
                  lazy.page.number(page), "\n\n",
                  title, sep="")
  }
  
  #*** HTML Format
  #*** Yes, it does seem really lame, in comparison. But the HTML options
  #*** can be set using lazy.options.
  if (reportFormat == "html"){
    if (initialize){
      setHtmlOptions(table=1, figure=1, footnote=1, chapter=1, section=1, subsection=1, 
                     font.family="serif", font="helvetica", font.size=11)
      assign("HTML.FOOTNOTES", NULL, envir=options()$htmlCounters)
    }
    code <- "<html>\n"
  }
  
  if (reportFormat == "markdown"){
    setHtmlOptions(table=1, figure=1, footnote=1, chapter=1, section=1, subsection=1, 
                   font.family="serif", font="helvetica", font.size=11)
    assign("HTML.FOOTNOTES", NULL, envir=options()$htmlCounters)
    code <- ""
  }
  
  return(code)
}


lazy.figure <-
function(filename, caption=NULL, align="center",
                         height=3, width=3, units="in", 
                         counter, counterSet=NULL,
                         label=NULL, placement="h",
                         alt="Image Not Found"){

  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html")) stop("option(\"lazyReportFormat\") must be either 'latex' or 'html'")
  
  
  #*** Construct the comment with the function call
  comment.char <- if (reportFormat == "latex") c("%%", "")
                  else if (reportFormat == "html") c("<!--", "-->")
  
  fncall <- paste(comment.char[1], paste(deparse(match.call()), collapse=" "), comment.char[2], "\n")
  
  #*** LaTeX format
  if (reportFormat == "latex"){

    #*** Set align argument to latex command
    if (align %in% c("left", "right")) align <- paste("flush", align, sep="")

    #*** Set height and width strings for figure
    height <- paste("height=", height, units, sep="")
    width  <- paste("width=",  width, units, sep="")
   
    #*** Specify and set counter
    counterStr <- if (!missing(counter)) paste("  ", lazy.counter(counter, fn="use"), sep="") else "%% \\usecounter{}\n"
    if (!is.null(counterSet)) counterStr <- paste(counterStr, "\n  ", lazy.counter(counter, value=counterSet - 1, fn="set"), sep="")

    #*** Set caption and label strings
    caption <- if (is.null(caption))  "      %% \\caption{}\n"
      else paste("      \\caption{", caption, "}\n", sep="")
    label <- if (is.null(label))  "      %% \\label{}\n"
      else paste("      \\label{", label, "}\n", sep="")

    #*** Produce LATEX code for the figure.
    code <- paste(
      fncall,
      "\\begin{figure}[", placement, "]\n",
      counterStr,
      "  \\begin{", align, "}\n",
      "    \\includegraphics[",height, ", ", width, "]{", filename, "}\n",
      caption, 
      label, 
      "  \\end{", align, "}\n",
      "\\end{figure}", sep="")
  }
  
  
  #*** HTML format
  if (reportFormat == "html"){
    
    #*** Caption
    if (is.null(caption)) caption <- ""
    else{
      if (!is.null(counterSet)) lazy.counter(counter, counterSet, fn="set")
      count.val <- lazy.counter(counter, fn="value")
      caption <- paste("Figure ", lazy.counter(counter, fn="value"), ": ", caption, sep="")
      lazy.counter(counter, count.val + 1, fn="set")
    }
    
    code <- paste(fncall,
                  "<p style='text-align:", align, ";'>",
                  "<img src='", filename, "' height=", height, units, " width=", width, units, " alt='", alt, "'/></p>\n", 
                  lazy.text(caption, italic=TRUE, align=align), sep="")
    if (!is.null(label)) code <- paste(lazy.label(label), code, sep="\n") 
    code <- paste(code, "\n\n", sep="")
  }

  return(code)
}


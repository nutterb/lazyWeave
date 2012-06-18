lazy.figure <-
function(filename, caption=NULL, align="center",
                         height=3, width=3, units="in", 
                         counter, counterSet=NULL,
                         label=NULL, placement="!H"){

  fncall <- paste("%%", paste(deparse(match.call()), collapse=" "), "\n")
#*** counter arguments are currently unused.  I haven't yet 
#*** figured out how to incorporate these into the documents.

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

  return(code)
}


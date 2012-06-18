lazy.list <-
function(item, ordered=TRUE, counter=NULL, counterSet=1,
                       title=NULL, style="arabic", symbol="$\\bullet$"){
  #*** Options for style
  #*** arabic, Roman, roman, Alph, alph

  fncall <- paste("%%", paste(deparse(match.call()), collapse=" "), "\n")

#*** Print the title of the list in bold face (currently there is no way 
#*** to turn off boldface type
  if (is.null(title)) title <- "%% \\textbf{} %% List title" 
  else{ 
    title <- paste("\\textbf{", title, "}", sep="")
  }

#*** Make a string of the items in the list
  item <- paste(paste("\\item", item), collapse="\n  ")
  item[1] <- paste(" ", item[1])

#*** If no counter for an ordered list is given, this makes a new counter
  if (is.null(counter)){
    newcount <- paste(sample(LETTERS, 6), collapse="")
    code <- lazy.counter(newcount)
  }
  else code <- "%% \\newcounter{}\n"
  
#*** make string for an ordered list, else unordered list
  if (ordered){
    orderedStart <- lazy.counter(if (is.null(counter)) newcount else counter, value=counterSet - 1, fn="set")
    if (is.null(counter)){
      lst <- paste("\\begin{list}{\\", style, "{", newcount, "}}",
                   "{\\usecounter{", newcount, "}}", sep="")
    }
    else{
      lst <- paste("\\begin{list}{\\", style, "{", counter, "}}",
                   "{\\usecounter{", counter, "}}", sep="")
    } 
    code <- paste(code, title, lst, orderedStart, sep="\n")
  }
  else{
    orderedStart <- ""
    code <-paste(title, "\n\\begin{list}{", symbol, "}{}", sep="")
  }

#*** Paste code together for the list
  code <- paste(fncall, code, item, "\\end{list}", "\n\n", sep="")

  return(code)
}


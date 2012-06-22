lazy.list <-
function(item, ordered=TRUE, counter=NULL, counterSet=1, title=NULL, 
         style=c("arabic", "Roman", "roman", "Alph", "alph"), 
         symbol=c("bullet", "circ", "blacksquare"),
         font, family, size){
  #$\\bullet$, $\\circ$, $\\blacksquare$,
  
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html")) stop("option(\"lazyReportFormat\") must be either 'latex' or 'html'")
  
  style.ref <- data.frame(latex=c("arabic", "Roman", "roman", "Alph", "alph"),
                          html =c("arabic", "I",     "i",     "A",    "a"),
                          stringsAsFactors=FALSE)
  symbol.ref <- data.frame(latex=c("bullet", "circ", "blacksquare"),
                           html =c("circle", "disc", "square"),
                           stringsAsFactors=FALSE)
  
  if (missing(font)) font <- get(".HTML.FONT.FONT.", envir=.GlobalEnv)
  if (missing(family)) family <- get(".HTML.FONT.FAMILY.", envir=.GlobalEnv)
  if (missing(size)) size <- get(".HTML.FONT.SIZE.", envir=.GlobalEnv)
  
  #*** Construct the comment with the function call
  comment.char <- if (reportFormat == "latex") c("%%", "")
  else if (reportFormat == "html") c("<!--", "-->")
  
  fncall <- paste(comment.char[1], paste(deparse(match.call()), collapse=" "), comment.char[2], "\n")  
  
  #*** Options for style
  #*** arabic, Roman, roman, Alph, alph
  style <- match.arg(style, c("arabic", "Roman", "roman", "Alph", "alph"))
  symbol <- match.arg(symbol, c("bullet", "circ", "blacksquare"))
  
  if (reportFormat == "latex"){
    symbol <- paste("$\\", symbol, "$", sep="")

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
  }
  
  if (reportFormat == "html"){
    #*** Match style and symbol from latex arguments to html arguments
    style <- style.ref[style.ref$latex == style, "html"]
    symbol <- symbol.ref[symbol.ref$latex == symbol, "html"]
    if (length(symbol) == 0) symbol <- "circle"
    
    #if (is.null(counter)) lazy.counter("html.counter.list", fn="set", value=1)

    #*** Print the title of the list in bold face (currently there is no way 
    #*** to turn off boldface type
    if (is.null(title)) title <- "<!-- List title -->" 
    else{ 
      title <- lazy.text.format(title, bold=TRUE)
    }
    
    tag <- if (ordered) "ol" else "ul"
    type <- if (ordered) style else symbol
    if (type %in% "arabic") type <- ""
    
    code <- paste("<", tag, 
                  " start='", if (is.null(counter)) 1 else lazy.counter(counter, fn="value"), 
                  "' type='", type, "' ",
                  "style='font-family:", font, ", ", family, "; font-size:", size, "pt;'>", title, sep="")
    lst <- paste("  <li>", item)
    lst <- paste(lst, collapse="\n")
    
    code <- paste(fncall, code, "\n", lst, "\n</", tag, ">\n\n", sep="")  
  }

  return(code)
}


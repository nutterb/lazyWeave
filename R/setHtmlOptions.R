setHtmlOptions <- function(table=NULL, figure=NULL, footnote=NULL,
                         chapter=NULL, section=NULL, subsection=NULL,
                         subsubsection=NULL, 
                         font.family=NULL, font=NULL, font.size=NULL){
  if (!is.null(table)) assign("HTML.COUNTER.TABLE", table, envir=htmlCounters)
  if (!is.null(figure)) assign("HTML.COUNTER.FIGURE", figure, envir=htmlCounters)
  if (!is.null(footnote)) assign("HTML.COUNTER.FOOTNOTE", footnote, envir=htmlCounters)
  if (!is.null(chapter)) assign("HTML.COUNTER.CHAPTER", chapter, envir=htmlCounters)
  if (!is.null(section)) assign("HTML.COUNTER.SECTION", section, envir=htmlCounters)
  if (!is.null(subsection)) assign("HTML.COUNTER.SUBSECTION", subsection, envir=htmlCounters)
  if (!is.null(subsubsection)) assign("HTML.COUNTER.SUBSUBSECTION", subsubsection, envir=htmlCounters)
  if (!is.null(font.family)) assign("HTML.FONT.FAMILY", font.family, envir=htmlCounters)
  if (!is.null(font)) assign("HTML.FONT.FONT", font, envir=htmlCounters)
  if (!is.null(font.size)) assign("HTML.FONT.SIZE", font.size, envir=htmlCounters)
}

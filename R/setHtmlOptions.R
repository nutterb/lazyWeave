setHtmlOptions <- function(table=NULL, figure=NULL, footnote=NULL,
                         chapter=NULL, section=NULL, subsection=NULL,
                         subsubsection=NULL, 
                         font.family=NULL, font=NULL, font.size=NULL){
  if (!is.null(table)) assign("HTML.COUNTER.TABLE", table, envir=options()$htmlCounters)
  if (!is.null(figure)) assign("HTML.COUNTER.FIGURE", figure, envir=options()$htmlCounters)
  if (!is.null(footnote)) assign("HTML.COUNTER.FOOTNOTE", footnote, envir=options()$htmlCounters)
  if (!is.null(chapter)) assign("HTML.COUNTER.CHAPTER", chapter, envir=options()$htmlCounters)
  if (!is.null(section)) assign("HTML.COUNTER.SECTION", section, envir=options()$htmlCounters)
  if (!is.null(subsection)) assign("HTML.COUNTER.SUBSECTION", subsection, envir=options()$htmlCounters)
  if (!is.null(subsubsection)) assign("HTML.COUNTER.SUBSUBSECTION", subsubsection, envir=options()$htmlCounters)
  if (!is.null(font.family)) assign("HTML.FONT.FAMILY", font.family, envir=options()$htmlCounters)
  if (!is.null(font)) assign("HTML.FONT.FONT", font, envir=options()$htmlCounters)
  if (!is.null(font.size)) assign("HTML.FONT.SIZE", font.size, envir=options()$htmlCounters)
}

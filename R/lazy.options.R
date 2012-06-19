lazy.options <- function(reportFormat=c("latex", "html"),
                         table=NULL, figure=NULL, footnote=NULL,
                         chapter=NULL, section=NULL, subsection=NULL,
                         subsubsection=NULL, 
                         font.family=NULL, font=NULL, font.size=NULL){
  reportFormat <- match.arg(reportFormat, c("latex", "html"))
  options("lazyReportFormat" = reportFormat)
  if (!is.null(table)) options("html.counter.table" = table)
  if (!is.null(figure))options("html.counter.figure" = figure)
  if (!is.null(footnote))options("html.counter.footnote" = footnote)
  if (!is.null(chapter))options("html.counter.chapter" = chapter)
  if (!is.null(section))options("html.counter.section" = section)
  if (!is.null(subsection))options("html.counter.subsection" = subsection)
  if (!is.null(subsubsection))options("html.counter.subsubsection" = subsubsection)
  if (!is.null(font.family))options("html.font.family" = font.family)
  if (!is.null(font))options("html.font.font" = font)
  if (!is.null(font.size))options("html.font.size" = font.size)
}
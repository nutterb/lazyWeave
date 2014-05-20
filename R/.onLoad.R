.onLoad <- function(libname, pkgname){
  options(lazyReportFormat = "latex")
  options(lazyReportCounters = list(HTML.COUNTER.TABLE = 1,
                                      HTML.COUNTER.FIGURE = 1,
                                      HTML.COUNTER.FOOTNOTE = 1,
                                      HTML.COUNTER.CHAPTER = 1, 
                                      HTML.COUNTER.SECTION = 1,
                                      HTML.COUNTER.SUBSECTION = 1, 
                                      HTML.COUNTER.SUBSUBSECTION = 1, 
                                      HTML.FOOTNOTES = NULL, 
                                      HTML.FONT.FAMILY = "serif",
                                      HTML.FONT.FONT = "helvetica", 
                                      HTML.FONT.SIZE = 11))
}

.onUnload <- function(libname, pkgname){
  options(lazyReportFormat = NULL)
  options(lazyReportCounter = NULL)
}

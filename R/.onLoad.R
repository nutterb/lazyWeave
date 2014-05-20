.onLoad <- function(libname, pkgname){
  options("lazyReportFormat" = "latex")
  htmlCounters <- new.env(parent=.GlobalEnv)
  assign("HTML.COUNTER.TABLE", 1, envir=htmlCounters)
  assign("HTML.COUNTER.FIGURE", 1, envir=htmlCounters)
  assign("HTML.COUNTER.FOOTNOTE", 1, envir=htmlCounters)
  assign("HTML.COUNTER.CHAPTER", 1, envir=htmlCounters)
  assign("HTML.COUNTER.SECTION", 1, envir=htmlCounters)
  assign("HTML.COUNTER.SUBSECTION", 1, envir=htmlCounters)
  assign("HTML.COUNTER.SUBSUBSECTION", 1, envir=htmlCounters)
  assign("HTML.FOOTNOTES", NULL, envir=htmlCounters)
  assign("HTML.FONT.FAMILY", "serif", envir=htmlCounters)
  assign("HTML.FONT.FONT", "helvetica", envir=htmlCounters)
  assign("HTML.FONT.SIZE", 11, envir=htmlCounters)
}

.onUnload <- function(libname, pkgname){
  rm(htmlCounters)
}

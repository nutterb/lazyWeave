.onLoad <- function(libname, pkgname){
  options("lazyReportFormat" = "latex")
  assign(".HTML.COUNTER.TABLE.", 1, envir=.GlobalEnv)
  assign(".HTML.COUNTER.FIGURE.", 1, envir=.GlobalEnv)
  assign(".HTML.COUNTER.FOOTNOTE.", 1, envir=.GlobalEnv)
  assign(".HTML.COUNTER.CHAPTER.", 1, envir=.GlobalEnv)
  assign(".HTML.COUNTER.SECTION.", 1, envir=.GlobalEnv)
  assign(".HTML.COUNTER.SUBSECTION.", 1, envir=.GlobalEnv)
  assign(".HTML.COUNTER.SUBSUBSECTION.", 1, envir=.GlobalEnv)
  assign(".HTML.FOOTNOTES.", NULL, envir=.GlobalEnv)
  assign(".HTML.FONT.FAMILY.", "serif", envir=.GlobalEnv)
  assign(".HTML.FONT.FONT.", "helvetica", envir=.GlobalEnv)
  assign(".HTML.FONT.SIZE.", 11, envir=.GlobalEnv)
}
lazy.insert.code <- function(file, prompt.symbol=options()$prompt, lines){
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html")) stop("option(\"lazyReportFormat\") must be either 'latex' or 'html'")
  
  file <- readLines(file)
  if (missing(lines)) lines <- 1:length(file)
  file <- paste(paste(prompt.symbol, file[lines], collapse=if (reportFormat == "latex") "\n" else "<br>"))
  if (reportFormat == "html") file <- gsub(" ", "&nbsp ", file)
  paste(lazy.verbatim.start(), file, lazy.verbatim.end(), sep="\n")
}
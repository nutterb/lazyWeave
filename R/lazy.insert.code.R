lazy.insert.code <- function(file, prompt.symbol=options()$prompt, lines){
  #*** retrieve the report format
  reportFormat <- getOption("lazyReportFormat")
  if (!reportFormat %in% c("latex", "html", "markdown")) stop("option(\"lazyReportFormat\") must be either 'latex', 'html', or 'markdown'")
  
  collapse.char = if (reportFormat == "latex") "\n" else if (reportFormat == "html") "<br>" else "\n\n"
  
  file <- readLines(file)
  if (missing(lines)) lines <- 1:length(file)
  if (reportFormat=='markdown') file <- paste("`", file, "`", sep="")
  file <- paste(paste(prompt.symbol, file[lines], collapse=collapse.char))
  if (reportFormat == "html") file <- gsub(" ", "&nbsp ", file)
  paste(lazy.verbatim.start(), file, lazy.verbatim.end(), sep="\n")
}
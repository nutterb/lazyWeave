lazy.insert.code <- function(file, prompt.symbol=options()$prompt, lines){
  file <- readLines(file)
  if (missing(lines)) lines <- 1:length(file)
  file <- paste(paste(prompt.symbol, file[lines], collapse="\n"))
  paste(lazy.verbatim.start(), file, lazy.verbatim.end(), sep="\n")
}
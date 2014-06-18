split_ctable <- function(x, max.rows=35, ...){
  rowList <- list()
  rows <- 1:nrow(x)
  if (length(rows) < max.rows) rowList <- list(rows)
  else {
    while(length(rows) > max.rows){
      lastRow <- if (!is.na(x$name[rows[max.rows] + 1])) max.rows else max(which(!is.na(x$name[rows[1]:rows[max.rows]]))) - 1
      rowList <- c(rowList, list(rows[1]:rows[lastRow]))
      rows <- rows[-c(1:lastRow)]
      if (length(rows) < max.rows) rowList <- c(rowList, list(rows[1]:tail(rows, 1)))
    }
  }
  Splits <- lapply(rowList, function(t) x[t, ])
  do.call("paste", list(unlist(lapply(Splits, write.ctable, ...)), collapse="\\clearpage"))
}

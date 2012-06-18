lazy.page.number <- function(num_style = c("arabic", "roman", "Roman", "alph", "Alph")){
  num_style <- match.arg(num_style, c("arabic", "roman", "Roman", "alph", "Alph"))
  paste("\\pagenumbering{", num_style, "}\n\n", sep="")
}
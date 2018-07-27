
#'@export
#'
#'
rm_tail_blank_line <- function(fl){

  lines <- readLines(fl)

  fileConn <- file(as.character(fl))

  writeLines(paste(lines, collapse = "\n"),sep="",fileConn)

  close(fileConn)

}

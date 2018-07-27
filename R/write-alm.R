
#' @export
write.alm <- function(dt,file.name){
  write.table(dt,file=file.name,quote = F,row.names = F,col.names = T)
  rm_tail_blank_line(file.name)
}

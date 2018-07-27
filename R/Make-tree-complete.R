
#' @export
#'
Make.tree.complete <- function(allLin) {

  allLin <- c(allLin,Find.addNode(allLin))
  return(allLin)

}

#' Make an incompleted tree completed
#' @param  allLin incompleted all lineages.
#' @return completed all lineages.
#' @export
#'
Make.tree.complete <- function(allLin) {

  allLin <- c(allLin,Find.addNode(allLin))
  return(allLin)

}


#' Find the missing nodes, including leaves and internal nodes.
#' @export
#'


Find.addNode <- function(allLin){

  addNode <- c()

  repeat{

    incomBr.1 <- allLin %>% Find.missed.sister();
    incomBr.2 <- allLin %>% Find.missed.mother();
    incomBr <- unique(c(incomBr.1,incomBr.2));

    if(incomBr %>% length()== 0) break

    addNode <- c(addNode,incomBr)
    allLin <- unique(c(allLin,incomBr));


  }

  return(addNode)

}

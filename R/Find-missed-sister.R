
#' Find missed sister in all lineaged tree.
#' @param  allLin the lineage vector of this tree, including internal nodes and leaves.
#' @export

Find.missed.sister <- function(allLin){

  the.root <- Find.root(allLin)

  if(testit::has_warning(Find.root(allLin))==T){

    allLin.2 <- allLin

  }else{

    allLin.2 <- allLin[allLin!=the.root]

  }


  oriTail <- allLin.2 %>% substr(nchar(.),nchar(.));
  oriHead <- allLin.2 %>% substr(1,nchar(.)-1);
  allLin.2.mirror <- paste(oriHead,ifelse(oriTail == "0","1","0"),sep="");

  the.missed.sister <- dplyr::setdiff(allLin.2.mirror,allLin.2)

  return(the.missed.sister)
}

#' Find unpaired tips of an incompleted tree all lineage.
#' @param  allLin all lineage
#' @return unpaired tips
#' @export

Find.unpaired.tip <- function(allLin){

  the.root <- Find.root(allLin)

  if(testit::has_warning(Find.root(allLin))==T){

    allLin.2 <- allLin

  }else{

    allLin.2 <- allLin[allLin!=the.root]

  }

  addNode <-
    c(allLin,allLin %>% Find.missed.mother()) %>%
    unique() %>%
    Find.missed.sister()


  if(length(addNode)!=0){


    oriTail <- addNode %>% substr(nchar(.),nchar(.));
    oriHead <- addNode %>% substr(1,nchar(.)-1);
    the.unpaired.tip <- paste(oriHead,ifelse(oriTail == "0","1","0"),sep="");

    return(the.unpaired.tip)
  }else{

    warning("There is no unpaired tip")

  }



}

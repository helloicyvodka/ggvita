
#'@export

Find.tips <- function(allLin){


  the.root <- Find.root(allLin)

  if(testit::has_warning(Find.root(allLin))==T){

    allLin.2 <- allLin

  }else{

    allLin.2 <- allLin[allLin!=the.root]

  }


  the.allMother <- allLin.2 %>% substr(1,nchar(.)-1) %>% unique()

  the.tips <- dplyr::setdiff(allLin.2,the.allMother)

  return(the.tips)

}

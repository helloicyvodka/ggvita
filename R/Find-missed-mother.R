
#' @export

Find.missed.mother <- function(allLin){

  the.root <- Find.root(allLin)

  if(testit::has_warning(Find.root(allLin))==T){

    allLin.2 <- allLin

  }else{

    allLin.2 <- allLin[allLin!=the.root]

  }

  all.missed <- c()


  repeat{



    allMother.2 <- allLin.2 %>% substr(1,nchar(.)-1)

    the.missed.mother <- dplyr::setdiff(allMother.2,allLin.2)
    the.missed.mother <- dplyr::setdiff(the.missed.mother,
                                        ifelse(the.root %in% c("Root","root"),"",the.root))

    if(length(the.missed.mother)==0)break

    allLin.2 <- c(allLin.2,the.missed.mother) %>% unique()
    all.missed <- c(all.missed,the.missed.mother) %>% unique()

  }


  return(all.missed)
}

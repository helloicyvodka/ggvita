

#' Find the root from allLin
#' @param  allLin all lineage
#' @return root of this tree
#' @export

Find.root <- function(allLin){

  the.ava.roots <- c("Root","root","")

  if(allLin[allLin %in% the.ava.roots] %>% length()  >1){

    stop("The lineage sequence includes abnormal roots!")
  }

  if("Root" %in% allLin){

    the.root <- "Root"

  }else if( "root" %in% allLin){

    the.root <- "root"

  }else{

    the.min.depth <- allLin %>% nchar() %>% min()
    the.potential.root <- allLin[nchar(allLin)==the.min.depth]
    the.potential.root.son <- allLin[nchar(allLin)==the.min.depth+1]

    if(length(the.potential.root)==2){
      the.root <- the.potential.root %>% unique() %>% substr(1,nchar(.)-1) %>% unique()
      warning("This lineage sequence doesn't include the root lineage!")
    }else if(length(the.potential.root)==1){
      if(length(the.potential.root.son)==2){
        the.root <- the.potential.root
      }else{stop("The tree doesn't have a normal root!")}
    }else{stop("The tree doesn't have a normal root!")}


  }

  return(the.root)

}

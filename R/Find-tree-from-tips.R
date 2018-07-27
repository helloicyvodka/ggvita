#'@export
#'
Find.tree.from.tips <- function(allTips){

  the.above.nodes <- allTips
  repeat{


    if(the.above.nodes %>% length()>1){

      the.above.nodes <- the.above.nodes %>% substr(1,nchar(.)-1) %>% unique()

      allTips <- c(allTips,the.above.nodes) %>% unique()

    }else{break}



  }
  allTips
}

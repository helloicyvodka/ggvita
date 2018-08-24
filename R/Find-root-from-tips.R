
#' Find root from tips
#' @param LeafLin Lineage of tips
#' @export

Find.root.from.tips <- function(LeafLin){
  
  the.Mini.Len <- min(nchar(LeafLin))
  
  for(i in the.Mini.Len:0){
    
    the.root <- unique(substr(LeafLin,1,i))
    
    if(length(the.root)==1){
      
      return(the.root)
      
    }
    
  }
}
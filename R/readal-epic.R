

#' Read in a gene expression level file on the tree downloaded from EPIC
#'
#' @param expr_file a file downloaded from EPIC dataset, usually ending with .csv
#' @param mc.cores This function is very slow. Suggest to use a few cores.
#' @return a data.frame contains 4 columns: cell,time,blot,Lineage
#'

readal.epic<-function(expr_file,mc.cores=1){

  # read in the expression file and pick up the colmuns: cell, time, blot

  epic.df <- readr::read_csv(file=expr_file,
                      col_names=T,
                      col_types="cciiiiiidiiii")

  epic.df <- data.frame(epic.df,stringsAsFactors = F)

  if(!all(c("cell","time","blot") %in% colnames(epic.df))){
    stop("col_names are not in colnames(the_gene_exprfile)!")
  }


  epic.df$node.seq<-LN_to_Bin(epic.df$cell,mc.cores=mc.cores)

  return(epic.df)

}

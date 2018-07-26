

alml_to_phylo <- function( alml_list , result.order){

  one.result <- alml_list[[ as.character( result.order ) ]]

  tree.S <- tr_to_phylo( alml_list, result.order, SorT='S')
  tree.T <- tr_to_phylo( alml_list, result.order, SorT='T')


  cell.type.treeS <- unique(attr( alml_list,"params")$fileS$Class)
  cell.type.treeT <- unique(attr( alml_list,"params")$fileT$Class)

  cell.types.two.tree <- union(cell.type.treeS, cell.type.treeT)

  label.list <- c(grDevices::rainbow( length(cell.types.two.tree )))

  names(label.list)<- cell.types.two.tree

  tree.S$tr_df$tip.fill<-label.list[tree.S$tr_df$Class]
  tree.T$tr_df$tip.fill<-label.list[tree.T$tr_df$Class]


  result <- list(ori.result = one.result,
                 tree.S     = tree.S,
                 tree.T     = tree.T
                 )
  result

}


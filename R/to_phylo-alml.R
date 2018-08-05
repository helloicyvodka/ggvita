

alml_to_phylo <- function( alml_list , result.order){

  one.result <- alml_list[[ as.character( result.order ) ]]

  tree.S <- tr_to_phylo( alml_list, result.order, SorT='S')
  tree.T <- tr_to_phylo( alml_list, result.order, SorT='T')


  cell.type.treeS <- unique(attr( alml_list,"params")$fileS$Class)
  cell.type.treeT <- unique(attr( alml_list,"params")$fileT$Class)

  cell.types.two.tree <- union(cell.type.treeS, cell.type.treeT) %>% sort()


  label.list <- grDevices::rainbow(length(cell.types.two.tree))

  dt <-data.frame(x=cell.types.two.tree,y=cell.types.two.tree,Fill=label.list,stringsAsFactors = F)

  p <-
    ggplot2::ggplot(dt)+
    ggplot2::geom_point(ggplot2::aes(x=x,y=y,color=I(Fill)),size=2,show.legend =T)+
    ggplot2::scale_color_manual(values = label.list,labels=cell.types.two.tree,breaks=label.list,name="Class")+
    ggplot2::theme_classic()

  p.legend <- cowplot::get_legend(p)

  names(label.list)<- cell.types.two.tree

  tree.S$tr_df$tip.fill<-label.list[tree.S$tr_df$Class]
  tree.T$tr_df$tip.fill<-label.list[tree.T$tr_df$Class]


  result <- list(ori.result = one.result,
                 tree.S     = tree.S,
                 tree.T     = tree.T,
                 legend     = p.legend
                 )
  result
}


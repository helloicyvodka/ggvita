#' @title geom_pruni
#'
#'





add_pruning_length<-function(tree,size=3){

  #tree<-pp2$plot$ggS

  parent_of_pruning<-tree$data2[tree$data2$matched_or_pruned=="pruned",]$parent

  the_data<-tree$data2[tree$data2$node %in% parent_of_pruning,]

  the_tips<-names(alm_label)

  the_data$subtree_num<-lapply(the_data$node.seq,function(x){
    y<-all_cells[startsWith(all_cells,x)] %>% length()
    #y<-y*2-1
    y
  }) %>% unlist()


  tree<-tree+geom_nodepoint(color="red",fill="white",size=size+1,shape=21,data=the_data)


  tree<-tree+geom_text(aes(label=subtree_num),data=the_data,size=size,color="red")

  tree$data<-tree$data[tree$data$matched_or_pruned!="pruned",]

  tree


}





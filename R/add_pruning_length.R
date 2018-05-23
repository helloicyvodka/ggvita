add_prune<-function(tree,size=3){


  tree$data

  parent_of_pruning<-tree$data2[tree$data2$matched_or_pruned=="pruned",]$parent

  the_data<-tree$data[tree$data$node %in% parent_of_pruning,]


  all_cells<- tree$data2$node.seq

  the_data$subtree_num<-lapply(the_data$node.seq,function(x){
    y<-all_cells[startsWith(all_cells,x)] %>% length()
    y
  }) %>% unlist()


  tree<-tree+geom_nodepoint(color="red",fill="white",size=size+1,shape=21,data=the_data)


  tree<-tree+geom_nodelab(aes(label=subtree_num),data=the_data,size=size,color="red")

  tree$data<-tree$data[tree$data$matched_or_pruned!="pruned",]

  tree


}





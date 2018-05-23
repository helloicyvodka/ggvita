#alml_list <- readal("/mnt/data/home/phil/acting/treeComparison/code/DELTA/fun.alml","./data/fun.alm","./data/fun.alm")
# tmp <- alml_to_phylo(alml_list,10)

# trr <- list(edge = matrix(c(2, 1), 1, 2), tip.label = "a", Nnode = 1L)
# class(trr) <- "phylo"
# str(trr)

alml_to_phylo <- function( alml_list , result.order){

  one.result <- alml_list[[ as.character( result.order ) ]]

  tree.S <- tr_to_phylo( alml_list, result.order, SorT='S')
  tree.T <- tr_to_phylo( alml_list, result.order, SorT='T')


  cell.type.treeS <- unique(attr( alml_list,"params")$fileS$Class)
  cell.type.treeT <- unique(attr( alml_list,"params")$fileT$Class)

  cell.types.two.tree <- union(cell.type.treeS, cell.type.treeT)

  label.list <- c(rainbow( length(cell.types.two.tree )))

  names(label.list)<- cell.types.two.tree

  tree.S$tr_df$tip.fill<-label.list[tree.S$tr_df$Class]
  tree.T$tr_df$tip.fill<-label.list[tree.T$tr_df$Class]


  result <- list(ori.result = one.result,
                 tree.S     = tree.S,
                 tree.T     = tree.T
                 )
  result

}



tr_to_phylo <- function( alml_list, result.order,SorT){

  one.result <- alml_list[[ as.character( result.order ) ]]

  tr.root <- one.result [[ paste0("Root",SorT)]]

  tr.match <-  one.result [[ paste0("Match",SorT)]]

  tr.prune <-  one.result [[ paste0("Prune",SorT)]]

  tr <- list( root  = tr.root,
              match = tr.match,
              prune = tr.prune
  )

  if(length(na.omit(tr.prune))!=0){
    tr.prune.sister <- as.character(sapply(tr.prune, function(x){

      x.parent <- get_parent(x)

      x.last <- substr(x, nchar(x) , nchar(x) )

      l <- list("1"="0", "0"="1")

      x.sister <- paste0(x.parent, l[[x.last]] )

      as.character(x.sister)

    }))

    tr$prune.sister <- tr.prune.sister

  }






  tr_df <- data.frame(node.seq=as.character(na.omit(c(tr$root,
                                                      tr$match))),
                      stringsAsFactors = F)



  tr_df$parent.seq <- sapply(tr_df$node.seq,function(x){


    if(!(x %in% tr.root)){

      repeat{

        x <- substr(x, 1, (nchar(x) - 1))


        if (x == "") {
          x <- "Root"
          break
        }

        if(x %in% tr_df$node.seq){
          break
        }

        if(nchar(x)==1){
          break
        }

      }

    }


    x

  })


  tr_df$mp <- sapply(tr_df$node.seq,function(x){

    if(x == tr$root){
      y <- "root"
    }
    else{
      y <- "matched"
    }

    y
  })



  tr_df$mp.order <- sapply(tr_df$node.seq,function(x){

   y <- which(c(tr$root,tr$match)==x)

   y

  })




  tr$tips <- setdiff(tr_df$node.seq,
                     tr_df$parent.seq)




  tr_df$isTip <- sapply(tr_df$node.seq,function(x){

   x %in% tr$tips

  })





  tr$nodes <- setdiff(tr_df$node.seq,
                      tr$tips)




  tr_df$node <- sapply(tr_df$node.seq,function(x) {
    if (x %in% tr$tips) {
      x.order <- which(tr$tips == x)
    }
    if (x %in% tr$nodes) {
      x.order <- which(tr$nodes == x) + length(tr$tips)
    }
    x.order
  }
  )






  tr_df$parent <- sapply(1:nrow(tr_df),function(i){

    r <- tr_df[i,]
    r.parent <- tr_df[tr_df$node.seq==r$parent.seq,]

    if(r$mp=="root"){
      y <- r$node
    }else{
      y <- r.parent$node
    }

    if(length(y)==0){

        stop("Existing a node/tip (not the root) without parent!")

      }

    y

  })






  file.tree <- attr( alml_list,"params")[[as.character(paste0("file",SorT))]]

  tr_df<-merge(tr_df,file.tree[file.tree$Lineage %in% tr_df$node.seq,],by.x = "node.seq", by.y = "Lineage",all.x=T)








  tr_df$label <- sapply(1:nrow(tr_df),function(x){

    r <- tr_df[x,]

    if(r$isTip==T){
      if(is.na(x)==T){
        y <-NA
      }else{
        y<-r$Class
      }

    }else{y <-NA}

    y
  })






  tr_df$edge.length <- sapply( 1:nrow(tr_df), function(i){


    r <- tr_df[i,]

    edge.length <- nchar(r$node.seq)-nchar(r$parent.seq)

    edge.length

  })





  phylo <- list()

  phylo$edge <- matrix( cbind ( tr_df[tr_df$mp!="root",]$parent, tr_df[tr_df$mp!="root",]$node), ncol = 2)

  phylo$tip.label <- as.character(sapply(1:nrow(tr_df[tr_df$isTip==T,]),function(i){

    r <- tr_df[tr_df$node == i,]
    r$label

  }))

  phylo$Nnode <- as.integer(length(unique(tr$nodes)))

  phylo$edge.length <- tr_df$edge.length[-1]

  phylo$root.edge <- 1

  class(phylo) <- "phylo"

  return(list( phylo = phylo,
               tr_df = tr_df ))

}

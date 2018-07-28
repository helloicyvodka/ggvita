

tr_to_phylo <- function( alml_list, result.order,SorT){




  one.result <- alml_list[[ as.character( result.order ) ]]

  tr.root <- one.result [[ paste0("Root",SorT)]]

  tr.match <-  one.result [[ paste0("Match",SorT)]]

  tr.prune <-  one.result [[ paste0("Prune",SorT)]]

  tr <- list( root  = tr.root,
              match = tr.match,
              prune = tr.prune
  )



  tr_df <- data.frame(node.seq.ori=as.character(na.omit(c(tr$root,
                                                      tr$match))),
                      stringsAsFactors = F)

  NewAll.df  <-
  c(tr_df$node.seq.ori,
    tr_df$node.seq.ori %>% Find.missed.mother()) %>%
    Find.tips() %>%
    ReLin %>%
    `$`(All)

  colnames(NewAll.df) <- c("node.seq","node.seq.ReLin")

  NewAll.list <- NewAll.df$node.seq.ReLin

  names(NewAll.list) <- NewAll.df$node.seq


  tr_df$node.seq <- sapply(tr_df$node.seq.ori,function(x){

    NewAll.list[as.character(x)]

  })



  tr_df$parent.seq <- sapply(tr_df$node.seq,function(x){


    if(!(x %in% tr.root)){

      repeat{

        x <- substr(x, 0, (nchar(x) - 1))

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



  tr_df$mp.order <- sapply(tr_df$node.seq.ori,function(x){

    y <- which(c(tr$root,tr$match)==x)

    y

  })




  tr$tips <-
    dplyr::setdiff(tr_df$node.seq,tr_df$parent.seq) %>%
    sort(decreasing = T)




  tr_df$isTip <- sapply(tr_df$node.seq,function(x){

    x %in% tr$tips

  })



  tr$nodes <- dplyr::setdiff( tr_df$node.seq, tr$tips )




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

  tr_df<-merge(tr_df,file.tree[file.tree$Lineage %in% tr_df$node.seq.ori,],by.x = "node.seq.ori", by.y = "Lineage",all.x=T)



  tr_df <- tr_df %>% arrange(mp.order)





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


  tr_df <- tr_df %>% arrange(mp.order)


  phylo <- list()

  phylo$edge <- matrix( cbind ( tr_df[tr_df$mp!="root",]$parent, tr_df[tr_df$mp!="root",]$node), ncol = 2)

  phylo$tip.label <- as.character(sapply(1:nrow(tr_df[tr_df$isTip==T,]),function(i){

    r <- tr_df[tr_df$node == i,]
    r$label

  }))

  phylo$Nnode <- as.integer(length(unique(tr$nodes)))

  #phylo$edge.length <- tr_df$edge.length[-1]

  phylo$root.edge <- 1

  class(phylo) <- "phylo"

  return(list( phylo = phylo,
               tr_df = tr_df ))

}

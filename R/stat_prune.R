
cal_prune <- function(data){

  pr.seq <- attr(data,"prune")


  if(length(pr.seq)!=0){


    tr <- attr(data,"file.tree")

    pr.df <- data.frame(pr.seq=pr.seq,stringsAsFactors = F)

    pr.df$pr.num <- sapply(pr.df$pr.seq,function(x){

      l <- startsWith(tr$Lineage,as.character(x))

      y <- length(l[l==T])

      y <- 2*y-1

      return(y)

    })

    pr.df$node.seq <- sapply(pr.df$pr.seq,function(x){

      x <- as.character(x)

      repeat{

        x <- substr(x, 1, (nchar(x) - 1))


        if (x == "") {
          x <- "Root"
          break
        }

        if(x %in% data$node.seq.ori){
          break
        }

        if(nchar(as.character(x))==1){
          break
        }

      }

       x <- data[data$node.seq.ori==x,"node.seq"] %>% as.character()

       if(length(x)!=1)stop("error in find the node!")

      return(x)
    })


    dt <- merge(pr.df,data,by="node.seq",all.x=T)

    return(dt)

  }else{warning("There is no pruned node to show!")}

}


#' Add prune node informantion to tree alignment result
#'
#' @title stat_prune
#' @param ggvita.object the ggvita object you want to add to
#' @param color  color of prune node information
#' @param size  size of prune node information
#' @param ... same with geom_segment parameters
#' @export


stat_prune <- function(ggvita.object,color="red",size=2) {

  r <- list()


  if(all(is.na(ggvita.object$data$PruneS)==F)){

    pr.data.S <- cal_prune(ggvita.object$plot$ggS$data)

    r$layer.S <- list(ggtree::geom_nodepoint(data=pr.data.S,shape=21,color=color,size=size,fill="white"),
                      ggtree::geom_nodelab(data=pr.data.S,ggplot2::aes(label=pr.num),color=color,size=size*0.75))
  }else{r$layer.S <- NULL}




  if(all(is.na(ggvita.object$data$PruneT)==F)){

  pr.data.T <- cal_prune(ggvita.object$plot$ggT$data)

  r$layer.T <- list(ggtree::geom_nodepoint(data=pr.data.T,shape=21,color=color,size=size,fill="white"),
                    ggtree::geom_nodelab(data=pr.data.T,ggplot2::aes(label=pr.num),color=color,size=size*0.75))

  }else{r$layer.T <- NULL}

  class(r) <- "ggvita"

  return(r)
}


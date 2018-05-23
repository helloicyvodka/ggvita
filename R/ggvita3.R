library(plyr)
library(dplyr)
library(ggplot2)
library(ggtree)
library(cowplot)
library(readr)



#' Show the basic structure of alignment results
#'
#' @param alml_list the variable created from readal
#' @param result.order the order of result you want to show
#' @param ... same as ggtree parameters (ggtree::ggtree())
#' @return a basic tree alignment structure

ggvita<- function(alml_list, result.order,...){

  if(as.numeric(result.order)>length(alml_list))stop("The result order is out of bound! ")

  one.result <- alml_list [[ as.character(result.order) ]]

  to.plot <- alml_to_phylo(alml_list,result.order)


  ggS <- ggtree(to.plot$tree.S$phylo,...)
  ggT <- ggtree(to.plot$tree.T$phylo,...)



  ### create the ggtree data

  ggS$data<-merge(ggS$data,to.plot$tree.S$tr_df,by=c("node","parent","isTip","label"))
  ggT$data<-merge(ggT$data,to.plot$tree.T$tr_df,by=c("node","parent","isTip","label"))






  if(length(setdiff(ggS$data$mp.order,ggT$data$mp.order))!=0){
    stop("Tips from treeS and treeT are not matched!")
  }else{


    ggT$data$y <- sapply(ggT$data$mp.order,function(x){

      ggS$data[ggS$data$mp.order==x,"y"]


    })

  }



  attr(ggS$data,"prune") <- one.result$PruneS
  attr(ggT$data,"prune") <- one.result$PruneT


  attr(ggS$data,"score.order") <- one.result$score_order
  attr(ggT$data,"score.order") <- one.result$score_order


  attr(ggS$data,"score") <- one.result$Score
  attr(ggT$data,"score") <- one.result$Score

  if(is.null(one.result$PValue)!=T){

    attr(ggS$data,"pvalue") <- one.result$PValue
    attr(ggT$data,"pvalue") <- one.result$PValue

  }


  attr(ggS,"SorT")<-"S"
  attr(ggT,"SorT")<-"T"


  attr(ggS$data,"file.tree") <- attr(alml_list,"params")$fileS
  attr(ggT$data,"file.tree") <- attr(alml_list,"params")$fileT


  plot.result <-structure(list(data=one.result,
                              toPlot=to.plot,
                              plot=list("ggS"=ggS,
                                        "ggT"=ggT)),
                         class=c("ggvita","list")
  )



  return(plot.result)


}









#' Show alignment results in classical way: including expression and prune nodes
#'
#' @param alml_list the variable created from readal
#' @param result.order the order of result you want to show
#' @param colors color gradinets. See scale_gradientn's color requirement
#' @param values the values for data distrbution adjustment.See scale_gradientn's values requirement
#' @param size size of prune nodes and expression band width
#' @param tip_size size of tips
#' @param ... same as ggtree parameters (ggtree::ggtree())
#' @return a basic tree alignment structure



ggvita.classic <- function(alml_list, result.order,expr_file,
                           values=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)/0.8,
                           colors=rainbow(11)[1:9],
                           size,
                           tip_size,
                           ...){


  p <- ggvita(alml_list,result.order,...)


  if(is.null(tip_size)==T){

    y.seq <- sort(p$plot$ggS$data$y ,decreasing = F)

    y.diff <- sapply(1:length(y.seq)-1,function(x){

      y.seq[x+1]-y.seq[x]

    })

    mini.diff <- min(unlist(y.diff))


    tip_size <- 40*(abs(mini.diff)+0.02)


  }

  g <- stat_epic(p,expr_file = expr_file,size=size)

  p<-p+g+geom_tippoint(aes(fill=I(tip.fill)),size=tip_size,shape=21,color="NA")

  p<-p+scale_color_gradientn(colors =colors,values = values )


  p<- p+stat_prune(p,size=size,color="blue")



  return(p)

}
















print.ggvita <- function(i){

  p<-ggdraw()+
    draw_plot(i$plot$ggS,0,0,0.5,1)+
    draw_plot(i$plot$ggT+scale_x_reverse(),0.5,0,0.5,1)
  print(p)
}



recale_from_bottom<-function(dtS,scale_width=1){
  dtS[dtS$isTip=="TRUE",]$y<-rank(dtS[dtS$isTip=="TRUE",]$y)*scale_width

  for(i in max(dtS$x,na.rm = T):0){
    for(i2 in 1:nrow(dtS)){
      if(is.na(dtS[i2,]$x)){
        next
      }else if(dtS[i2,]$x==i){
        if(dtS[i2,]$isTip=="FALSE"){
          dtS[i2,]$y<-mean(dtS[dtS$parent==dtS[i2,]$node,]$y,na.rm = T)
        }
      }
    }
  }

  dtS
}

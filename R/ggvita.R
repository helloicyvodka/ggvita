#' Show the basic structure of alignment results
#'
#' @param alml_list the object read from readal
#' @param result.order the order of result you want to show
#' @param trace_down_for_pruned default is True. if FALSE, ggvita will not catch the sister terminal cell type for mother of pruned terminal cells.
#' @param ... same as ggtree parameters (ggtree::ggtree()) ladderize=T
#' @return a basic tree alignment structure
#' @export

ggvita <- function(alml_list,result.order,trace_down_for_pruned=T,...){


  # bug fixing
  if (attr(alml_list,'params')$method=="g"){

    result.order <- 1

    alml_list2 <- list("1"=alml_list)

    attr(alml_list2,"params") <- attr(alml_list,"params")

    if(!is.null(alml_list[["Info"]])){
      alml_list2$Info <- alml_list[["Info"]]
    }

    class(alml_list2) <- class(alml_list)

    alml_list <- alml_list2

  }

  if(as.numeric(result.order)>length(alml_list))stop("The result order is out of bound! ")


  one.result <- alml_list [[ as.character(result.order) ]]


  to.plot <- alml_to_phylo(alml_list,result.order,trace_down_for_pruned=trace_down_for_pruned)


  ggS <- ggtree::ggtree(to.plot$tree.S$phylo,ladderize=T,...)

  ggT <- ggtree::ggtree(to.plot$tree.T$phylo,ladderize=T,...)



  ### create the ggtree data

  ggS$data<-merge(ggS$data,to.plot$tree.S$tr_df,by=c("node","parent","isTip","label"))

  ggT$data<-merge(ggT$data,to.plot$tree.T$tr_df,by=c("node","parent","isTip","label"))






  if(length(dplyr::setdiff(ggS$data$mp.order,ggT$data$mp.order))!=0){

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
                                        "ggT"=ggT,
                                        "Legend"=to.plot$legend)),
                         class=c("ggvita")
  )

  return(plot.result)


}





















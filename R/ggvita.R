#'
#' Draw trees alignment results
#'
#' @title ggvita
#'
#' @param one_result one in read_alml(...)$`result_list`.
#' @param layout one of 'rectangular', 'slanted', 'fan', 'circular', 'radial', 'equal_angle' or 'daylight'.
#' @param brach_size branch size, default is 0.2.
#' @param tip_size tip size, default is 0.5.
#' @param tiplab_size tip label size, default is 0.1.
#' @param print Logical. If TRUE, then the alml result is printed. Otherwise, not.
#' @param ... same as the function ggtree attributes. See it by help(ggtree).
#'
#' @import dplyr
#' @import data.table
#' @import rlist
#' @import parallel
#' @import ggplot2
#' @import ggtree
#' @import pipeR
#' @export
#'

ggvita <- function(one_result,
                   branch_size = 0.2,
                   branch_alpha=1,
                   tip_size = 0.5,
                   tiplab_size = 0.1,
                   print = F,
                   show.pruned=T,

                   mapping        = NULL,
                   layout         = "rectangular",
                   open.angle     = 0,
                   mrsd           = NULL,
                   as.Date        = FALSE,
                   yscale         = "none",
                   yscale_mapping = NULL,
                   ladderize      = TRUE,
                   right          = FALSE,
                   branch.length  = "branch.length",
                   ...){
  UseMethod("ggvita", one_result)
}




#' @export
ggvita.alml<- function(one_result,
                       branch_size = 0.2,
                       branch_alpha=1,
                       tip_size = 0.5,
                       tiplab_size = 0.1,
                       print = F,
                       show.pruned=T,

                       mapping        = NULL,
                       layout         = "rectangular",
                       open.angle     = 0,
                       mrsd           = NULL,
                       as.Date        = FALSE,
                       yscale         = "none",
                       yscale_mapping = NULL,
                       ladderize      = TRUE,
                       right          = FALSE,
                       branch.length  = "branch.length",
                       ...){




  the_toPlot<-alml_2_phylo(one_result)

  S_pruned<-filter(the_toPlot$treeS$nodes_order,matched_or_pruned=="pruned")$node.order

  T_pruned<-filter(the_toPlot$treeT$nodes_order,matched_or_pruned=="pruned")$node.order



  trS<-ggtree(the_toPlot$treeS$phylo_tr %>% groupOTU(focus=c(1)),
              mapping=aes(linetype=group),
              size=branch_size,
              layout = layout,
              alpha=branch_alpha,


              open.angle     = open.angle,
              mrsd           = mrsd,
              as.Date        = as.Date,
              yscale         = yscale,
              yscale_mapping = yscale_mapping,
              ladderize      = ladderize,
              right          = right,
              branch.length  = branch.length,
              ...

              )




  trS$data$group[T]<-0

  if(length(S_pruned)!=0){

    trS$data$group[trS$data$node %in% S_pruned]<-1

    }


  trT<-ggtree(the_toPlot$treeT$phylo_tr %>% groupOTU(focus=c(1)),
              mapping=aes(linetype=group),
              size=branch_size,
              layout = layout,
              alpha=branch_alpha,


              open.angle     = open.angle,
              mrsd           = mrsd,
              as.Date        = as.Date,
              yscale         = yscale,
              yscale_mapping = yscale_mapping,
              ladderize      = ladderize,
              right          = right,
              branch.length  = branch.length,
              ...
  )

  trT$data$group[T]<-0

  if(length(T_pruned)!=0){
    trT$data$group[trT$data$node %in% T_pruned]<-1
  }


  ### create the ggtree data
  dt_S<-merge(trS$data,the_toPlot$treeS$nodes_order[,c("parent.seq","parent.order","node.seq","node.order","matched_or_pruned")],by.x="node",by.y="node.order")
  #dt_S %>% View()
  dt_T<-merge(trT$data,the_toPlot$treeT$nodes_order[,c("parent.seq","parent.order","node.seq","node.order","matched_or_pruned")],by.x="node",by.y="node.order")
  #dt_T %>% View()

  matched_S_and_T<-list.parse(the_toPlot$matched_pair)


  # pruned_sister
  for(i in list.parse(filter(dt_S,matched_or_pruned=="pruned_sister"))){
    if(T){
      dt_S[dt_S$node.seq==i$parent.seq,"y"]<-dt_S[dt_S$node.seq==i$node.seq,"y"]
    }
  }



  for(i in matched_S_and_T){
    dt_T[dt_T$node.seq==i$mtT,"y"]<-dt_S[dt_S$node.seq==i$mtS,"y"]
  }

  for(i in list.parse(filter(dt_T,matched_or_pruned=="pruned_sister"))){
    dt_T[dt_T$node.seq==i$node.seq,"y"]<-dt_T[dt_T$node.seq==i$parent.seq,"y"]
  }

  for(i in list.parse(filter(dt_T,matched_or_pruned=="pruned"))%>%list.sort(parent.order)){
    sub_tips_seq<-dt_T$node.seq[unlist(lapply(dt_T$node.seq,
                                      function(x){
                                        startsWith(x,i$parent.seq)
                                        }
                                      )
                               )
                        ]

    sub_tips_seq<-setdiff(sub_tips_seq,i$node.seq)
    subtips_y_floor<-min(filter(dt_T,node.seq %in% sub_tips_seq)$y)
    subtips_y_ceiling<-max(filter(dt_T,node.seq %in% sub_tips_seq)$y)

    sister.seq<-paste0(substr(i$node.seq,1,nchar(i$node.seq)-1),
                       list("1"="0","0"="1")[[substr(i$node.seq,nchar(i$node.seq),nchar(i$node.seq))]])


    if(i$isTip==T & filter(dt_T,node.seq==sister.seq)$isTip==T){
      if(substr(i$node.seq,nchar(i$node.seq),nchar(i$node.seq))=="0"){
        dt_T[dt_T$node.seq==i$node.seq,"y"]<- dt_T[dt_T$node.seq==i$parent.seq,"y"]-0.35
      }else{
        dt_T[dt_T$node.seq==i$node.seq,"y"]<- dt_T[dt_T$node.seq==i$parent.seq,"y"]+0.35
      }
    }else{
      if(substr(i$node.seq,nchar(i$node.seq),nchar(i$node.seq))=="0"){
        dt_T[dt_T$node.seq==i$node.seq,"y"]<-subtips_y_floor-0.35
      }else{
        dt_T[dt_T$node.seq==i$node.seq,"y"]<-subtips_y_ceiling+0.35
      }
    }
}



  # dt_T$branch<-dt_T$branch/1.5
  # dt_S$branch<-dt_S$branch/1.5


  trT$data<- merge(trT$data[,-4],dt_T[,c("node","y")],by="node")
  trS$data<- merge(trS$data[,-4],dt_S[,c("node","y")],by="node")




  ###############################
  ## put two tree together and color the tips by tissue classes

  labellist2<-
    c(
    "Neu" = c("blue"),
    "Dea" = c("green"),
    "Str" = c("red"),
    "Epi" = c("orange"),
    "Mus" = c("yellow"),
    "Bla" = c("purple"),
    "Gla" = c("black"),
    "Int" = c("brown"),
    "Ger" = c("cyan"),
    "???" = c("skyblue")
  )


  trS$data$colorlabel<-labellist2[trS$data$label]
  trT$data$colorlabel<-labellist2[trT$data$label]














  oldw <- getOption("warn")
  options(warn = -1)


  ggS<-trS+
    geom_tippoint(size=tip_size,aes(fill=I(colorlabel)),shape=21,color="NA")+
    geom_tiplab(align = T,size=tiplab_size)+
    ggtitle(paste0("score_order:",one_result$score_order,"  ","score: ",one_result$Score))





  ggT<-trT+
    geom_tippoint(size=tip_size,aes(fill=I(colorlabel)),shape=21,color="NA")+
    geom_tiplab(align = T,size=tiplab_size,hjust =1 )+
    ggtitle(paste0("RootS:",one_result$RootS,"  ","RootT:",one_result$RootT))






    # Find the parent and annotation the parent

    trS_parent_of_pruned<-filter(trS$data,
                                 node %in%
                                   trS$data[trS$data$group==1,]$parent)



    trT_parent_of_pruned<-filter(trT$data,
                                 node %in%
                                   trT$data[trT$data$group==1,]$parent)

    # Collapse the pruned nodes

    ggS$data<-filter(ggS$data,group==0)
    ggT$data<-filter(ggT$data,group==0)

    # trS$data[trS$data$group==1,]$x<-NA
    # trS$data[trS$data$group==1,]$y<-NA
    # trT$data[trS$data$group==1,]$x<-NA
    # trT$data[trS$data$group==1,]$y<-NA


  if(show.pruned==T){

      ggS<-ggS+geom_point(data=trS_parent_of_pruned,
                 mapping=aes(x,
                             y
                             #size=log10(15-x)
                 ),
                 color="red",
                 size=0.5
      )



    ggT<-ggT+geom_point(data=trT_parent_of_pruned,
                 mapping=aes(x,
                             y
                             #size=log10(15-x)
                 ),
                 color="red",
                 size=0.5
      )

  }






  attr(ggS,"SorT")<-"S"
  attr(ggT,"SorT")<-"T"




  options(warn = oldw)












  this_result<-structure(list(data=one_result,
                              toPlot=the_toPlot,
                              plot=list("ggS"=ggS,
                                        "ggT"=ggT+scale_x_reverse())),
                         class=c("ggvita","list")
  )














  if(print==T){return((this_result))}
  else {return(invisible(this_result))}




}





#---------------------------------------------------------------------------------------
# print method applied for class ggvita

#' @export

print.ggvita <- function(i){
  multiplot(i$plot$ggS,
            i$plot$ggT,
            ncol = 2)
}

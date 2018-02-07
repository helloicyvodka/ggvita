#' Add EPIC data to local tree alignment result
#'
#' @title geom_EPIC
#' @param expr_file the address of the gene expressing file from EPIC dataset
#' @param expr_size expression bar width
#' @param expr_alpha expression bar alpha
#' @param mc.cores the CPU cores to use in the function
#' @param color_gradientn color gradients, a vector.
#' @param values controlling the distribution of colors, same length with color_gradientn
#' @return local tree alignment result and corresponding gene expression. Phenotype and Genotype.
#' @export
#'
#'
#'
#'
#'
#'


geom_EPIC<-function(expr_file,
                    expr_size = 1.5,
                    expr_alpha = 1,
                    mc.cores = 5,
                    color_gradientn=c("black","grey","darkblue","blue","skyblue","purple","red","orange","green","yellow"),
                    values=c(0,0.01,0.02,0.04,0.06,0.08,0.1,0.2,0.4,0.6,0.8)
){

 the_env<-new.env(parent = emptyenv())
 the_env$expr_file                 <-  readal.epic(expr_file)
 the_env$expr_size                 <-  expr_size
 the_env$expr_alpha                <-  expr_alpha
 the_env$mc.cores                  <-  mc.cores
 the_env$color_gradientn           <-  color_gradientn
 the_env$values                    <-  values
 class(the_env)<-c("geom_EPIC","geom_ggvita",class(the_env))
 the_env
}





#---------------------------------------------------------------------------------------------------
# add_ggvita.geom_EPIC is the method applied for the geom_EPIC object
#' @import plyr
#' @import dplyr


add_ggvita.geom_EPIC<-function(e1,e2,SorT){

  epic_gene_expr_simple<-(e2$expr_file)

  epic_gene_expr_simple<-epic_gene_expr_simple[,scale_blot:=scale(blot)]

  full_tr<-e1$toPlot

  full_tr2<-e1$plot

  full_tr_nodes_order<-
    full_tr[[paste0("tree",SorT)]]$nodes_order %>%
    data.table()

  setnames(full_tr_nodes_order,"node.order","node")

  full_tr_ggtree_data<-full_tr2[[paste0("gg",SorT)]]$data

  full_tr_merge<-
    merge(full_tr_ggtree_data,
          full_tr_nodes_order[,c("parent.seq","parent.order","node.seq","node")],
          by="node"
    ) %>% data.table()

  setkey(full_tr_merge,"node.seq")

  ## find each expr data its "node.x","parent.x","seg_x_start","seg_x_end","seg_y"

  epic_gene_expr_simple <- epic_gene_expr_simple %>% filter(Lineage %in%  full_tr_merge$node.seq)

  epic_gene_expr_simple$node.x  <-epic_gene_expr_simple$Lineage %>%
    mclapply(function(m){full_tr_merge[m]$"x"},mc.cores = e2$mc.cores) %>% unlist()

  ##
  epic_gene_expr_simple$parent.x<-
    epic_gene_expr_simple$Lineage %>% mclapply(
      function(m){tmp_parent_seq<-as.character(full_tr_merge[m]$parent.seq)
      full_tr_merge[node.seq==tmp_parent_seq,]$"x"
      },
      mc.cores = e2$mc.cores) %>%
    unlist()


  ## add cell time freq
  epic_gene_expr_simple_celltime_freq<-
    epic_gene_expr_simple[,"cell"] %>%
    table() %>% data.table()


  setnames(epic_gene_expr_simple_celltime_freq,c(".","N"),c("cell","time_freq"))

  epic_gene_expr_simple<-
    merge(epic_gene_expr_simple,epic_gene_expr_simple_celltime_freq,by="cell")%>%data.table()

  ##



  epic_gene_expr_simple$time_rank_in_cell<-epic_gene_expr_simple[,rank(time),by=cell]$V1


  epic_gene_expr_simple$node.x<-as.numeric(epic_gene_expr_simple$node.x)

  epic_gene_expr_simple$parent.x<-as.numeric(epic_gene_expr_simple$parent.x)

  epic_gene_expr_simple<-mutate(epic_gene_expr_simple,seg_x_start=((time_rank_in_cell-1)/ time_freq)*(node.x-parent.x)+parent.x)


  epic_gene_expr_simple<-mutate(epic_gene_expr_simple,seg_x_end=((time_rank_in_cell)/ time_freq)*(node.x-parent.x)+parent.x)


  epic_gene_expr_simple$seg_y<-
    epic_gene_expr_simple$Lineage %>%
    mclapply(function(x){
      full_tr_merge[x]$y
    },mc.cores = e2$mc.cores) %>%
    unlist()

  epic_gene_expr_simple$branch<-
    epic_gene_expr_simple$Lineage %>%
    mclapply(function(x){
      full_tr_merge[x]$branch
    },mc.cores = e2$mc.cores) %>%
    unlist()

  epic_gene_expr_simple<-epic_gene_expr_simple%>%as.data.frame()%>%data.table()



  #the_scale_blot<-epic_gene_expr_simple$scale_blot


  #EPIC_colors_gradient<-colors_gradient

  #ggtr_annotation<-full_tr2[[paste0("gg",SorT)]]+                  #change to envrionment
  # scale_color_gradientn(#colors=EPIC_colors_gradient,
  #   colors = c("black","grey","darkblue","blue","skyblue","purple","red","orange","green","yellow"),
  #   values=c(0,0.01,0.02,0.04,0.06,0.08,0.1,0.2,0.4,0.6,0.8)
  #   #rescale=F
  # )+




  ggtr_annotation<-
    full_tr2[[paste0("gg",SorT)]]+
    geom_segment(
      mapping = aes(
        x = seg_x_start,
        xend = seg_x_end,
        y = seg_y,
        yend = seg_y,
        colour = scale_blot,
        group = group
      ),
      linetype = "solid",
      size = e2$expr_size,
      alpha = e2$expr_alpha,
      data = epic_gene_expr_simple %>% mutate(group = "0")
    )+
    scale_color_gradientn(
      colors = e2$color_gradientn,
      values=  e2$values )+
    geom_tippoint(size=(full_tr2[[paste0("gg",SorT)]]$layers[[3]]$aes_params$size),aes(fill=I(colorlabel)),shape=21,color="NA")

  #class(ggtr_annotation)<-c("ggvita",class(ggtr_annotation))
  return(ggtr_annotation)
}
















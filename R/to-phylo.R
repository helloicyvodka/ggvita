#' Transform a tree seq split with space to a phylo
#'
#' @param seq the lineage name  seq of all nodes in a tree, split with space
#' @param the_root the root of the tree
#' @param SorT "S" or"T"
#' @import plyr
#' @import dplyr
#' @import data.table
#' @import rlist
#' @import parallel
#' @import ggplot2
#' @import ggtree
#' @import pipeR


tr2phylo <- function(seq,the_root,SorT) {

  tr_alm_label<-alm_label[[as.character(paste0("tree",SorT))]]

  # @ group the tips and the nodes

  the_seq <- s2v(seq)

  the_ori_seq_nb <- length(the_seq)

  #sub_seq<-unlist(lapply(seq,get_sub_seq))


  # @ test

  if (the_root != "Root") {
    if (!all(startsWith(the_seq, prefix = as.character(the_root)))) {
      stop("Existing nodes outside the subroot!")
    }
  }



  # @ make-up the tree


  repeat {
    the_outside_son <- get_outside_son(the_seq)

    the_outside_parent <- get_outside_parent(the_seq)

    the_outside_node <- union(the_outside_parent, the_outside_son)

    the_seq <- c(the_seq, the_outside_node)

    the_outside_son <- get_outside_son(the_seq)

    the_outside_parent <- get_outside_parent(the_seq)

    the_outside_node <- union(the_outside_parent, the_outside_son)


    if (length(the_outside_parent) == 0 &
        length(the_outside_son) == 0) {
      break
    }
  }




  the_parent <- unlist(lapply(the_seq[-1], get_parent))
  the_parent <- c(the_seq[1], the_parent)


  if (any(unlist(lapply(the_seq, function(x) {
    nb.branches(the_seq, x)
  })) == 1)) {
    the_single_banche_node <-
      the_seq[which(unlist(lapply(the_seq, function(x) {
        nb.branches(the_seq, x)
      })) == 1)]
    print(paste0(
      "existing nb.branches:",
      paste(the_single_banche_node, collapse = " ")
    ))
    stop("existing nb.branches==1")
  }



  the_tip <-
    the_seq[which(unlist(lapply(the_seq, function(x) {
      nb.branches(the_seq, x)
    })) == 0)]

  the_node <-
    the_seq[which(unlist(lapply(the_seq, function(x) {
      nb.branches(the_seq, x)
    })) == 2)]


  # @ give the nb.node to the seq



  get_the_order <- function(x) {
    if (x %in% the_tip) {
      x.order <- which(the_tip == x)
    }
    if (x %in% the_node) {
      x.order <- which(the_node == x) + length(the_tip)
    }
    x.order
  }

  # @ give the nb.node to the seq



  the_seq_order <- unlist(lapply(the_seq, get_the_order))

  the_parent_order <- unlist(lapply(the_parent, get_the_order))


  nodes_order <- data.frame(
    parent.seq = the_parent,
    parent.order = the_parent_order,
    node.seq = the_seq,
    node.order = the_seq_order
    ,
    stringsAsFactors = F
  )




  get_the_label <- function(x,tr_alm_label) {
    x <- as.character(x)
    if (x %in% names(tr_alm_label)) {
      x.label <- tr_alm_label[[x]]
    } else{
      x.label <- "???"
    }
    x.label

  }
  nodes_order <- nodes_order %>% mutate(label =
                                          unlist(lapply(nodes_order$node.seq, function(x) {
                                            x <- as.character(x)
                                            if (x %in% names(tr_alm_label)) {
                                              x.label <- tr_alm_label[[x]]
                                              if (length(x.label) == 0) {
                                                x.label <- "???"
                                              }
                                            } else{
                                              x.label <- "???"
                                            }
                                            x.label
                                          })))

  nodes_order <- nodes_order %>% mutate(isTip =
                                          unlist(lapply(nodes_order$node.seq, function(x) {
                                            x <- as.character(x)
                                            if (nb.branches(the_seq, x) ==
                                                2) {
                                              y <- "FALSE"
                                            }
                                            if (nb.branches(the_seq, x) ==
                                                0) {
                                              y <- "TRUE"
                                            }
                                            if (nb.branches(the_seq, x) ==
                                                1) {
                                              print("Error:nb.branches==1")
                                            }
                                            y
                                          })))
  nodes_order <-
    data.table(nodes_order[order(nodes_order$node.order),])




  # @ phylo

  phylo_tree <- list()

  phylo_tree$edge <-
    matrix(cbind(the_parent_order, the_seq_order), ncol = 2)[-1, ]

  phylo_tree$tip.label <-
    as.character(unlist(lapply(the_seq[order(the_seq_order)][1:length(the_tip)], function(x){get_the_label(x,tr_alm_label)})))

  phylo_tree$Nnode <-
    as.integer(length(unique(the_node)))

  class(phylo_tree) <- "phylo"

  return(list(phylo_tr = phylo_tree, nodes_order = nodes_order))


}## end tr2phylo function




#-----------------------------------------------------------------------------------------------------
#' Transform a alml result to phylo
#'
#'
#' @param the_result one result which is read in from alml file
#' @import plyr
#' @import dplyr
#' @import data.table
#' @import rlist
#' @import parallel
#' @import ggplot2
#' @import ggtree
#' @import pipeR


alml_2_phylo <- function(the_result) {
  # transform a result(S and T) into 2 phylos

  #print("===Transforming S to phylo===")



  if (is.na(the_result$PruneS) == F) {
    treeS <-
      tr2phylo(
        paste0(
          the_result$RootS,
          " ",
          the_result$MatchS,
          the_result$PruneS
        ),
        the_result$RootS,
        "S"
      )

  } else{
    treeS <-
      tr2phylo(paste0(the_result$RootS,
                      " ",
                      the_result$MatchS),
               the_result$RootS,
               "S")
  }

  treeS$nodes_order <- treeS$nodes_order %>%
    mutate(matched_or_pruned =
             unlist(lapply(treeS$nodes_order$node.seq, function(m) {
               m <- as.character(m)
               if (m %in% s2v(paste0(the_result$RootS, " ", the_result$MatchS))) {
                 p_or_s <- "matched"
               } else if (m %in% s2v(the_result$PruneS)) {
                 p_or_s <- "pruned"
               } else{
                 p_or_s <- "pruned_sister"
               }
               p_or_s
             })))








  #print("===Transforming T to phylo===")
  if (is.na(the_result$PruneT) == F) {
    treeT <-
      tr2phylo(
        paste0(
          the_result$RootT,
          " ",
          the_result$MatchT,
          the_result$PruneT
        ),
        the_result$RootT,
        "T"
      )

  } else{
    treeT <-
      tr2phylo(paste0(the_result$RootT, " ", the_result$MatchT),
               the_result$RootT,
               "T")
  }

  treeT$nodes_order <-
    treeT$nodes_order %>%
    mutate(matched_or_pruned =
             unlist(lapply(treeT$nodes_order$node.seq, function(m) {
               m <- as.character(m)
               if (m %in% s2v(paste0(the_result$RootT, " ", the_result$MatchT))) {
                 p_or_s <- "matched"
               } else if (m %in% s2v(the_result$PruneT)) {
                 p_or_s <- "pruned"
               } else{
                 p_or_s <- "pruned_sister"
               }
               p_or_s
             })))





  matched_pair <-
    data.table(mtS = s2v(paste0(
      the_result$RootS, " ", the_result$MatchS
    )), mtT = s2v(paste0(
      the_result$RootT, " ", the_result$MatchT
    )))

  r <- list(treeS = treeS,
            treeT = treeT,
            matched_pair = matched_pair)
  return(r)

}

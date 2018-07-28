
# 气忌盛，心忌满，才忌露

ReLin <-
  function(LeafLin) {

    if((intersect(LeafLin,LeafLin %>% substr(1,nchar(.)-1)) %>% length() )!=0)
      stop("Not all leaves!")

    Find.root.from.Leaves <- function(LeafLin){

      the.Mini.Len <- min(nchar(LeafLin))

      for(i in the.Mini.Len:0){

        the.root <- unique(substr(LeafLin,1,i))

        if(length(the.root)==1){

          return(the.root)

        }

      }
    }


    LeafLin <- as.character(LeafLin)
    the.root <- Find.root.from.Leaves(LeafLin)
    the.growing.tree <- list(c(the.root, the.root))
    the.all.tree  <- the.growing.tree

    PreNum <- length(LeafLin)


    repeat {
      the.growing.tree <- lapply(the.growing.tree, function(x) {
        # x=100
        xx1.0 <- paste0(x[1], "0")  # 1000
        xx1.1 <- paste0(x[1], "1")  # 1001
        xx2.0 <- paste0(x[2], "0")  # ...0
        xx2.1 <- paste0(x[2], "1")  # ...1


        ttt.0 <-
          xx1.0 %>%
          startsWith(LeafLin, prefix = .) %>%
          `[`(. == T) %>%
          length()


        ttt.1 <-
          xx1.1 %>%
          startsWith(LeafLin, prefix = .) %>%
          `[`(. == T) %>%
          length()
        if (ttt.0 == 0) {
          if (ttt.1 == 0)
            return(list(c(x[1], x[2])))
          if (ttt.1 >= 1)
            return(list(c(xx1.1, x[2])))
        }
        if (ttt.0 >= 1) {
          if (ttt.1 == 0)
            return(list(c(xx1.0, x[2])))
          if (ttt.1 >= 1)
            return(list(c(xx1.0, xx2.0), c(xx1.1, xx2.1)))
        }
      })  %>%
        unlist(recursive = F)

      the.all.tree <-
        unlist(list(the.all.tree,the.growing.tree),recursive = F) %>%
        unique()

      the.all.tree

      if (length(the.growing.tree) == PreNum)
        break
    }




    # Leaf

    NewLeafList <- rlist::list.map(the.growing.tree, .[2])

    names(NewLeafList) <- rlist::list.mapv(the.growing.tree, .[1])

    NewLeafLin <-
      sapply(LeafLin, function(x)as.character(NewLeafList[[x]])) %>%
      as.character()

    Leaf.df <- data.frame(LeafLin=LeafLin,NewLeafLin=NewLeafLin,stringsAsFactors = F)


    # All

    Test <- sapply(the.all.tree,function(x){x[1]==""} )

    if(any(Test)){

      the.all.tree[[which(Test ==T)]] <- c("Root","Root")

    }



    AllLin <- rlist::list.mapv(the.all.tree, .[1]) %>% as.character()

    NewAllList <- rlist::list.map(the.all.tree, .[2])

    names(NewAllList) <- AllLin

    NewAllLin <-
      sapply(AllLin, function(x)
        as.character(NewAllList[[x]])) %>% as.character()


    All.df  <- data.frame(AllLin=AllLin,NewAllLin=NewAllLin, stringsAsFactors = F)

    return(list(All=All.df,Leaf=Leaf.df))
  }







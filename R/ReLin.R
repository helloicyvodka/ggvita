
#' Renew the incomplete tree leave lineages
#' @param LeafLin incomplete leave lineages
#' @return a list inclued both new and past lineages data.frames for all and leaves
#' @export

ReLin <-
  function(LeafLin,UseSubRoot=F) {

    LeafLin <- as.character(LeafLin)

    if(length(LeafLin)==1)return(list(All=c(""),Leaf=c("")))

    if( (intersect(LeafLin,LeafLin %>% substr(1,nchar(.)-1)) %>% length())!=0)
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

    the.root <- Find.root.from.Leaves(LeafLin)

    # tree grow from the real sub root

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
      sapply(LeafLin, function(x){

        m <- sapply(names(NewLeafList),function(xx){

          if(startsWith(x,prefix = xx)==T){

            return(nchar(xx))
          }else{return(0)}

        },USE.NAMES = F)

        as.character(NewLeafList[[which(m==max(m))]])

        },USE.NAMES = F)

    #NewLeafLin <- sapply(NewLeafLin,function(x)paste0(the.root,x))

    Leaf.df <- data.frame(LeafLin=LeafLin,NewLeafLin=NewLeafLin,stringsAsFactors = F)

    InternalList <- setdiff(the.all.tree,the.growing.tree)

    All.df  <- data.frame(AllLin=c(InternalList %>% rlist::list.mapv(.[1]),Leaf.df$LeafLin),
                          NewAllLin=c(InternalList %>% rlist::list.mapv(.[2]),Leaf.df$NewLeafLin),
                          stringsAsFactors = F)

    All.df$AllLin[All.df$AllLin==""]  <- "Root"
    All.df$NewAllLin[All.df$NewAllLin==""]  <- "Root"


    if(UseSubRoot==F){

      All.df$NewAllLin <-  sub(pattern = the.root,
                               replacement = "",
                               All.df$NewAllLin)

      Leaf.df$NewLeafLin <- sub(pattern = the.root,
                                replacement = "",
                                Leaf.df$NewLeafLin)

    }


    return(list(All=All.df,
                Leaf=Leaf.df,
                NewAllLin=All.df$NewAllLin %>% unique()))
  }







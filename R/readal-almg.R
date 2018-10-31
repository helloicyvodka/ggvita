#' Read in results from DELTA calculation
#'
#' @title readal.almg
#' @author Meng Yuan
#' @param file file address of alml result (with random tree alignment pvalue)
#' @param all same with DELTA param. If T, the result will include more information: treeS, treeT, node class, score matrix.
#' @return a list containing the result (phylos of treeS and treeT, the matching dataframe) and the result analysis (pValue and etc.)
#' @export

readal.almg <- function(file,all=F){

  # file <- result.list.file[1]


  the_result<-list()

  the_prefix<-c("Score","RootS","RootT","PruneS","PruneT","MatchS","MatchT","}","PValue","Min")

  the_text<-readLines(file)

  nl<-length(the_text)

  result.rownum <- min(which(startsWith(the_text,"Score:")))

  all <- as.character(all)
  if(all=="T"|all=="TRUE"){

    treeS.rownum <- which(startsWith(the_text,"treeS"))
    treeT.rownum <- which(startsWith(the_text,"treeT"))
    NodeClass.rownum <- which(startsWith(the_text,"nodeClass"))
    ScoreMatrix.rownum <- which(startsWith(the_text,"scoreMatrix"))

    treeS.Info <- the_text[(treeS.rownum+1):(treeT.rownum-2)]
    treeS.Info <- lapply(treeS.Info,function(x){strsplit(x,split = "\t") %>% unlist()})
    treeS.df <- Reduce(rbind,treeS.Info[-1]) %>% data.frame(stringsAsFactors = F)
    colnames(treeS.df) <- treeS.Info[[1]]
    rownames(treeS.df) <- 1:nrow(treeS.df)

    treeT.Info <- the_text[(treeT.rownum+1):(NodeClass.rownum-3)]
    treeT.Info <- lapply(treeT.Info,function(x){strsplit(x,split = "\t") %>% unlist()})
    treeT.df <- Reduce(rbind,treeT.Info[-1]) %>% data.frame(stringsAsFactors = F)
    colnames(treeT.df) <- treeT.Info[[1]]
    rownames(treeT.df) <- 1:nrow(treeT.df)

    NodeClass.Info <- the_text[(NodeClass.rownum+1):(ScoreMatrix.rownum-3)]
    NodeClass.Info <- lapply(NodeClass.Info,function(x){strsplit(x,split = "\t") %>% unlist()})
    NodeClass.df <- Reduce(rbind,NodeClass.Info) %>% data.frame(stringsAsFactors = F)
    colnames(NodeClass.df) <- c("treeS.Class","treeT.Class")
    rownames(NodeClass.df) <- 1:nrow(NodeClass.df)

    ScoreMatrix.Info <- the_text[(ScoreMatrix.rownum+1):(result.rownum-2)]
    ScoreMatrix.Info <- lapply(ScoreMatrix.Info,function(x){strsplit(x,split = "\t") %>% unlist() %>% as.numeric()})
    ScoreMatrix.df <- Reduce(rbind,ScoreMatrix.Info) %>% data.frame(stringsAsFactors = F)
    colnames(ScoreMatrix.df) <- treeT.df$id
    rownames(ScoreMatrix.df) <- treeS.df$id

    the_result[["Info"]] <-
      list(treeS=treeS.df,
           treeT=treeT.df,
           NodeClass= NodeClass.df,
           ScoreMatrix = ScoreMatrix.df)

  }





  for(i in result.rownum:(nl)){

    the_line<-the_text[[i]]


    for(i2 in 1:(length(the_prefix)-3)){

      if(startsWith(the_line,the_prefix[i2])){

        the_result[[as.character(the_prefix[i2])]]<-s2v(unlist(strsplit(the_line,split = ":"))[2])

      }
    }

    #"DELTA score between random trees"

    if(startsWith(the_line,"DELTA score between random trees")){

      the_result[["PValue"]]<-list()

      the_result[["PValue"]][["all"]]<-s2v( unlist(strsplit(the_line,split = ":"))[2] )

    }

    if(startsWith(the_line,"Min")){

      the_result[["PValue"]][["Min"]]<-s2v( unlist(strsplit(unlist(strsplit(the_line,split = " "))[1],split = ":"))[2] )

      the_result[["PValue"]][["Max"]]<-unlist(strsplit(unlist(strsplit(the_line,split = " "))[2],split = ":"))[2]

      the_result[["PValue"]][["AVG"]]<-unlist(strsplit(unlist(strsplit(the_line,split = " "))[3],split = ":"))[2]

      the_result[["PValue"]][["pvalue"]]<-unlist(strsplit(unlist(strsplit(the_line,split = " "))[4],split = ":"))[2]

    }
  }

  #class(the_result) <- c("almg_list",class(the_result))

  the_result


}

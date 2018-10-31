#' Read in results from DELTA calculation
#'
#' @title readal.alml
#' @author Meng Yuan
#' @param file file address of alml result (with random tree alignment pvalue)
#' @param all same with DELTA param. If T, the result will include more information: treeS, treeT, node class, score matrix.
#' @return a list containing the result (phylos of treeS and treeT, the matching dataframe) and the result analysis (pValue and etc.)
#' @export



# import rlist
# import plyr
# import dplyr
# import ggtree


readal.alml<-function(file,all=F){

  the_result<-list()

  the_prefix<-c("Score","RootS","RootT","PruneS","PruneT","MatchS","MatchT","}","PValue","Min")

  the_text<-readLines(file)

  nl<-length(the_text)

  result.rownum <- min(which(startsWith(the_text,"1{")))

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



  for(i in result.rownum:(nl-1)){

    the_line<-the_text[[i]]


    if(regexpr("^[0-9]",the_line)==T){

      num<-as.character(regmatches(the_line,regexpr("^([0-9]+)",the_line)))

      if(startsWith(the_text[[(i+1)]],"Score")==T){

        if(length(num)>0){
          the_result[[num]]<-list()
          the_result[[num]]<-list("score_order"=as.numeric(num))
        }
      }

    }else{

      for(i2 in 1:(length(the_prefix)-3)){

        if(startsWith(the_line,the_prefix[i2])){

          the_result[[num]][[as.character(the_prefix[i2])]]<-s2v(unlist(strsplit(the_line,split = ":"))[2])

        }
      }


      if(startsWith(the_line,"DELTA score between random trees")){

        the_result[[num]][["PValue"]]<-list()

        the_result[[num]][["PValue"]][["all"]]<-s2v( unlist(strsplit(the_line,split = ":"))[2] )

      }

      if(startsWith(the_line,"Min")){

        the_result[[num]][["PValue"]][["Min"]]<-s2v( unlist(strsplit(unlist(strsplit(the_line,split = " "))[1],split = ":"))[2] )

        the_result[[num]][["PValue"]][["Max"]]<-unlist(strsplit(unlist(strsplit(the_line,split = " "))[2],split = ":"))[2]

        the_result[[num]][["PValue"]][["AVG"]]<-unlist(strsplit(unlist(strsplit(the_line,split = " "))[3],split = ":"))[2]

        the_result[[num]][["PValue"]][["pvalue"]]<-unlist(strsplit(unlist(strsplit(the_line,split = " "))[4],split = ":"))[2]

      }
    }

  }


  for(i in 1:length(the_result)){
    class(the_result[[i]])<-c("alml",class(the_result[[i]]))
  }

  class(the_result)<-c("alml_list",class(the_result))

  return(the_result)

}

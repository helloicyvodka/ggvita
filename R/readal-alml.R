#' Read in results from DELTA calculation
#'
#' @title readal.alml
#' @author Meng Yuan
#' @param file file address of alml result (with random tree alignment pvalue)
#' @return a list containing the result (phylos of treeS and treeT, the matching dataframe) and the result analysis (pValue and etc.)
#' @export



# import rlist
# import plyr
# import dplyr
# import ggtree


readal.alml<-function(file){

  the_result<-list()

  the_prefix<-c("Score","RootS","RootT","PruneS","PruneT","MatchS","MatchT","}","PValue","Min")

  the_text<-readLines(file)

  nl<-length(the_text)



  for(i in 1:(nl-1)){

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


      if(startsWith(the_line,"PValue")){

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

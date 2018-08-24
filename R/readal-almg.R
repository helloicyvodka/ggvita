


#' @export

readal.almg <- function(file){

  # file <- result.list.file[1]


  the_result<-list()

  the_prefix<-c("Score","RootS","RootT","PruneS","PruneT","MatchS","MatchT","}","PValue","Min")

  the_text<-readLines(file)

  nl<-length(the_text)


  for(i in 1:(nl)){

    the_line<-the_text[[i]]


    for(i2 in 1:(length(the_prefix)-3)){

      if(startsWith(the_line,the_prefix[i2])){

        the_result[[as.character(the_prefix[i2])]]<-s2v(unlist(strsplit(the_line,split = ":"))[2])

      }
    }


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



  the_result


}


# Includes:
# readal.alm, readal.alml, readal.epic



#-----------------------------------------------------------------------------------------------------



#' Read in an alm file
#'
#' Read in an alm file and create a list accordingly
#'
#' An alm file contains the information of teriminal tips and their tissue types.
#'
#' @param fileS the address of the treeS alm file
#' @param fileT the address of the treeT alm file
#' @return NULL, but create a variable named "alm_label" automately
#' @keywords readal.alm
#' @import magrittr
#' @export


readal.alm<-function(fileS,fileT){

  label_S<-read.table(fileS,header = T,colClasses = "character")

  label_T<-read.table(fileT,header = T,colClasses = "character")


  #


  label_list_S<-list()

  label_S$Lineage<-label_S$Lineage %>% as.character()

  for(i in 1:nrow(label_S)){
    label_list_S[[label_S[i,1]]]<-label_S[i,3]
  }

  #

  label_list_T<-list()

  label_T$Lineage<-label_T$Lineage %>% as.character()

  for(i in 1:nrow(label_T)){
    label_list_T[[label_T[i,1]]]<-label_T[i,3]
  }

  label_list<-list(treeS=label_list_S,treeT=label_list_T)

  class(label_list)<-c("alm_list",class(label_list))

  alm_label<<-label_list

  cat("The variable alm_label is created.")

}








#-----------------------------------------------------------------------------------------------------










#' Read local alignment results from HSA calculation and transform the result from txt to list in R
#'
#' @title read_alml
#' @author Meng Yuan
#' @param file file address
#' @return a list containing the result (phylos of treeS and treeT, the matching dataframe) and the result analysis (pValue and etc.)
#' @export
#' @import rlist
#' @import plyr
#' @import dplyr
#' @import ggtree



readal.alml<-function(file){

  if(!exists("alm_label")){
    stop("Please run readal.alm firstly and create the alm_label varaiable! ATTENTION: readal.alm will creates alm_label variable automately.")
  }


  the_result<-list()

  the_prefix<-c("Score","RootS","RootT","PruneS","PruneT","MatchS","MatchT","}","PValue","Min")

  the_text<-as.list(readLines(file))

  nl<-length(the_text)



  for(i in 1:nl){

    the_line<-the_text[[i]]

    if(regexpr("^[0-9]",the_line)){

      num2<-as.integer(regmatches(the_line,regexpr("^([0-9]+)",the_line)))

      if(length(num2)>0){

        num<-as.character(regmatches(the_line,regexpr("^([0-9]+)",the_line)))

        the_result[[num]]<-list()

        the_result[[num]]<-list("score_order"=as.numeric(num))
      }
    }


    for(i2 in 1:(length(the_prefix)-3)){

      if(startsWith(the_line,the_prefix[i2])){

        the_result[[num]][[as.character(the_prefix[i2])]]<-unlist(strsplit(the_line,split = ":"))[2]

        }

      }

    if(startsWith(the_line,"PValue")){

      the_result[["PValue"]][["all"]]<-unlist(strsplit(the_line,split = ":"))[2]

      }

    if(startsWith(the_line,"Min")){

      the_result[["PValue"]][["Min"]]<-unlist(strsplit(unlist(strsplit(the_line,split = " "))[1],split = ":"))[2]

      the_result[["PValue"]][["Max"]]<-unlist(strsplit(unlist(strsplit(the_line,split = " "))[2],split = ":"))[2]

      the_result[["PValue"]][["AVG"]]<-unlist(strsplit(unlist(strsplit(the_line,split = " "))[3],split = ":"))[2]

      }

  }



  # result_list<-lapply(the_result[1:(length(the_result)-1)],
  #                     function(x){
  #                       cat("=");
  #                       return(alml_2_phylo(x))})


  result_analysis<-strsplit(the_result[[length(the_result)]],split = " ")

  the_pValue<-strsplit(result_analysis$all,split=" ") %>% unlist()

  final_result<-list.update(the_result[-length(the_result)],PValue=as.numeric(the_pValue[.i]))

  for(i in 1:length(final_result)){
    class(final_result[[i]])<-c("alml",class(final_result[[i]]))
  }

  class(final_result)<-c("alml_list",class(final_result))

  return(final_result)

  }






#-----------------------------------------------------------------------------------------------------

#' Read in a gene expression level file on the tree downloaded from EPIC
#'
#' @import magrittr
#' @import data.table
#' @param expr_file a file downloaded from EPIC dataset, usually ending with .csv
#' @return a data.frame contains 4 columns: cell,time,blot,Lineage
#'

readal.epic<-function(expr_file){

  ## read in the expression file and pick up the colmuns: cell, time, blot


  epic_gene_expr <- fread(expr_file)

  if(!all(c("cell","time","blot") %in% colnames(epic_gene_expr))){
    stop("col_names are not in colnames(the_gene_exprfile)!")
  }


  epic_gene_expr$Lineage<-epic_gene_expr$cell %>% LN_to_Bin(.)

  col_names<-c("cell","time","blot","Lineage")

  epic_gene_expr_simple<-epic_gene_expr[,list(cell,time,blot,Lineage)]


  return(epic_gene_expr_simple)

}


#-----------------------------------------------------------------------------------------------------












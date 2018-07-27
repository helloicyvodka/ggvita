Na.omit.expr.dt <- function(com.expr){


  repeat{

    com.expr.tips <- com.expr %>% filter(Lineage %in% (Lineage %>% Find.tips())) %>% na.omit() %>% `$`("Lineage")

    com.expr.tips.na <- setdiff(com.expr %>% filter(Lineage %in% (Lineage %>% Find.tips())) %>% `$`("Lineage"),
                                com.expr.tips)

    if(com.expr.tips.na %>% length()!=0){

      com.expr <- com.expr  %>% filter(!Lineage %in% com.expr.tips.na)

    }else{break}

  }


  if(com.expr$Lineage %>% Find.addNode() %>% length()!=0)stop("Error: Tree is not complete!")

  return(com.expr)

}





















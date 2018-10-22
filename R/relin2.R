# write by Jianrong Yang
#
# relin2<- function(allLin) {
#   if(length(allLin) == 1) {
#     return(c("z"));
#   }
#   maxDepth <- allLin %>% nchar %>% max;
#   curPrefix <- c("");
#   ret <- data.frame(ori=c(),new=c());
#   for(dp in c(1:maxDepth)) {
#     if(length(curPrefix) == 0) {break;}
#     curPrefix <- paste(curPrefix,c("0","1"),sep="");
#     myDescendant <- sapply(curPrefix,function(x){
#       myPattern <- paste("^",x,sep="");
#       grep(myPattern,allLin,perl=T,value=T)
#     });
#     reachTerminal <- names(which(sapply(myDescendant,length) == 1));
#     ret <- rbind(ret,data.frame(ori=unlist(myDescendant[reachTerminal]),
#                                 new=reachTerminal));
#     curPrefix <- setdiff(curPrefix,reachTerminal)
#   }
#   return(ret)
# }


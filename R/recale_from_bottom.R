
recale_from_bottom<-function(dtS,scale_width=1){

  dtS[dtS$isTip=="TRUE",]$y<-rank(dtS[dtS$isTip=="TRUE",]$y)*scale_width

  for(i in max(dtS$x,na.rm = T):0){
    for(i2 in 1:nrow(dtS)){
      if(is.na(dtS[i2,]$x)){
        next
      }else if(dtS[i2,]$x==i){
        if(dtS[i2,]$isTip=="FALSE"){
          dtS[i2,]$y<-mean(dtS[dtS$parent==dtS[i2,]$node,]$y,na.rm = T)
        }
      }
    }
  }

  dtS
}

#' Add EPIC data to local tree alignment result
#'
#' @title stat_EPIC
#' @param ggvita.object the ggvita object you want to add to
#' @param expr_file the address of the gene expressing file from EPIC dataset
#' @param ... same with geom_segment parameters
#' @return  tree alignment result and corresponding gene expression. Phenotype and Genotype.
#' @export
#'


stat_epic <- function(ggvita.object,expr_file,...){

  df.S <- cal_EPIC(ggvita.object$plot$ggS$data,expr_file)
  df.T <- cal_EPIC(ggvita.object$plot$ggT$data,expr_file)

  g.S  <- geom_segment(data = df.S,
                       aes(
                         x = x,
                         y = y,
                         xend = xend,
                         yend = yend,
                         color = scaled.blot
                       ),...)


  g.T  <- geom_segment(data = df.T,
                       mapping=
                         aes(
                           x = x,
                           y = y,
                           xend = xend,
                           yend = yend,
                           color = scaled.blot
                         ),...)

  attr(g.S,"SorT") <- "S"
  attr(g.T,"SorT") <- "T"

  g <-list(layer.S=g.S,layer.T=g.T)

  class(g) <- c("ggvita_layers")

  return(g)
}






















cal_EPIC<-function(data,expr_file){



  #epic.dt <- readal.epic(expr_file = "~/2017-2018/Predict_expr/data/epic/epic_data/CD20061215_pha4I2L_11.csv",)

  epic.df <- readal.epic(expr_file)


  tr.prune <- attr(data,"prune")

  tr.prune.sister <- c()

  if(length(na.omit(attr(data,"prune")))!=0){


    tr.prune.sister <- as.character(sapply(tr.prune, function(x){

      x.parent <- get_parent(x)

      x.last <- substr(x, nchar(x) , nchar(x) )

      l <- list("1"="0", "0"="1")

      x.sister <- paste0(x.parent, l[[x.last]] )

      as.character(x.sister)

    }))





    pr.df <-data.frame(pr.sister=tr.prune.sister)

    pr.df$final.parent <- sapply(tr.prune.sister,function(x){


      if(!(x %in% tr.root)){

        repeat{

          x <- substr(x, 1, (nchar(x) - 1))


          if (x == "") {
            x <- "Root"
            break
          }

          if(x %in% data$node.seq){
            break
          }

          if(nchar(x)==1){
            break
          }

        }

      }


      x

    })




  }



  epic.df.tr <- epic.df[epic.df$node.seq %in% na.omit(c(data$node.seq,tr.prune.sister)) ,c("cell","time","blot","node.seq" )]

  epic.df.tr$node.seq <- sapply(epic.df.tr$node.seq, function(x){

    if(exists("pr.df")==T){

      if(x %in% pr.df$pr.sister){

        y <- pr.df[pr.df$pr.sister==x,"final.parent"]

      }else{
        y <- x
      }

    }else{

       y <- x
    }

    y

  })

  epic.df.tr <- merge(epic.df.tr,data,by="node.seq")


  time.freq <- data.frame(table(epic.df.tr$node.seq))
  colnames(time.freq) <- c("node.seq","time.freq")



  epic.df.tr$time.rank <- sapply(1:nrow(epic.df.tr),function(i){

    r <- epic.df.tr[i,]

    same.branch.times <- epic.df.tr[epic.df.tr$node.seq == r$node.seq,]$time

    r.time.rank <- rank(same.branch.times)[same.branch.times==r$time]

    return(r.time.rank)

  })



  epic.df.tr$node.x <- epic.df.tr$x


  epic.df.tr$ parent.x  <- sapply(1:nrow(epic.df.tr),function(i){

    r <- epic.df.tr[i,]

    parent.x <- unique( unlist(epic.df.tr[epic.df.tr$node.seq==r$parent.seq,"node.x"]))

    parent.x

  })




  epic.df.tr$x <- sapply(1:nrow(epic.df.tr),function(i){

    r <- epic.df.tr[i,]

    time.freq.r <- time.freq[time.freq$node.seq==r$node.seq,"time.freq"]

    seg_x_start <- ((r$time.rank-1)/time.freq.r) * (as.numeric(r$node.x)-as.numeric(r$parent.x))+as.numeric(r$parent.x)


    return(seg_x_start)

  })




  epic.df.tr$xend <- sapply(1:nrow(epic.df.tr),function(i){

    r <- epic.df.tr[i,]

    time.freq.r <- as.numeric(time.freq[time.freq$node.seq==r$node.seq,"time.freq"])

    seg_x_end <- (r$time.rank/time.freq.r) * (as.numeric(r$node.x)-as.numeric(r$parent.x))+as.numeric(r$parent.x)

    return(seg_x_end)

  })



  epic.df.tr$yend <- epic.df.tr$y

  epic.df.tr$scaled.blot <- epic.df.tr$blot

  epic.df.tr$scaled.blot[epic.df.tr$scaled.blot < 0] <-0

  pre.blot <- epic.df.tr$scaled.blot

  epic.df.tr$scaled.blot <- sapply(epic.df.tr$scaled.blot,function(x){

    (x-min(pre.blot))/(max(pre.blot)-min(pre.blot))


  })


  return(epic.df.tr)

 }


































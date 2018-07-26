
#' Add layer to ggvita
#'
#' The key function which helps ggvita work together with ggtree and ggplot2 system. OO system. Layer adding style.
#'
#' @param e1 element one, a ggvita object
#' @param e2 element two, any layer in ggvita, ggtree, ggplot, including "geom_?", "stat_?", "scale_?","theme_?"
#' @return a ggvita object
#' @export




#' @export


ggvita_add <- function(e1, e2) { UseMethod("ggvita_add",e2)}

ggvita_add.gg <- function(e1,e2){

  e1$plot$ggS <- e1$plot$ggS + e2

  e1$plot$ggT <- e1$plot$ggT + e2

  class(e1) <- "ggvita"

  e1


}

ggvita_add.ggvita <- function(e1,e2){

  e1$plot$ggS <- e1$plot$ggS + e2$layer.S

  e1$plot$ggT <- e1$plot$ggT + e2$layer.T

  class(e1) <- "ggvita"

  e1


}



#'@export

"%++%" <- ggvita_add









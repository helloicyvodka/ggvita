
#' Add layer to ggvita
#'
#' The key function which helps ggvita work together with ggtree and ggplot2 system. OO system. Layer adding style.
#'
#' @param e1 element one, a ggvita object
#' @param e2 element two, any layer in ggvita, ggtree, ggplot, including "geom_*", "stat_*", "scale_*",....but not "theme_*" ("%+ggvita%" instead)
#' @return a ggvita object
#' @example
#' ggvita(one_alml_result)+geom_abline(...)+scale_color_gradient(...)
#' @export

# "+.gg" <- function(e1, e2) {
#   # Get the name of what was passed in as e2, and pass along so that it
#   # can be displayed in error messages
#   e2name <- deparse(substitute(e2))
#
#   #if( "ggvita" %in% class(e1)) ggvita_add(e1,e2,e2name)
#   if (is.theme(e1))  add_theme(e1, e2, e2name)
#   else if (is.ggplot(e1)) add_ggplot(e1, e2, e2name)
#   else if (is.ggproto(e1)) {
#     stop("Cannot add ggproto objects together.",
#          " Did you forget to add this object to a ggplot object?",
#          call. = FALSE)
#   }
# }


#' @export



"+.ggvita"<-function(e1,e2){

  e2name <- deparse(substitute(e2))

  ggvita_add(e1,e2,e2name)

}

#' Add theme to ggvita object
#' @import plyr
#' @import dplyr
#' @import ggtree
#' @import ggplot2
#' @import rlist
#' @import data.table
#' @import magrittr
#' @import pipeR
#'
#'
#' @export
#' @example
#'
#' ggvita(...) %+ggvita% theme_classic()
#'
#'
#'
#'


#' @import ggplot2
#' @import ggtree

ggvita_add<-function(e1,e2,e2name)UseMethod("ggvita_add",e2)


#' @import ggplot2
#' @import ggtree

ggvita_add.default<- function(e1,e2,e2name) {

  stop("Don't know how to add !")
}

#' @import ggplot2
#' @import ggtree

ggvita_add.Layer<- function(e1,e2,e2name) {
  e1$plot$ggS<- e1$plot$ggS+e2
  e1$plot$ggT<- e1$plot$ggT+e2
  e1
}

ggvita_add.list<- function(e1,e2,e2name) {
  e1$plot$ggS<- e1$plot$ggS+e2
  e1$plot$ggT<- e1$plot$ggT+e2
  e1
}

ggvita_add.Scale<- function(e1,e2,e2name) {
  e1$plot$ggS<- e1$plot$ggS+e2
  e1$plot$ggT<- e1$plot$ggT+e2
  e1
}

ggvita_add.theme<- function(e1,e2,e2name) {
  e1$plot$ggS<- e1$plot$ggS+e2
  e1$plot$ggT<- e1$plot$ggT+e2
  e1
}
#' @import ggplot2
#' @import ggtree


ggvita_add.ggvita_layers <- function(e1,e2,e2name) {
 e1$plot$ggS<-e1$plot$ggS+e2$layer.S
 e1$plot$ggT<-e1$plot$ggS+e2$layer.T
 e1
}


ggvita_add.ggvita_themes <- function(e1,e2,e2name) {
  e1$plot$ggS<-e1$plot$ggS+e2$theme.S
  e1$plot$ggT<-e1$plot$ggS+e2$theme.T
  e1
}

ggvita_add.ggvita_scales <- function(e1,e2,e2name) {
  e1$plot$ggS<-e1$plot$ggS+e2$scale.S
  e1$plot$ggT<-e1$plot$ggS+e2$scale.T
  e1
}

"%+ggvita%" <-`+.ggvita`








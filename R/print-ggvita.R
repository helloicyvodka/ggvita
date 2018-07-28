
#'@export

print.ggvita <- function(i){

  p <-
    cowplot::plot_grid(
      i$plot$ggS+ggplot2::coord_flip()+ggplot2::scale_x_reverse(),
      i$plot$ggT+ggplot2::coord_flip(),
      ncol=1)

  pp <- cowplot::plot_grid(p,i$plot$Legend,nrow=1,rel_widths = c(10,1))
  print(pp)

}

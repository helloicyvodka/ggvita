
#'@export

print.ggvita <- function(i){

  p<-cowplot::ggdraw()+
     cowplot::draw_plot(i$plot$ggS,0,0,0.5,1)+
     cowplot::draw_plot(i$plot$ggT+ggplot2::scale_x_reverse(),0.5,0,0.5,1)
  print(p)

}

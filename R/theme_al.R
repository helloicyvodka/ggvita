#' A theme to add legend on both sides
#' @export
theme_al <- function(){

  l <- list(theme.S=theme(legend.position = "left"),
            theme.T=theme(legend.position = "right"))
  class(l)<-"ggvita"
  return(l)
}

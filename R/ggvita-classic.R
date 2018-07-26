#' Show alignment results in classical way: including expression and prune nodes
#'
#' @param alml_list the variable created from readal
#' @param result.order the order of result you want to show
#' @param colors color gradinets. See scale_gradientn's color requirement
#' @param values the values for data distrbution adjustment.See scale_gradientn's values requirement
#' @param size size of prune nodes and expression band width
#' @param tip_size size of tips
#' @param mc.cores This function is very slow. Suggest to use a few cores.
#' @param ... same as ggtree parameters (ggtree::ggtree())
#' @return a basic tree alignment structure
#' @export



ggvita.classic <- function(alml_list,
                           result.order,
                           expr_file,
                           size,
                           tip_size,
                           values=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)/0.8,
                           colors=rainbow(11)[1:9],
                           mc.cores =1,
                           ...){


  p <- ggvita(alml_list,result.order,...)

  g <- stat_epic(p,expr_file = expr_file,size=size,mc.cores = mc.cores)

  p <- p  %++% g %++% geom_tippoint(aes(fill=I(tip.fill)),size=tip_size,shape=21,color="NA")

  p <- p  %++% scale_color_gradientn(colors =colors,values = values )

  p <- p  %++% stat_prune(p,size=size,color="blue")

  return(p)

}

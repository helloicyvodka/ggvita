#' Draw score matrix
#' @param alml_list result read from readal
#' @param n rainbow max num for scale. heatmap color style.
#' @export

DrawScoreMatrix <- function(alml_list,n=48){

  score.matrix  <- as.matrix(alml_list$Info$ScoreMatrix)

  score.matrix <- as.matrix(score.matrix)
  score.vector <- as.vector(score.matrix)
  breaks <- unique(quantile(score.vector, probs = seq(0,1,0.01)))
  gplots::heatmap.2(score.matrix ,
            dendrogram = "none",
            labRow = FALSE,
            labCol = FALSE,
            Rowv=FALSE,
            Colv=FALSE,
            trace='none',
            col = rev(rainbow(n)[1:(length(breaks)-1)]),
            key = FALSE,
            keysize = 0.01,
            margins = c(2,2),
            breaks =breaks,
            xlab = "TreeT",
            ylab = "TreeS")

}

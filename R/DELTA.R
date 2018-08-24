
#' Calculate DELTA score
#'@param treeS: TreeS file path.(required)
#'@param treeT: TreeT file path.(required)
#'@param cost: Cost file path. the cost file contains the score for different types of leaves.(required)
#'@param method: l or g. g for global alignment; l for local alignment. default: g.
#'@param max_target: target num for l, local alignment. default: 1
#'@param test: testNum to calculate p-value. If testNum <=2, do not output p-value. default 0
#'@param outfile: output file path. default: TreeS file path + l or g, based on -method
#'@param all: T or F. output as much information as possible. default F;
#'@param prune: pruneScore is the punish for pruning one leaf. default 1
#'@export


DELTA <- function(treeS,
                  treeT,
                  cost,
                  outfile="./",
                  method="g",
                  max_target=1,
                  test=0,
                  all="F",
                  prune=1,
                  DELTA.address){

  system(paste0(DELTA.address,
                " ",
                "-treeS",
                " ",
                treeS,
                " ",
                "-treeT",
                " ",
                treeT,
                " ",
                "-cost",
                " ",
                cost,
                " ",
                "-max_target",
                " ",
                max_target,
                " ",
                "-method",
                " ",
                method,
                " ",
                "-outfile",
                " ",
                outfile,
                " ",
                "-test",
                " ",
                test,
                " ",
                "-all",
                " ",
                all,
                " ",
                "-prune",
                " ",
                prune
  ),
  intern = F)

}

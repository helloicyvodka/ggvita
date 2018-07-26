
# ggvita (visualization of tree alignments)

#### Sun Yat-sen University, Evolution and Genomics Function Lab, Chen&Yang Lab
#### Authors : Jianrong Philip Yang, Meng Yuan, Xiaolong Cao
#### Date : 2017-10-10

ggvita,an R package, is developed for visualization of tree alignment results based on ggtree and ggplot2 packages. Pro. Jianrong Philip Yang designed an algorithm to calculate the similarity between trees. This algorithm can dectect tree similar phenotypes at the sub tree scale.These results connect genotypes with phenotypes during a binary-tree development, from single cell ( usually zygote ) to multiple cells.  When we connect genotypes with phenotypes of livings, we can observe some interesting phenomena and find out the biology mechanisms behind them. geom_EPIC function can add expression levels of genes from EPIC (http://epic.gs.washington.edu/) on the branches of trees alignment results.


## Installation


### Method 1 (User Method):

```
install.packages("devtools")  
install_github("helloicyvodka/ggvita")  
library(ggvita)  
```

### Method 2 (Developer Method):

Linux shell:
```
$ git clone "http://...."  
```
R console:
```
load_all(the_file_cloned_to)  
```

If there is any problem, welcome to email me.

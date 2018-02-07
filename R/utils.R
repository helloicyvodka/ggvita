#' Transform a decimal number to a binary sequence of character
#'
#' @param x a decimal number
#' @return a binary sequence of character
#'
Dec2Bin <- function(x) {
  i <- 0
  string <- numeric(32)
  while(x > 0) {
    string[32 - i] <- x %% 2
    x <- x %/% 2
    i <- i + 1
  }
  first <- match(1, string)
  paste(as.character(string[first:32]),collapse="")
}







#' Transform binary sequence to decimal sequence
#'
#' @param seq a binary sequence of character class
#' @return  a decimal number of numeric class
#' @examples
#' Bin2Dec("10")
#' Bin2Dec("1000")
Bin2Dec<- function(seq){
  sum(2^(which(rev(unlist(strsplit(as.character(seq), "")) == 1))-1))
}








#' Sort the vector of binary sequences increasingly according to their decimal values
#'
#' @param seq_vector a vector containing some binary sequences
#' @return the vector of binary sequences in increasing order

sort_Bin_by_Dec<-function(seq_vector){
  Bin2Dec <- function(x){
    if(x=="Root"){
      dec<-1
    }else{
      x<-paste0("1",x)
      dec<-sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))
    }
    dec
  }
  seq_vector[order(unlist(lapply(seq_vector,Bin2Dec)))]
}





get_parent <- function(x) {
  x <- substr(x, 1, (nchar(x) - 1))
  if (x == "") {
    x <- "Root"
  }
  x
}




#' Get the binary sequence of parent
#'
#' We can use the binary index to find the index of its parent.
#'
#' @param seq a binary sequence of the daughter
#' @return a binary sequence of the mother/parent

Bin_get_parent<-get_parent


nb.branches <- function(the_seq, x) {

  if(x=="Root"){
    x<-""
  }

  x_0 <- paste0(x, "0")
  x_1 <- paste0(x, "1")
  i <- 0

  if (x_0 %in% the_seq) {
    i <- i + 1
  }
  if (x_1 %in% the_seq) {
    i <- i + 1
  }

  i
}


get_outside_son <- function(the_seq) {
  unique(unlist(lapply(the_seq, function(x) {
    if (nb.branches(the_seq, x) == 1) {
      x_0 <- paste0(x, "0")
      x_1 <- paste0(x, "1")
      return(setdiff(c(x_0, x_1), the_seq))
    }
  })))
}

get_outside_parent <- function(the_seq) {
  the_seq<-the_seq[-1]
  min_level <- min(nchar(the_seq))
  y <- unlist(lapply(the_seq, function(x) {

    if (nchar(x) > min_level & x != "Root" ) {
      if (T) {
        if (!(substr(x, 1, (nchar(x) - 1)) %in% the_seq)) {
          return(substr(x, 1, (nchar(x) - 1)))
        }
      }
    }
  }))
  unique(y)
}


s2v <- function(x) {
  unlist(strsplit(x, split = " "))
}

`%+%` <- function(a, b) paste0(a, b)



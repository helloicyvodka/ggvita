#' Transform Lineage Name to Lineage
#'
#' Transform C.elegans cell Lineage Name (e.g. "MSapapap") to Lineage ("10010101") (binary sequence, index)
#' @author Meng Yuan
#' @param the_LN_vec C.elegans cell Lineage Name (e.g. "MSapapap")
#' @param mc.cores This function is very slow. Suggest to use a few cores.
#' @return Lineage(binary sequence, index)
#' @importFrom magrittr %>%
#' @export






LN_to_Bin<- function(the_LN_vec,mc.cores=1,Root.style="Root") {


  r <-
    parallel::mclapply(the_LN_vec,
                          function(x)the_LN_2_trueLN(x,Root.style=Root.style),
                          mc.cores = mc.cores) %>%
    as.character()

  return(r)


}







#' @importFrom magrittr %>%

the_LN_2_trueLN <- function(x,Root.style="Root") {

  the_prefix <- c(
    "AB",
    "Ab",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G1",
    "G2",
    "H1L",
    "H1R",
    "H2L",
    "H2R",
    "K",
    "M",
    "MS",
    "P0",
    "P1",
    "P10",
    "P11",
    "P12",
    "P2",
    "P3",
    "P4",
    "P5",
    "P6",
    "P7",
    "P8",
    "P9",
    "QL",
    "QR",
    "TL",
    "TR",
    "U",
    "V1L",
    "V1R",
    "V2L",
    "V2R",
    "V3L",
    "V3R",
    "V4L",
    "V4R",
    "V5L",
    "V5R",
    "V6L",
    "V6R",
    "W",
    "Y",
    "Z",
    "Z1",
    "Z4",
    "Z2",
    "Z3",
    "EMS"
  )

  #########

  the_true_prefix <- c(
    "Za", #"AB"
    "Za",
    "Zaprppppapa",
    "Zppa",
    "Zpppa",
    "Zpap",
    "Zaplppppapp",
    "Zaprpaaaapa",
    "Zaplapaapa",
    "Zaplaaappp",
    "Zaarpapppp",
    "Zaarppaaap",
    "Zaarpppaap",
    "Zaplpapppaa",
    "Zpaaapaapp",
    "Zpaa",
    "Z",
    "Zp",#P1(CXL:"Zaplapaapp"->YM:"Zp")
    "Zaprapapap", #P10
    "Zaplapappa", #P11
    "Zaprapappa", #P12
    "Zpp",#P2(CXL:"Zaprapaapp"->YM:"Zpp")
    "Zppp", #P3(CXL: "Zaplappaaa"->YM:"Zppp")
    "Zpppp",#P4(CXL:"Zaprappaaa"->YM:"Zpppp")
    "Zaplappaap",
    "Zaprappaap",
    "Zaplappapp",
    "Zaprappapp",
    "Zaplapapap",
    "Zaplapapaaa",
    "Zaprapapaaa",
    "Zaplappppp",
    "Zaprappppp",
    "Zaplppppapa",
    "Zaarppapaa",
    "Zaarppppaa",
    "Zaarppapap",
    "Zaarppppap",
    "Zaplappapa",
    "Zaprappapa",
    "Zaarppappa",
    "Zaarpppppa",
    "Zaplapapaap",
    "Zaprapapaap",
    "Zaarppappp",
    "Zaarpppppp",
    "Zaprapaapa",
    "Zaprpppaaaa",
    "Z",
    "Zpaapppaap",
    "Zpaaappaap",
    "Zppppp",
    "Zppppa",
    "Zpa"
  )



  the_prefix_list  <- the_true_prefix

  names(the_prefix_list) <- the_prefix

  the_matched_prefix <- the_prefix[startsWith(x, the_prefix)]

  the_matched_prefix <- the_matched_prefix[which(nchar(the_matched_prefix) == (the_matched_prefix %>% nchar() %>% max()))]

  if (length(the_matched_prefix) == 0) {
    stop("The prefix was not matched!")
  }



  #a -> 0; p -> 1
  #l -> 0; r -> 1
  #d -> 0; v -> 1

  x <- x %>%
    as.character() %>%
    gsub("\\.", "", .) %>%
    gsub(" ", "", .)


  the_matcher_prefix.2 <- the_prefix_list[the_matched_prefix] %>% as.character()


   x <-
    x %>%
    sub(
      the_matched_prefix,
      the_matcher_prefix.2 ,
      .
    )

  if(x!="Z"){

    x<-sub("Z", "", x) %>%
      gsub("a", "0", .) %>%
      gsub("l", "0", .) %>%
      gsub("d", "0", .) %>%
      gsub("p", "1", .) %>%
      gsub("r", "1", .) %>%
      gsub("v", "1", .)

  }else{
    x<- Root.style %>% as.character()
  }

  return(x)
}


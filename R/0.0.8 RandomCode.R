RandomCode <- function(n = 10,
                       head = format(Sys.time(), "%Y%m%d%H%M%S"),
                       tail = NULL){
  number <- c(as.character(0:9),LETTERS)
  code <- NULL
  
  i <- 1
  while(i <= n){
    code <- paste0(code,number[floor(runif(1,1,36.99))])
    i <- i+1
  }
  
  code <- paste0(head,code,tail)
  return(code)
}




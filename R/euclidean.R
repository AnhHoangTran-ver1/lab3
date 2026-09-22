#'
#' Euclidean algorithm
#' 
#' @param a A numeric value.
#' @param b A numeric value.
#'
#' @returns The greatest common divisor of a and b.
#'
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export 
euclidean <- function(a, b){
  if (!is.numeric(a) || !is.numeric(b)){
    stop("The arguments are not numeric")
  }
  while(b != 0){
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}

euclidean(123612, 13892347912)
euclidean(100, 1000)


#' @param a A numeric value.
#'v@param b A numeric value.
#'
#' @return The greatest common divisor of a and b.
#'
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export 
euclidean <- function(a, b){
  while(b != 0){
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}

euclidean(123612, 13892347912)
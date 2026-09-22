#<<<<<<< Updated upstream
#'
#' euclidean
#'
#' @param a A numeric value.
#' @param b A numeric value.
#' 
#' @description The best experience with the lab 3 
#'
#' 
#=======
#>>>>>>> Stashed changes
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
#<<<<<<< Updated upstream
  if (!is.numeric(a) || !is.numeric(b)){
    stop("The arguments are not numeric")
  }
#=======
#  stopifnot(is.numeric(a) && length(a) == 1 && is.numeric(b) && length(b) == 1)
#>>>>>>> Stashed changes
  while(b != 0){
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}

euclidean(123612, 13892347912)
euclidean(100, 1000)

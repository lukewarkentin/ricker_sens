#' Get SMSY from alpha and beta using [Lambert's equation](https://peerj.com/articles/1623/) 
#'
#' @param a Ricker alpha
#' @param b Ricker beta
#'
#' @return
#' @export
#'
#' @examples
SMSY_lambert <- function(a ,b )  { 
  SMSY <- (1-gsl::lambert_W0(exp(1-log(a))))/b 
  SMSY
}
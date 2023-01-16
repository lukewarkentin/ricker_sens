#' Get SMSY using [Lambert's equation](https://peerj.com/articles/1623/) 
#'
#' @param a 
#' @param b 
#'
#' @return
#' @export
#'
#' @examples
SMSY_lambert <- function(a ,b )  { 
  SMSY <- (1-gsl::lambert_W0(exp(1-log(a))))/b 
  SMSY
}
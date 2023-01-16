# Get SMSY and Sgen from alpha and beta
#' Get SMSY and Sgen from alpha and beta
#'
#' @param a 
#' @param b 
#' @param int_lower 
#' @param int_upper 
#'
#' @return
#' @export
#'
#' @examples
#' 
get_SMSY_Sgen <- function(a, b, int_lower=-1, int_upper=1/b*2) {
  SMSY <- log(a)*(0.5-0.07*log(a))/b # Hilborn and Walters equation to estimate SMSY
  fun_Sgen <- function(Sgen, a, b, SMSY) {Sgen* a * exp( - b*Sgen) - SMSY}
  Sgen <- uniroot(fun_Sgen, interval=c(int_lower, int_upper), a=a, b=b, SMSY=SMSY)$root
  est  <- data.frame(SMSY, Sgen)
  est
}


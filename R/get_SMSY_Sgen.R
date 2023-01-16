# Get SMSY and Sgen from alpha and beta
#' Get SMSY and Sgen from alpha and beta
#'
#' @param a Ricker alpha
#' @param b Ricker beta
#' @param int_lower  lower interval for uniroot function to get Sgen. Default to -1
#' @param int_upper  upper interval for unirtoor function to get Sgen. Default to twice the carrying capacity
#'
#' @return data frame with SMSY and Sgen
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


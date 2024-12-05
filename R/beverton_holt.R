#' Beverton Holt
#'
#' @param stock
#' @param alpha
#' @param beta
#'
#' @return bh
#' @export
#'
#' @examples
beverton_holt <- function(stock, alpha, beta) {

  bh <- (alpha * stock) / (beta + stock)

  return(bh)
}

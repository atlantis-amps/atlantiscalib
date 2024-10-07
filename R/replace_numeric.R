#' Function to replace numeric(0) with NA
#'
#' @param x
#'
#' @return x
#' @export
#'
#' @examples
replace_numeric <- function(x) {
  if(is.list(x)) {
    lapply(x, function(y) if(length(y) == 0) "" else y)
  } else {
    x
  }
}
